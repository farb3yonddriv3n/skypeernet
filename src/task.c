#include <common.h>

static int resume(struct peer_s *p)
{
    int cb(struct list_s *l, void *ut, void *ud)
    {
        struct task_s *t = (struct task_s *)ut;
        struct peer_s *p = (struct peer_s *)ud;
        if (!t || !p) return -1;
        char *buffer;
        uint64_t size;
        if (os.filepart(t->file.fullpath, t->file.iter * p->cfg.net.max.task_buffer,
                        p->cfg.net.max.task_buffer, &buffer, &size) != 0) return -1;
        p->send_buffer.type = BUFFER_FILE;
        sn_setr(p->send_buffer.u.file.bin, buffer, size);
        ifr(payload.send(p, COMMAND_FILE, t->host, t->port, t->idx,
                         t->parts, t->file.name, &t->tcp));
        if ((++t->file.iter) * p->cfg.net.max.task_buffer >= t->file.size)
            if (list.del(l, t) != 0) return -1;
        if (buffer) free(buffer);
        return 1;
    }
    if (!p) return -1;
    ifr(list.map(&p->tasks.list, cb, p));
    return 0;
}

static int update(struct peer_s *p)
{
    if (!p) return -1;
    int send_sz, task_sz;
    ifr(list.size(&p->send.nbl, &send_sz));
    ifr(list.size(&p->tasks.list, &task_sz));
    if (send_sz <= p->cfg.net.max.send_queue && task_sz != 0) return resume(p);
    return 0;
}

static int clean(void *ut)
{
    if (!ut) return -1;
    struct task_s *t = (struct task_s *)ut;
    if (t->action == TASK_FILE_DELETE) {
        ifr(remove(t->file.fullpath));
    }
    free(t);
    return 0;
}

static int find(struct list_s *tasks, const char *filename,
                int host, unsigned short port, struct task_s **found)
{
    if (!tasks || !filename || !found) return -1;
    *found = NULL;
    struct task_find_s { const char    *filename;
                         int            host;
                         unsigned short port;
                         struct task_s *found; };
    int cb(struct list_s *l, void *ut, void *ud) {
        struct task_s      *t  = (struct task_s *)ut;
        struct task_find_s *tf = (struct task_find_s *)ud;
        if (t->host == tf->host && t->port == tf->port &&
            dmemcmp(tf->filename, SHA256HEX, t->file.name, sizeof(t->file.name))) {
            tf->found = t;
            return 1;
        }
        return 0;
    }
    struct task_find_s tf = { .filename = filename,
                              .host     = host,
                              .port     = port,
                              .found    = NULL };
    ifr(list.map(tasks, cb, &tf));
    *found = tf.found;
    return 0;
}

static int add(struct peer_s *p, const char *blockdir, unsigned char *filename,
               int nfilename, int host, unsigned short port,
               unsigned char *parent, enum task_e action,
               struct tcp_s *tcp)
{
    if (!p || !filename) return -1;
    if (nfilename != SHA256HEX) return -1;
    struct task_s *exists;
    ifr(find(&p->tasks.list, (const char *)filename, host, port, &exists));
    if (exists) return 0;
    struct task_s *t = malloc(sizeof(*t));
    if (!t) return -1;
    memset(t, 0, sizeof(*t));
    if (nfilename != sizeof(t->file.name)) return -1;
    t->host   = host;
    t->port   = port;
    t->action = action;
    t->idx  = ++(p->tasks.idx);
    if (tcp) memcpy(&t->tcp, tcp, sizeof(t->tcp));
    if (parent) memcpy(t->file.parent, parent, SHA256HEX);
    memcpy(t->file.name, filename, nfilename);
    snprintf(t->file.fullpath, sizeof(t->file.fullpath), "%s/%.*s",
                                                         blockdir,
                                                         nfilename, filename);
    ifr(os.filesize(t->file.fullpath, &t->file.size));
    ifr(os.fileparts(t->file.fullpath, p->cfg.net.max.task_buffer, &t->parts));
    ifr(list.add(&p->tasks.list, t, task.clean));
    return task.update(p);
}

static int dump(struct peer_s *p, json_object **obj)
{
    int cb(struct list_s *l, void *ut, void *ud) {
        struct task_s *t     = (struct task_s *)ut;
        json_object   *tasks = (json_object *)ud;
        json_object *id = json_object_new_int(t->idx);
        json_object *size = json_object_new_int64(t->file.size);
        json_object *name = json_object_new_string_len((const char *)t->file.parent,
                                                       sizeof(t->file.parent));
        json_object *jt = json_object_new_object();
        json_object_object_add(jt, "idx",  id);
        json_object_object_add(jt, "size", size);
        json_object_object_add(jt, "name", name);
        json_object_array_add(tasks, jt);
        return 0;
    }
    if (!p) return -1;
    *obj = json_object_new_object();
    json_object *jtasks = json_object_new_array();
    json_object_object_add(*obj, "tasks", jtasks);
    return list.map(&p->tasks.list, cb, jtasks);
}

static int cancel(struct peer_s *p, unsigned int idx,
                  bool *cancelled)
{
    if (!p || !cancelled) return -1;
    *cancelled = false;
    struct task_id_s { unsigned int idx; struct task_s *found; };
    int find_by_id(struct list_s *l, void *ut, void *ud) {
        struct task_s    *t  = (struct task_s *)ut;
        struct task_id_s *ti = (struct task_id_s *)ud;
        if (t->idx == ti->idx) {
            ti->found = t;
            return 1;
        }
        return 0;
    }
    struct task_id_s ti = { .idx = idx, .found = NULL };
    ifr(list.map(&p->tasks.list, find_by_id, &ti));
    if (ti.found) {
        *cancelled = true;
        return list.del(&p->tasks.list, ti.found);
    }
    return 0;
}

struct module_task_s task = {
    .add    = add,
    .update = update,
    .cancel = cancel,
    .dump   = dump,
    .clean  = clean,
};
