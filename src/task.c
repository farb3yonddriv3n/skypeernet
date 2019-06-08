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
        char fpfilename[128];
        snprintf(fpfilename, sizeof(fpfilename), "%.*s", (int)sizeof(t->file.name),
                 t->file.name);
        if (os.filepart(fpfilename, t->file.iter * p->cfg.net.max.task_buffer,
                        p->cfg.net.max.task_buffer, &buffer, &size) != 0) return -1;
        p->send_buffer.type = BUFFER_FILE;
        sn_setr(p->send_buffer.u.file.bin, buffer, size);
        int            host  = t->host;
        unsigned short port  = t->port;
        unsigned int   tidx  = t->idx;
        unsigned int   parts = t->parts;
        unsigned char  filename[SHA256HEX];
        memcpy(filename, t->file.name, sizeof(t->file.name));
        if ((++t->file.iter) * p->cfg.net.max.task_buffer >= t->file.size)
            if (list.del(l, t) != 0) return -1;
        if (payload.send(p, COMMAND_FILE, host, port, tidx,
                         parts, filename) != 0) return -1;
        if (buffer) free(buffer);
        return 1;
    }
    if (!p) return -1;
    if (list.map(&p->tasks.list, cb, p) != 0) return -1;
    return 0;
}

static int update(struct peer_s *p)
{
    if (!p) return -1;
    int send_sz, task_sz;
    if (list.size(&p->send.nbl,  &send_sz) != 0) return -1;
    if (list.size(&p->tasks.list, &task_sz) != 0) return -1;
    if (send_sz <= p->cfg.net.max.send_queue && task_sz != 0) return resume(p);
    return 0;
}

static int clean(void *t)
{
    if (!t) return -1;
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

static int add(struct peer_s *p, const char *filename, int nfilename,
               int host, unsigned short port)
{
    if (!p || !filename) return -1;
    if (nfilename != SHA256HEX) return -1;
    struct task_s *exists;
    ifr(find(&p->tasks.list, filename, host, port, &exists));
    if (exists) return 0;
    struct task_s *t = malloc(sizeof(*t));
    if (!t) return -1;
    memset(t, 0, sizeof(*t));
    if (nfilename != sizeof(t->file.name)) return -1;
    memcpy(t->file.name, filename, nfilename);
    t->host = host;
    t->port = port;
    t->idx  = ++(p->tasks.idx);
    ifr(os.filesize(filename, &t->file.size));
    ifr(os.fileparts(filename, p->cfg.net.max.task_buffer, &t->parts));
    ifr(list.add(&p->tasks.list, t, task.clean));
    return task.update(p);
}

struct module_task_s task = {
    .add    = add,
    .update = update,
    .clean  = clean,
};
