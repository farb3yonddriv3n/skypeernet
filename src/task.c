#include <common.h>

#define MAX_TASK_BUFFER (512 * 1024) // 512kB

static int resume(struct peer_s *p)
{
    int cb(struct list_s *l, void *ut, void *ud)
    {
        struct task_s *t = (struct task_s *)ut;
        struct peer_s *p = (struct peer_s *)ud;
        if (!t || !p) return -1;
        char *buffer;
        size_t size;
        if (os.filepart(t->file.name, t->file.iter * MAX_TASK_BUFFER,
                        MAX_TASK_BUFFER, &buffer, &size) != 0) return -1;
        p->send_buffer.type = BUFFER_FILE;
        sn_setr(p->send_buffer.u.file.bin, buffer, size);
        int            host = t->host;
        unsigned short port = t->port;
        if ((++t->file.iter) * MAX_TASK_BUFFER >= t->file.size)
            if (list.del(l, t) != 0) return -1;
        if (payload.send(p, COMMAND_FILE,
                         host, port) != 0) return -1;
        if (buffer) free(buffer);
        return 1;
    }
    if (!p) return -1;
    if (list.map(&p->task, cb, p) != 0) return -1;
    return 0;
}

static int update(struct peer_s *p)
{
    if (!p) return -1;
    int send_sz, task_sz;
    if (list.size(&p->send.nbl, &send_sz) != 0) return -1;
    if (list.size(&p->task,     &task_sz) != 0) return -1;
    if (send_sz == 0 && task_sz != 0) resume(p);
    return 0;
}

static int clean(void *t)
{
    if (!t) return -1;
    free(t);
    return 0;
}

static int init(struct peer_s *p, const char *filename, int nfilename,
                int host, unsigned int port)
{
    if (!p || !filename) return -1;
    struct task_s *t = malloc(sizeof(*t));
    if (!t) return -1;
    memset(t, 0, sizeof(*t));
    if (nfilename > sizeof(t->file.name)) return -1;
    memcpy(t->file.name, filename, nfilename);
    t->host = host;
    t->port = port;
    if (list.add(&p->task, t, task.clean) != 0) return -1;

    if (os.filesize(t->file.name, &t->file.size) != 0) return -1;
    p->send_buffer.type = BUFFER_FILE_SEND;
    p->send_buffer.u.file_send.size = t->file.size;
    return payload.send(p, COMMAND_FILE_SEND,
                        host, port);
}

struct module_task_s task = {
    .init   = init,
    .update = update,
    .clean  = clean,
};
