#ifndef TASK_H_
#define TASK_H_

struct task_s {
    unsigned int idx;
    unsigned int parts;
    struct {
        uint64_t      size;
        uint64_t      iter;
        unsigned char name[SHA256HEX];
    } file;
    int            host;
    unsigned short port;
};

struct module_task_s {
    int (*add)(struct peer_s *p, const char *filename, int nfilename,
               int host, unsigned short port);
    int (*update)(struct peer_s *p);
    int (*clean)(void *t);
};

extern struct module_task_s task;

#endif
