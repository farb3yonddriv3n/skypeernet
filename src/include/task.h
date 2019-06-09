#ifndef TASK_H_
#define TASK_H_

enum task_e {
    TASK_FILE_KEEP,
    TASK_FILE_DELETE,
};

struct task_s {
    unsigned int idx;
    unsigned int parts;
    struct {
        uint64_t      size;
        uint64_t      iter;
        unsigned char name[SHA256HEX];
        char          fullpath[256];
    } file;
    int            host;
    unsigned short port;
    enum task_e    action;
};

struct module_task_s {
    int (*add)(struct peer_s *p, const char *blockdir, unsigned char *filename,
               int nfilename, int host, unsigned short port,
               enum task_e action);
    int (*update)(struct peer_s *p);
    int (*clean)(void *t);
};

extern struct module_task_s task;

#endif
