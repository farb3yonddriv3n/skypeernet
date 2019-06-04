#ifndef JOB_H_
#define JOB_H_

struct job_s {
    unsigned char file[SHA256HEX];
    struct {
        char  *file[SHA256HEX];
        size_t size;
    } chunks;
};

struct module_job_s {
    int (*add)(struct group_s *remote,
               unsigned char *file, void **found);
};

extern const struct module_job_s job;

#endif
