#ifndef JOB_H_
#define JOB_H_

enum job_chunk_e {
    JOBCHUNK_NONE,
    JOBCHUNK_STARTED,
    JOBCHUNK_RECEIVING,
    JOBCHUNK_STALLED,
    JOBCHUNK_DONE,
};

struct job_chunk_s {
    char             chunk[SHA256HEX];
    size_t           size;
    enum job_chunk_e state;
};

struct job_s {
    unsigned char file[SHA256HEX];
    struct {
        struct job_chunk_s *array;
        size_t              size;
    } chunks;
    struct {
        int none;
        int inprogress;
        int done;
    } counter;
};

struct module_job_s {
    int (*add)(struct list_s *jobs, struct group_s *remote,
               unsigned char *file, int nfile, bool *found,
               bool *added);
};

extern const struct module_job_s job;

#endif
