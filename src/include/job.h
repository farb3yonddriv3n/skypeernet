#ifndef JOB_H_
#define JOB_H_

enum job_counter_e {
    CHUNKCOUNTER_ADD,
    CHUNKCOUNTER_REM,
};

enum job_chunk_state_e {
    JOBCHUNK_NONE,
    JOBCHUNK_NOTFOUND,
    JOBCHUNK_RECEIVING,
    JOBCHUNK_DONE,
};

struct job_chunk_s {
    unsigned char          chunk[SHA256HEX];
    size_t                 size;
    enum job_chunk_state_e state;
    double                 updated;
    struct {
        int            host;
        unsigned short port;
    } net;
};

struct job_s {
    struct {
        unsigned char name[SHA256HEX];
        uint64_t      size;
    } file;
    struct {
        struct job_chunk_s *array;
        size_t              size;
    } chunks;
    struct {
        int none;
        int receiving;
        int notfound;
        int done;
    } counter;
};

struct module_job_s {
    int (*add)(struct list_s *jobs, struct group_s *remote,
               unsigned char *file, int nfile, bool *found,
               bool *added);
    int (*update)(struct list_s *jobs, const char *filename);
    void (*resume)(struct ev_loop *loop, struct ev_timer *timer, int revents);
    int (*show)(struct list_s *jobs);
};

extern const struct module_job_s job;

#endif
