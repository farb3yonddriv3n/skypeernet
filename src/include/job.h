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
    unsigned char pubkeyhash[SHA256HEX];
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
    int (*add)(struct config_s *cfg, struct list_s *jobs, struct group_s *remote,
               unsigned char *file, int nfile, bool *found,
               bool *added, bool *exists);
    int (*update)(struct peer_s *p, const char *downloaddir,
                  struct list_s *jobs, const char *filename);
    void (*resume)(struct ev_loop *loop, struct ev_timer *timer, int revents);
    int (*finalize)(struct config_s *cfg, struct group_s *remote, unsigned char *file,
                    int nfile, bool *finalized);
    int (*dump)(struct config_s *cfg, struct list_s *jobs, json_object **obj);
    int (*remove)(struct list_s *jobs, unsigned char *file,
                  int nfile, bool *removed);
    struct {
        int (*save)(struct distfs_s *dfs);
        int (*load)(struct distfs_s *dfs);
    } data;
};

extern const struct module_job_s job;

#endif
