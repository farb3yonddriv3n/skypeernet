#ifndef ROOT_H_
#define ROOT_H_

struct root_s {
    struct {
        struct block_s **array;
        size_t           size;
    } blocks;
};

struct module_root_s {
    int (*init)(struct root_s *r);
    int (*compare)(struct root_s *local, struct root_s *remote,
                   struct block_equal_s *equal);
    struct {
        int (*add)(struct root_s *r, struct block_s *b);
        int (*size)(struct root_s *r, size_t *size);
    } blocks;
    struct {
        int (*import)(struct root_s *r, const char *filename);
        int (*export)(struct root_s *r);
    } data;
};

enum root_equal_e {
    ROOT_BOTH_EQUAL,
    ROOT_LOCAL_DIFF,
    ROOT_REMOTE_DIFF,
};

struct root_compare_s {
    enum root_equal_e equal;
    uint64_t          blockidx;
};

extern const struct module_root_s root;

#endif
