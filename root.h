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
    int (*add)(struct root_s *r, struct block_s *b);
    struct {
        int (*export)(struct root_s *r);
    } data;
};

extern const struct module_root_s root;

#endif
