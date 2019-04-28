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
                   struct root_diff_s *diff);
    int (*copy)(struct root_s *dst, const struct root_s *src);
    int (*validate)(const struct root_s *r, bool *valid);
    int (*clean)(struct root_s *r);
    struct {
        int (*add)(struct root_s *r, struct block_s *b);
        int (*append)(struct root_s *r, const json_object *blockobj);
        int (*size)(struct root_s *r, size_t *size);
        int (*export)(const struct root_s *r, const uint64_t blockidx,
                      json_object **block_obj);

    } blocks;
    struct {
        struct {
            int (*file)(struct root_s *r, const char *filename);
            int (*object)(struct root_s *r, const json_object *obj);
        } load;
        int (*save)(const struct root_s *r, json_object **robj);
    } data;
};

enum root_diff_e {
    ROOT_LOCAL,
    ROOT_REMOTE
};

struct root_diff_s {
    bool             verdict;
    enum root_diff_e winner;
    uint64_t         blockidx;
};

extern const struct module_root_s root;

#endif
