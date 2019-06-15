#ifndef ROOT_H_
#define ROOT_H_

#define DISTFS_BASE_ROOT_HASH "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

struct root_s {
    unsigned char hash[SHA256HEX];
    struct {
        struct block_s **array;
        size_t           size;
    } blocks;
    struct {
        int            host;
        unsigned short port;
        unsigned char  filename[SHA256HEX];
    } net;
};

struct module_root_s {
    int (*init)(struct root_s **r);
    int (*compare)(struct root_s *local, struct root_s *remote,
                   struct root_diff_s *diff);
    int (*copy)(struct root_s **dst, const struct root_s *src);
    int (*validate)(const struct root_s *r, bool *valid);
    int (*canmerge)(struct root_s *dst, struct root_s *src,
                    struct root_diff_s *diff);
    int (*merge)(struct root_s *dst, struct root_s *src,
                 bool *merged);
    int (*find)(struct root_s *r, unsigned char *h, void **found,
                int *host, unsigned short *port);
    int (*dump)(struct root_s *r, struct config_s *cfg);
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
            int (*json)(struct root_s **r, char *content, int ncontent);
            int (*file)(struct root_s **r, const char *filename);
            int (*object)(struct root_s **r, const json_object *obj);
        } load;
        struct {
            int (*file)(const struct root_s *r, const char *filename);
            int (*object)(const struct root_s *r, json_object **robj);
        } save;
    } data;
    struct {
        int (*set)(struct root_s *r, int host, unsigned short port,
                   unsigned char *filename);
    } net;
};

enum root_merge_e {
    MERGE_NOT_NEEDED,
    MERGE_POSSIBLE,
    MERGE_IMPOSSIBLE
};

enum root_diff_e {
    ROOT_NONE,
    ROOT_DST,
    ROOT_SRC
};

struct root_diff_s {
    bool              equal;
    enum root_diff_e  winner;
    uint64_t          blockidx;
    struct {
        struct root_s *ptr;
        size_t         size;
    } src;
    struct {
        struct root_s *ptr;
        size_t         size;
    } dst;
    enum root_merge_e canmerge;
};

extern const struct module_root_s root;

#endif
