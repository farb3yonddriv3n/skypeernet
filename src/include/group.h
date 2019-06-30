#ifndef GROUP_H_
#define GROUP_H_

struct group_s {
    struct {
        struct root_s **array;
        size_t          size;
    } roots;
};

struct module_group_s {
    int (*init)(struct group_s **g);
    int (*compare)(struct group_s *local, struct group_s *remote,
                   bool *equal);
    int (*receive)(struct group_s *g, struct transaction_s *t);
    int (*validate)(struct group_s *g, bool *valid);
    int (*dump)(struct group_s *g, struct config_s *cfg, json_object **obj);
    int (*export)(struct group_s *g, void *ud,
                  int (*cb)(json_object *e, void *ud));
    struct {
        int (*root)(struct group_s *g, unsigned char *h,
                    struct root_s **found);
        int (*transaction)(struct group_s *g,
                           unsigned char *h,
                           void **found,
                           void *data,
                           int (*cb)(unsigned char *pubkeyhash,
                                     void *data));
    } find;
    int (*clean)(struct group_s *g);
    struct {
        int (*add)(struct group_s *g, struct root_s *r);
    } roots;
    struct {
        int (*save)(struct group_s *g);
        int (*load)(struct group_s *g);
    } db;
};

const struct module_group_s group;

#endif
