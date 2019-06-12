#ifndef TRANSACTION_H_
#define TRANSACTION_H_

enum transaction_e {
    TFILE_ADD,
    TFILE_UPDATE,
    TFILE_VOUCH,
    TFILE_SEED,
    TFILE_ACCESS
};

struct transaction_s {
    int                version;
    unsigned int       timestamp;
    unsigned char      hash[SHA256HEX];
    enum transaction_e type;

    struct {
        unsigned char  prev[SHA256HEX];
        unsigned char  current[SHA256HEX];
    } blockhash;

    union {
        struct file_s        add;
        /*
        struct file_s        update;
        struct file_vouch_s  vouch;
        struct file_seed_s   seed;
        struct file_access_s access;
        */
    } action;
};

struct transaction_param_s {
    enum transaction_e type;
    union {
        struct {
            const char *name;
            const char *pathname;
        } add;
        struct {
            bool *valid;
        } validate;
        struct {
            json_object **obj;
        } save;
        struct {
            json_object *obj;
        } load;
        struct {
            unsigned char  *hash;
            void          **found;
        } find;
    } action;
};

struct module_transaction_s {
    int (*init)(struct transaction_s **t,
                struct transaction_param_s *param);
    int (*validate)(struct transaction_s *t, bool *valid);
    void (*metadump)(struct transaction_s *t);
    int (*dump)(struct transaction_s *t);
    unsigned char *(*hash)(struct transaction_s *t);
    int (*find)(struct transaction_s *t, unsigned char *h,
                void **found);
    int (*clean)(struct transaction_s *t);
    struct {
        int (*load)(struct transaction_s **t, json_object *tobj);
        int (*save)(struct transaction_s *t, json_object **tobj);
    } data;
};

struct transaction_sub_s {
    int (*init)(struct transaction_s *t,
                struct transaction_param_s *param,
                unsigned char *dst_hash);
    int (*validate)(struct transaction_s *t, unsigned char *dst_hash,
                    bool *valid);
    int (*dump)(struct transaction_s *t);
    int (*find)(struct transaction_s *t, unsigned char *file_hash,
                void **found);
    int (*clean)(struct transaction_s *t);
    struct {
        int (*load)(struct transaction_s *t, json_object *tobj);
        int (*save)(struct transaction_s *t, json_object **parent);
    } data;
};

extern const struct module_transaction_s transaction;
extern const struct transaction_sub_s transaction_file_add;

#endif
