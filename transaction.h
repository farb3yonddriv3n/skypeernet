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
            sn name;
        } add;
        struct {
            bool *valid;
        } validate;
    } action;
};

struct transaction_mod_s {
    int (*init)(struct transaction_s *t,
                struct transaction_param_s *param);
    int (*validate)(struct transaction_s *t, bool *valid);
    void (*metadump)(struct transaction_s *t);
    int (*dump)(struct transaction_s *t);
};

struct transaction_sub_s {
    int (*init)(struct transaction_s *t,
                struct transaction_param_s *param,
                unsigned char *dst_hash);
    int (*validate)(struct transaction_s *t, unsigned char *dst_hash);
    int (*dump)(struct transaction_s *t);
};

extern const struct transaction_mod_s transaction;
extern const struct transaction_sub_s transaction_file_add;

#endif
