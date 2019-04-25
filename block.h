#ifndef BLOCK_H_
#define BLOCK_H_

struct block_s {
    uint64_t index;
    struct {
        unsigned char prev[SHA256HEX];
        unsigned char current[SHA256HEX];
        unsigned char transaction[SHA256HEX];
        uint64_t      nounce;
    } hash;

    struct {
        struct transaction_s **array;
        size_t                 size;
    } transactions;
};

enum block_equal_e {
    BLOCK_LOCAL,
    BLOCK_REMOTE
};

struct block_equal_s {
    bool               verdict;
    enum block_equal_e winner;
    uint64_t           index;
};

struct module_block_s {
    int (*init)(struct block_s **b, unsigned char *prev,
                unsigned char *current, const uint64_t nounce,
                const uint64_t index);
    int (*mine)(unsigned char *prev_hash, unsigned char *dst_hash,
                uint64_t *nounce);
    int (*validate)(unsigned char *prev_hash, unsigned char *dst_hash,
                    const uint64_t nounce, bool *valid);
    int (*size)(struct block_s *b, size_t *s);
    int (*compare)(struct block_s *local, struct block_s *remote,
                   struct block_equal_s *equal);
    struct {
        int (*add)(struct block_s *b, struct transaction_s *t);
        int (*hash)(struct block_s *b);
    } transaction;

    struct {
        int (*import)(struct block_s **b, json_object *bobj);
        int (*export)(struct block_s *b, json_object **bobj);
    } data;
};

extern const struct module_block_s block;

#endif
