#ifndef BLOCK_H_
#define BLOCK_H_

struct module_block_s {
    int (*mine)(unsigned char *prev_hash, unsigned char *dst_hash,
                uint64_t *nounce);
    int (*validate)(unsigned char *prev_hash, unsigned char *dst_hash,
                    const uint64_t nounce, bool *valid);
};

extern const struct module_block_s block;

#endif
