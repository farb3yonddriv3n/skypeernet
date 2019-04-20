#include <common.h>

static void hash_calc(char *buffer, const int nbuffer, unsigned char *md,
                      const uint64_t nounce, unsigned char *prev_hash)
{
    snprintf(buffer, nbuffer, "%.*s%ld",
             SHA256HEX,
             prev_hash,
             nounce);
    sha256hex((unsigned char *)buffer, strlen(buffer), md);
}

static int mine(unsigned char *prev_hash, unsigned char *dst_hash,
                uint64_t *nounce)
{
    char buffer[128];
    unsigned char md[SHA256HEX];
    char target[5];
    memset(target, '0', sizeof(target));

    for (*nounce = 0; /* void */; (*nounce)++) {
        hash_calc(buffer, sizeof(buffer), md, *nounce, prev_hash);
        if (memcmp(md, target, sizeof(target)) == 0) break;
    }

    memcpy(dst_hash, md, sizeof(md));

    return 0;
}

static int validate(unsigned char *prev_hash, unsigned char *dst_hash,
                    const uint64_t nounce, bool *valid)
{
    char buffer[128];
    unsigned char md[SHA256HEX];

    *valid = false;
    hash_calc(buffer, sizeof(buffer), md, nounce, prev_hash);
    if (memcmp(md, dst_hash, SHA256HEX) == 0) *valid = true;
    return 0;
}

const struct module_block_s block = {
    .mine     = mine,
    .validate = validate,
};
