#include <common.h>

static int init(struct block_s *b, unsigned char *prev,
                unsigned char *current, const uint64_t nounce)
{
    memset(b, 0, sizeof(*b));
    memcpy(b->hash.prev, prev, SHA256HEX);
    memcpy(b->hash.current, current, SHA256HEX);
    b->hash.nounce = nounce;
    return 0;
}

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
    char target[2];
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

static int add(struct block_s *b, struct transaction_s *t)
{
    b->transactions.size++;
    b->transactions.array = realloc(b->transactions.array,
                                    sizeof(void *) * b->transactions.size);
    if (!b->transactions.array) return -1;
    b->transactions.array[b->transactions.size - 1] = t;
    return 0;
}

static int transaction_hash(struct block_s *b)
{
    struct transaction_s *t;
    unsigned char *prev = b->hash.current;
    size_t i;
    for (i = 0; i < b->transactions.size; i++) {
        t = b->transactions.array[i];
        memcpy(t->blockhash.prev, prev, SHA256HEX);
        char buffer[1024];
        snprintf(buffer, sizeof(buffer), "%.*s%.*s",
                 SHA256HEX, prev,
                 SHA256HEX, t->hash);
        sha256hex((unsigned char *)buffer, strlen(buffer), t->blockhash.current);
        prev = t->blockhash.current;
    }
    return 0;
}

static int import(struct block_s *b)
{
    return 0;
}

static int export(struct block_s *b)
{
    json_object *blockobj = json_object_new_object();
    json_object *hash = json_object_new_object();
    json_object_object_add(blockobj, "hash", hash);

    json_object *prev = json_object_new_string_len((const char *)b->hash.prev,
                                                   sizeof(b->hash.prev));
    json_object_object_add(hash, "prev", prev);
    json_object *current = json_object_new_string_len((const char *)b->hash.current,
                                                      sizeof(b->hash.current));
    json_object_object_add(hash, "current", current);
    json_object *nounce = json_object_new_int64(b->hash.nounce);
    json_object_object_add(hash, "nounce", nounce);

    int i;
    json_object *transactions = json_object_new_array();
    json_object_object_add(blockobj, "transactions", transactions);
    for (i = 0; i < b->transactions.size; i++) {
        json_object *tobj;
        int ret = transaction.data.export(b->transactions.array[i], &tobj);
        if (ret != 0) return ret;
        json_object_array_add(transactions, tobj);
    }

    printf("json: %s\n", json_object_to_json_string(blockobj));
    return 0;
}

const struct module_block_s block = {
    .init             = init,
    .mine             = mine,
    .validate         = validate,
    .transaction.add  = add,
    .transaction.hash = transaction_hash,
    .data.import      = import,
    .data.export      = export,
};
