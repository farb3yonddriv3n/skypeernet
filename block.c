#include <common.h>

static int mallocz(struct block_s **b)
{
    *b = malloc(sizeof(**b));
    if (!(*b)) return -1;
    memset(*b, 0, sizeof(**b));
    return 0;
}

static int init(struct block_s **b, unsigned char *prev,
                unsigned char *current, const uint64_t nounce,
                const uint64_t index)
{
    if (mallocz(b) != 0) return -1;
    memcpy((*b)->hash.prev, prev, SHA256HEX);
    memcpy((*b)->hash.current, current, SHA256HEX);
    (*b)->hash.nounce = nounce;
    (*b)->index = index;
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

static int transaction_add(struct block_s *b, struct transaction_s *t)
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
    memcpy(b->hash.transaction, prev, SHA256HEX);
    return 0;
}

static int import(struct block_s **b, json_object *bobj)
{
    if (mallocz(b) != 0) return -1;

    json_object *bhash;
    json_object_object_get_ex(bobj, "hash", &bhash);

#define BIND_STR(m_dst, m_name, m_src, m_obj)\
        json_object_object_get_ex(m_obj, m_name, &m_src);\
        if (json_object_get_string_len(m_src) == sizeof((*b)->m_dst))\
            memcpy((*b)->m_dst, json_object_get_string(m_src),\
                   json_object_get_string_len(m_src));

#define BIND_INT(m_dst, m_name, m_src, m_obj)\
        json_object_object_get_ex(m_obj, m_name, &m_src);\
        (*b)->m_dst = json_object_get_int64(m_src);

    json_object *obj;
    BIND_STR(hash.prev,        "prev",        obj, bhash);
    BIND_STR(hash.current,     "current",     obj, bhash);
    BIND_STR(hash.transaction, "transaction", obj, bhash);
    BIND_INT(hash.nounce,      "nounce",      obj, bhash);
    BIND_INT(index,            "index",       obj, bobj);

    json_object *transactions;
    json_object_object_get_ex(bobj, "transactions", &transactions);
    if (json_object_get_type(transactions) == json_type_array) {
        array_list *transactions_array = json_object_get_array(transactions);
        int i;
        for (i = 0; i < array_list_length(transactions_array); i++) {
            json_object *transaction_item = array_list_get_idx(transactions_array, i);
            struct transaction_s *t;
            transaction.data.import(&t, transaction_item);
            block.transaction.add(*b, t);
        }
    }

    return 0;
}

static int export(struct block_s *b, json_object **blockobj)
{
    *blockobj = json_object_new_object();
    json_object *hash = json_object_new_object();
    json_object *index = json_object_new_int64(b->index);
    json_object_object_add(*blockobj, "index", index);
    json_object_object_add(*blockobj, "hash", hash);

    json_object *prev = json_object_new_string_len((const char *)b->hash.prev,
                                                   sizeof(b->hash.prev));
    json_object_object_add(hash, "prev", prev);
    json_object *current = json_object_new_string_len((const char *)b->hash.current,
                                                      sizeof(b->hash.current));
    json_object_object_add(hash, "current", current);
    json_object *t = json_object_new_string_len((const char *)b->hash.transaction,
                                                      sizeof(b->hash.transaction));
    json_object_object_add(hash, "transaction", t);
    json_object *nounce = json_object_new_int64(b->hash.nounce);
    json_object_object_add(hash, "nounce", nounce);

    int i;
    json_object *transactions = json_object_new_array();
    json_object_object_add(*blockobj, "transactions", transactions);
    for (i = 0; i < b->transactions.size; i++) {
        json_object *tobj;
        int ret = transaction.data.export(b->transactions.array[i], &tobj);
        if (ret != 0) return ret;
        json_object_array_add(transactions, tobj);
    }

    return 0;
}

const struct module_block_s block = {
    .init             = init,
    .mine             = mine,
    .validate         = validate,
    .transaction.add  = transaction_add,
    .transaction.hash = transaction_hash,
    .data.import      = import,
    .data.export      = export,
};
