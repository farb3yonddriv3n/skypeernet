#include <common.h>

#define MAX_TRANSACTIONS 1
#define VERSION          1

enum transaction_process_e {
    PROCESS_INIT,
    PROCESS_VALIDATE,
    PROCESS_DUMP,
    PROCESS_IMPORT,
    PROCESS_EXPORT,
};

struct transaction_map_s {
    enum transaction_e              type;
    const struct transaction_sub_s *module;
};

static struct transaction_map_s transaction_module[MAX_TRANSACTIONS] = {
    { TFILE_ADD, &transaction_file_add },
};

static int process(enum transaction_process_e ptype, struct transaction_s *t,
                   struct transaction_param_s *param, unsigned char *dst_hash)
{
    int i;
    for (i = 0; i < MAX_TRANSACTIONS; i++)
        if (transaction_module[i].type == t->type)
            switch (ptype) {
                case PROCESS_INIT:
                    return transaction_module[i].module->init(t, param, dst_hash);
                case PROCESS_VALIDATE:
                    return transaction_module[i].module->validate(t, dst_hash);
                case PROCESS_DUMP:
                    return transaction_module[i].module->dump(t);
                case PROCESS_IMPORT:
                    return transaction_module[i].module->data.load(t, param->action.load.obj);
                case PROCESS_EXPORT:
                    return transaction_module[i].module->data.save(t, param->action.save.obj);
                default:
                    return -1;
            }
    return -1;
}

static void hash_calc(struct transaction_s *t, unsigned char *sub_hash,
                      unsigned char *dst_hash)
{
    char buffer[2048];
    snprintf(buffer, sizeof(buffer), "%d%d%d%.*s",
             t->version,
             t->timestamp,
             t->type,
             SHA256HEX,
             sub_hash);
    sha256hex((const unsigned char *)buffer, strlen(buffer), dst_hash);
}

static int mallocz(struct transaction_s **t)
{
    *t = malloc(sizeof(**t));
    if (!(*t)) return -1;
    memset(*t, 0, sizeof(**t));
    return 0;
}

static int init(struct transaction_s **t,
                struct transaction_param_s *param)
{
    if (mallocz(t) != 0) return -1;

    (*t)->version   = VERSION;
    (*t)->timestamp = 100;
    (*t)->type      = param->type;

    unsigned char sub_hash[SHA256HEX];
    int ret = process(PROCESS_INIT, *t, param, sub_hash);
    if (ret != 0) return ret;

    hash_calc(*t, sub_hash, (*t)->hash);

    return 0;
}

static int validate(struct transaction_s *t, bool *valid)
{
    *valid = true;

    unsigned char action_hash[SHA256HEX];
    int ret = process(PROCESS_VALIDATE, t, NULL, action_hash);
    if (ret != 0) return ret;

    unsigned char transaction_hash[SHA256HEX];
    hash_calc(t, action_hash, transaction_hash);

    if (memcmp(t->hash, transaction_hash, sizeof(t->hash)) != 0)
        *valid = false;

    return 0;
}

static unsigned char *hash(struct transaction_s *t)
{
    return t->hash;
}

static void metadump(struct transaction_s *t)
{
    printf("Version: %d\n",   t->version);
    printf("Timestamp: %d\n", t->timestamp);
    printf("Hash: %.*s\n",    (int)sizeof(t->hash), t->hash);
    printf("Type: %d\n",      t->type);
}

static int dump(struct transaction_s *t)
{
    return process(PROCESS_DUMP, t, NULL, NULL);
}

static int load(struct transaction_s **t, json_object *tobj)
{
    if (mallocz(t) != 0) return -1;

#define BIND_STR(m_dst, m_name, m_src, m_obj)\
        json_object_object_get_ex(m_obj, m_name, &m_src);\
        if (json_object_get_string_len(m_src) == sizeof((*t)->m_dst))\
            memcpy((*t)->m_dst, json_object_get_string(m_src),\
                   json_object_get_string_len(m_src));

#define BIND_INT(m_dst, m_name, m_src, m_obj)\
        json_object_object_get_ex(m_obj, m_name, &m_src);\
        (*t)->m_dst = json_object_get_int(m_src);

    json_object *obj;
    BIND_INT(version,   "version",   obj, tobj);
    BIND_INT(timestamp, "timestamp", obj, tobj);
    BIND_STR(hash,      "hash",      obj, tobj);

    json_object *blockhash;
    json_object_object_get_ex(tobj, "blockhash", &blockhash);

    BIND_STR(blockhash.prev,    "prev",    obj, blockhash);
    BIND_STR(blockhash.current, "current", obj, blockhash);

    struct transaction_param_s param = { .action.load.obj = tobj };
    return process(PROCESS_IMPORT, *t, &param, NULL);
}

static int save(struct transaction_s *t, json_object **tobj)
{
    *tobj = json_object_new_object();
    json_object *version = json_object_new_int(t->version);
    json_object_object_add(*tobj, "version", version);

    json_object *timestamp = json_object_new_int(t->timestamp);
    json_object_object_add(*tobj, "timestamp", timestamp);

    json_object *thash = json_object_new_string_len((const char *)t->hash,
                                                    sizeof(t->hash));
    json_object_object_add(*tobj, "hash", thash);

    json_object *type = json_object_new_int(t->type);
    json_object_object_add(*tobj, "type", type);

    json_object *blockhash = json_object_new_object();
    json_object_object_add(*tobj, "blockhash", blockhash);
    json_object *prev = json_object_new_string_len((const char *)t->blockhash.prev,
                                                   sizeof(t->blockhash.prev));
    json_object_object_add(blockhash, "prev", prev);
    json_object *current = json_object_new_string_len((const char *)t->blockhash.current,
                                                      sizeof(t->blockhash.current));
    json_object_object_add(blockhash, "current", current);

    json_object *fileadd;
    struct transaction_param_s param = { .action.save.obj = &fileadd };
    int ret = process(PROCESS_EXPORT, t, &param, NULL);
    if (ret != 0) return ret;
    json_object_object_add(*tobj, "fileadd", fileadd);

    return 0;
}

const struct module_transaction_s transaction = {
    .init        = init,
    .hash        = hash,
    .validate    = validate,
    .metadump    = metadump,
    .dump        = dump,
    .data.load   = load,
    .data.save   = save,
};
