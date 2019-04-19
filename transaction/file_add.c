#include <common.h>

static int hash_chunks(struct transaction_s *t,
                       unsigned char *dst_hash)
{
    unsigned char prev[SHA256HEX];
    memset(prev, 0, sizeof(prev));
    int i;
    for (i = 0; i < t->action.add.chunks.count; i++) {
        struct file_chunk_s *fc = &t->action.add.chunks.array[i];
        char buffer[SHA256HEX * 4];
        snprintf(buffer, sizeof(buffer), "%.*s%.*s",
                 (int)sizeof(prev), prev,
                 (int)sizeof(fc->chunk_hash), fc->chunk_hash);
        sha256hex((unsigned char *)buffer, strlen(buffer), dst_hash);
        memcpy(prev, dst_hash, SHA256HEX);
    }

    return 0;
}

static int init(struct transaction_s *t, struct transaction_param_s *param,
                unsigned char **dst_hash)
{
    char *fcontent;
    size_t size = eioie_fread(&fcontent, param->action.add.name);
    if (size <= 0) return -1;

    t->action.add.meta.size = size;
    sha256hex((unsigned char *)fcontent, size, t->action.add.hash);
    int ret = file_chunks(fcontent, size, &t->action.add.chunks.array,
                          &t->action.add.chunks.count);
    if (ret != 0) return -1;

    ret = hash_chunks(t, t->action.add.chunks.hash);
    if (ret != 0) return -1;

    *dst_hash = t->action.add.hash;

    return 0;
}

/*
static unsigned char *hash(struct transaction_s *t)
{
    return t->action.add.hash;
}
*/

static int dump(struct transaction_s *t)
{
    transaction.metadump(t);
    printf("File content hash: %.*s\n", (int)sizeof(t->action.add.hash), t->action.add.hash);
    return 0;
}

const struct transaction_sub_s transaction_file_add = {
    .init     = init,
    //.clean    = clean;
    .dump     = dump,
    //.hash     = hash;
    //.dispatch = dispatch;
};
