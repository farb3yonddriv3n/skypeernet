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

static int hash_meta(struct transaction_s *t,
                     unsigned char *dst_hash)
{
    char buffer[2048];
    snprintf(buffer, sizeof(buffer), "%s%ld%s%s",
             t->action.add.meta.name,
             t->action.add.meta.size,
             t->action.add.meta.description,
             t->action.add.meta.type);
    sha256hex((unsigned char *)buffer, strlen(buffer), dst_hash);
    return 0;
}

static int hash(unsigned char *hash_meta,
                unsigned char *hash_content,
                unsigned char *hash_chunks,
                unsigned char *dst_hash)
{
    char buffer[1024];
    snprintf(buffer, sizeof(buffer), "%.*s%.*s%.*s",
             SHA256HEX, hash_meta,
             SHA256HEX, hash_content,
             SHA256HEX, hash_chunks);
    sha256hex((unsigned char *)buffer, strlen(buffer), dst_hash);
    return 0;
}

static int init(struct transaction_s *t, struct transaction_param_s *param,
                unsigned char *dst_hash)
{
    memset(&t->action.add, 0, sizeof(t->action.add));

    char *fcontent;
    size_t size = eioie_fread(&fcontent, param->action.add.name);
    if (size <= 0) return -1;
    t->action.add.meta.size = size;
    int ret = hash_meta(t, t->action.add.meta.hash);
    if (ret != 0) return -1;

    sha256hex((unsigned char *)fcontent, size, t->action.add.content.hash);

    ret = file_chunks(fcontent, size, &t->action.add.chunks.array,
                          &t->action.add.chunks.count);
    if (ret != 0) return -1;
    ret = hash_chunks(t, t->action.add.chunks.hash);
    if (ret != 0) return -1;

    hash(t->action.add.meta.hash,
         t->action.add.content.hash,
         t->action.add.chunks.hash,
         t->action.add.hash);

    memcpy(dst_hash, t->action.add.hash, sizeof(t->action.add.hash));

    return 0;
}

static int validate(struct transaction_s *t, unsigned char *dst_hash)
{
    unsigned char hmeta[SHA256HEX];
    int ret = hash_meta(t, hmeta);
    if (ret != 0) return ret;

    unsigned char hchunks[SHA256HEX];
    ret = hash_chunks(t, hchunks);
    if (ret != 0) return ret;

    return hash(hmeta,
                t->action.add.content.hash,
                hchunks,
                dst_hash);
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
    .validate = validate,
    .dump     = dump,
    //.hash     = hash;
    //.dispatch = dispatch;
    //.export   = export,
};
