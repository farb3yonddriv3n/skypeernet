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

static int export(struct transaction_s *t, json_object **parent)
{
    struct file_s *f = &t->action.add;

    *parent = json_object_new_object();
    json_object *hash = json_object_new_string_len((const char *)f->hash, sizeof(f->hash));
    json_object_object_add(*parent, "hash", hash);

    json_object *meta = json_object_new_object();
    json_object_object_add(*parent, "meta", meta);
    json_object *meta_name = json_object_new_string(f->meta.name);
    json_object_object_add(meta, "name", meta_name);
    json_object *meta_size = json_object_new_int64(f->meta.size);
    json_object_object_add(meta, "size", meta_size);
    json_object *meta_desc = json_object_new_string(f->meta.description);
    json_object_object_add(meta, "desc", meta_desc);
    json_object *meta_type = json_object_new_string(f->meta.type);
    json_object_object_add(meta, "type", meta_type);
    json_object *meta_hash = json_object_new_string_len((const char *)f->meta.hash,
                                                        sizeof(f->meta.hash));
    json_object_object_add(meta, "hash", meta_hash);

    json_object *content = json_object_new_object();
    json_object_object_add(*parent, "content", content);
    json_object *content_hash = json_object_new_string_len((const char *)f->content.hash,
                                                           sizeof(f->content.hash));
    json_object_object_add(content, "hash", content_hash);

    json_object *chunks = json_object_new_object();
    json_object_object_add(*parent, "chunks", chunks);
    json_object *chunks_hash = json_object_new_string((const char *)f->chunks.hash);
    json_object_object_add(chunks, "hash", chunks_hash);
    json_object *chunks_array = json_object_new_array();
    json_object_object_add(chunks, "array", chunks_array);
    int i;
    for (i = 0; i < f->chunks.count; i++) {
        struct file_chunk_s *fc = &f->chunks.array[i];
        struct json_object *chunk = json_object_new_object();
        json_object_array_add(chunks_array, chunk);
        json_object *chunk_size = json_object_new_int64(fc->size);
        json_object_object_add(chunk, "size", chunk_size);
        json_object *chunk_part = json_object_new_int(fc->part);
        json_object_object_add(chunk, "part", chunk_part);
        json_object *chunk_part_hash = json_object_new_string_len((const char *)fc->part_hash,
                                                                  sizeof(fc->part_hash));
        json_object_object_add(chunk, "part_hash", chunk_part_hash);
        json_object *chunk_chunk_hash = json_object_new_string_len((const char *)fc->chunk_hash,
                                                                   sizeof(fc->chunk_hash));
        json_object_object_add(chunk, "chunk_hash", chunk_chunk_hash);
    }
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
    .init        = init,
    //.clean     = clean;
    .validate    = validate,
    .dump        = dump,
    //.hash      = hash;
    //.dispatch  = dispatch;
    .data.export = export,
};
