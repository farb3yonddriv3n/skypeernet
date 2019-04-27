#include <common.h>

static int hash_chunks(struct transaction_s *t,
                       unsigned char *dst_hash)
{
    unsigned char prev[SHA256HEX];
    memset(prev, 0, sizeof(prev));
    int i;
    for (i = 0; i < t->action.add.chunks.size; i++) {
        struct file_chunk_s *fc = &t->action.add.chunks.array[i];
        char buffer[SHA256HEX * 4];
        snprintf(buffer, sizeof(buffer), "%.*s%ld%d%.*s%.*s",
                 (int)sizeof(prev), prev,
                 fc->size,
                 fc->part,
                 (int)sizeof(fc->hash.content), fc->hash.content,
                 (int)sizeof(fc->hash.chunk), fc->hash.chunk);
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

static int hash(unsigned char *hash_cmeta,
                unsigned char *hash_content,
                unsigned char *hash_chunks,
                unsigned char *dst_hash)
{
    char buffer[1024];
    snprintf(buffer, sizeof(buffer), "%.*s%.*s%.*s",
             SHA256HEX, hash_cmeta,
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
                      &t->action.add.chunks.size);
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

static int validate(struct transaction_s *t, unsigned char *dst_hash,
                    bool *valid)
{
    unsigned char hmeta[SHA256HEX];
    if (hash_meta(t, hmeta) != 0) return -1;
    if (memcmp(t->action.add.meta.hash, hmeta, sizeof(hmeta)) != 0) {
        *valid = false;
        return 0;
    }

    unsigned char hchunks[SHA256HEX];
    if (hash_chunks(t, hchunks) != 0) return -1;
    if (memcmp(t->action.add.chunks.hash, hchunks, sizeof(hchunks)) != 0) {
        *valid = false;
        return 0;
    }

    hash(hmeta, t->action.add.content.hash, hchunks, dst_hash);
    if (memcmp(t->action.add.hash, dst_hash, sizeof(t->action.add.hash)) != 0)
        *valid = false;

    return 0;
}

static int load(struct transaction_s *t, json_object *tobj)
{
    struct file_s *f = &t->action.add;
    json_object *fadd;
    json_object_object_get_ex(tobj, "fileadd", &fadd);

#define BIND_STR(m_dst, m_name, m_src, m_obj)\
        assert(json_object_object_get_ex(m_obj, m_name, &m_src) == true);\
        if (json_object_get_string_len(m_src) == sizeof(m_dst))\
            memcpy(m_dst, json_object_get_string(m_src),\
                   json_object_get_string_len(m_src));

#define BIND_STRLEN(m_dst, m_name, m_src, m_obj)\
        json_object_object_get_ex(m_obj, m_name, &m_src);\
        if (json_object_get_string_len(m_src) < sizeof(m_dst))\
            memcpy(m_dst, json_object_get_string(m_src),\
                   json_object_get_string_len(m_src));

#define BIND_INT64(m_dst, m_name, m_src, m_obj)\
        json_object_object_get_ex(m_obj, m_name, &m_src);\
        m_dst = json_object_get_int64(m_src);

    json_object *obj;
    json_object *fcontent;
    json_object_object_get_ex(fadd, "content", &fcontent);
    BIND_STR(f->content.hash, "hash", obj, fcontent);

    json_object *fmeta;
    json_object_object_get_ex(fadd,  "meta", &fmeta);
    BIND_STRLEN(f->meta.description, "desc", obj, fmeta);
    BIND_STR(f->meta.hash,           "hash", obj, fmeta);
    BIND_STRLEN(f->meta.name,        "name", obj, fmeta);
    BIND_STRLEN(f->meta.type,        "type", obj, fmeta);
    BIND_INT64(f->meta.size,         "size", obj, fmeta);

    BIND_STR(f->hash, "hash", obj, fadd);

    json_object *fchunks;
    json_object_object_get_ex(fadd, "chunks", &fchunks);
    BIND_STR(f->chunks.hash, "hash", obj, fchunks);

    json_object *farray;
    json_object_object_get_ex(fchunks, "array", &farray);
    if (json_object_get_type(farray) == json_type_array) {
        array_list *chunks_array = json_object_get_array(farray);
        int i;
        if (file_chunks_alloc(f, array_list_length(chunks_array)) != 0)
            return -1;
        for (i = 0; i < array_list_length(chunks_array); i++) {
            json_object *chunk_item = array_list_get_idx(chunks_array, i);

            BIND_INT64(f->chunks.array[i].size, "size", obj, chunk_item);
            BIND_INT64(f->chunks.array[i].part, "part", obj, chunk_item);

            json_object *chash;
            json_object_object_get_ex(chunk_item, "hash", &chash);

            BIND_STR(f->chunks.array[i].hash.chunk,   "chunk",   obj, chash);
            BIND_STR(f->chunks.array[i].hash.content, "content", obj, chash);
        }
    }

    return 0;
}

static int save(struct transaction_s *t, json_object **parent)
{
    struct file_s *f = &t->action.add;

    *parent = json_object_new_object();
    json_object *fhash = json_object_new_string_len((const char *)f->hash, sizeof(f->hash));
    json_object_object_add(*parent, "hash", fhash);

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
    for (i = 0; i < f->chunks.size; i++) {
        struct file_chunk_s *fc = &f->chunks.array[i];
        struct json_object *chunk = json_object_new_object();
        json_object_array_add(chunks_array, chunk);
        json_object *chunk_size = json_object_new_int64(fc->size);
        json_object_object_add(chunk, "size", chunk_size);
        json_object *chunk_part = json_object_new_int(fc->part);
        json_object_object_add(chunk, "part", chunk_part);

        struct json_object *chunk_hash = json_object_new_object();
        json_object_object_add(chunk, "hash", chunk_hash);
        json_object *chunk_hash_content = json_object_new_string_len((const char *)fc->hash.content,
                                                                     sizeof(fc->hash.content));
        json_object_object_add(chunk_hash, "content", chunk_hash_content);
        json_object *chunk_hash_chunk = json_object_new_string_len((const char *)fc->hash.chunk,
                                                                   sizeof(fc->hash.chunk));
        json_object_object_add(chunk_hash, "chunk", chunk_hash_chunk);
    }
    return 0;
}

static int dump(struct transaction_s *t)
{
    transaction.metadump(t);
    printf("File content hash: %.*s\n", (int)sizeof(t->action.add.hash), t->action.add.hash);
    return 0;
}

const struct transaction_sub_s transaction_file_add = {
    .init        = init,
    .validate    = validate,
    .dump        = dump,
    .data.load   = load,
    .data.save   = save,
};
