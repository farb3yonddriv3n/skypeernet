#include <common.h>

static int hash_chunks(struct transaction_s *t,
                       unsigned char *dst_hash)
{
    if (!t || !dst_hash) return -1;
    unsigned char prev[SHA256HEX];
    memset(prev, 0, sizeof(prev));
    int i;
    for (i = 0; i < t->action.add.chunks.size; i++) {
        struct file_chunk_s *fc = &t->action.add.chunks.array[i];
        char buffer[2048];
        snprintf(buffer, sizeof(buffer), "%.*s%ld%d%s%s%.*s%.*s",
                 (int)sizeof(prev), prev,
                 fc->size,
                 fc->part,
                 fc->tag,
                 fc->timeiter,
                 (int)sizeof(fc->hash.content), fc->hash.content,
                 (int)sizeof(fc->hash.chunk), fc->hash.chunk);
        sha256hex((unsigned char *)buffer, strlen(buffer), dst_hash);
        memcpy(prev, dst_hash, SHA256HEX);
    }
    return 0;
}

static int hash_slots(struct file_s *f, unsigned char *dst_hash)
{
    if (!f || !dst_hash) return -1;
    unsigned char prev[SHA256HEX];
    memset(prev, 0, sizeof(prev));
    memset(dst_hash, 0, SHA256HEX);
    int i;
    for (i = 0; i < f->enc.slots.size; i++) {
        struct file_key_slot_s *s = &f->enc.slots.array[i];
        char buffer[2048];
        snprintf(buffer, sizeof(buffer), "%.*s%.*s%s",
                 (int)sizeof(prev), prev,
                 (int)sizeof(s->to), s->to,
                 s->key);
        sha256hex((unsigned char *)buffer, strlen(buffer), dst_hash);
        memcpy(prev, dst_hash, SHA256HEX);
    }
    return 0;
}

static int hash_meta(struct transaction_s *t,
                     unsigned char *dst_hash)
{
    if (!t || !dst_hash) return -1;
    struct file_s *f = &t->action.add;
    // Scheme binding keeps pre-CEK (legacy) hashes byte-identical while
    // making the scheme and key slots tamper-evident for new files:
    //   legacy -> ""    plain -> "plain"    cek -> "cek" + slots hash
    char bind[SHA256HEX + 8];
    if (f->enc.scheme == FILE_SCHEME_LEGACY)
        bind[0] = 0;
    else if (f->enc.scheme == FILE_SCHEME_PLAIN)
        snprintf(bind, sizeof(bind), "plain");
    else {
        unsigned char hslots[SHA256HEX];
        ifr(hash_slots(f, hslots));
        snprintf(bind, sizeof(bind), "cek%.*s", (int)sizeof(hslots), hslots);
    }
    char buffer[4096];
    snprintf(buffer, sizeof(buffer), "%s%ld%s%s%s",
             f->meta.name,
             f->meta.size,
             f->meta.description.enc,
             f->meta.tags,
             bind);
    sha256hex((unsigned char *)buffer, strlen(buffer), dst_hash);
    return 0;
}

static int hash(unsigned char *hash_cmeta,
                unsigned char *hash_chunks,
                unsigned char *pubkeyhash,
                unsigned char *dst_hash)
{
    if (!hash_cmeta || !hash_chunks || !pubkeyhash || !dst_hash) return -1;
    char buffer[1024];
    snprintf(buffer, sizeof(buffer), "%.*s%.*s%.*s",
             SHA256HEX, hash_cmeta,
             SHA256HEX, hash_chunks,
             SHA256HEX, pubkeyhash);
    sha256hex((unsigned char *)buffer, strlen(buffer), dst_hash);
    return 0;
}

static int slot_add(struct file_s *f, unsigned char *pubkeyhash,
                    RSA *pubkey, unsigned char *cek)
{
    if (!f || !pubkeyhash || !pubkey || !cek) return -1;
    f->enc.slots.array = realloc(f->enc.slots.array,
                                 ++f->enc.slots.size * sizeof(struct file_key_slot_s));
    if (!f->enc.slots.array) return -1;
    struct file_key_slot_s *s = &f->enc.slots.array[f->enc.slots.size - 1];
    memset(s, 0, sizeof(*s));
    memcpy(s->to, pubkeyhash, sizeof(s->to));
    unsigned char *wrapped;
    int            nwrapped;
    ifr(rsa_encrypt(pubkey, cek, FILE_CEK_SIZE, &wrapped, &nwrapped));
    size_t         nb64;
    unsigned char *b64 = base64_encode(wrapped, nwrapped, &nb64);
    free(wrapped);
    if (!b64 || nb64 >= sizeof(s->key)) {
        free(b64);
        return -1;
    }
    snprintf(s->key, sizeof(s->key), "%.*s", (int)nb64, b64);
    free(b64);
    return 0;
}

static int slot_add_group(struct file_s *f, unsigned char *gkhash,
                          unsigned char *gk, unsigned char *cek)
{
    if (!f || !gkhash || !gk || !cek) return -1;
    f->enc.slots.array = realloc(f->enc.slots.array,
                                 ++f->enc.slots.size * sizeof(struct file_key_slot_s));
    if (!f->enc.slots.array) return -1;
    struct file_key_slot_s *s = &f->enc.slots.array[f->enc.slots.size - 1];
    memset(s, 0, sizeof(*s));
    memcpy(s->to, gkhash, sizeof(s->to));
    unsigned char iv[AES_IV_SIZE];
    if (RAND_bytes(iv, sizeof(iv)) != 1) return -1;
    unsigned char tag[AES_TAG_SIZE];
    unsigned char buffer[sizeof(iv) + sizeof(tag) + FILE_CEK_SIZE + AES_TAG_SIZE];
    int nct = aes_encrypt(cek, FILE_CEK_SIZE, NULL, 0, gk, iv,
                          buffer + sizeof(iv) + sizeof(tag), tag);
    if (nct < 1) return -1;
    memcpy(buffer, iv, sizeof(iv));
    memcpy(buffer + sizeof(iv), tag, sizeof(tag));
    size_t         nb64;
    unsigned char *b64 = base64_encode(buffer, sizeof(iv) + sizeof(tag) + nct,
                                       &nb64);
    if (!b64 || nb64 >= sizeof(s->key)) {
        free(b64);
        return -1;
    }
    snprintf(s->key, sizeof(s->key), "%.*s", (int)nb64, b64);
    free(b64);
    return 0;
}

static int recipient_slot(struct file_s *f, unsigned char *cek,
                          const char *pubkeyhash)
{
    if (!f || !cek || !pubkeyhash) return -1;
    if (dmemcmp(pubkeyhash, SHA256HEX,
                psig->cfg.keys.local.hash.public,
                sizeof(psig->cfg.keys.local.hash.public)))
        return 0;
    int i;
    for (i = 0; i < f->enc.slots.size; i++)
        if (memcmp(f->enc.slots.array[i].to, pubkeyhash, SHA256HEX) == 0)
            return 0;
    struct config_key_s *ck;
    ifr(config_keyexists(&psig->cfg, (unsigned char *)pubkeyhash, &ck));
    if (ck && ck->type == CONFIG_KEY_GROUP)
        return slot_add_group(f, (unsigned char *)pubkeyhash, ck->aes.key, cek);
    if (ck && ck->rsa.public)
        return slot_add(f, (unsigned char *)pubkeyhash, ck->rsa.public, cek);
    struct world_peer_s wp = { .found = NULL };
    memcpy(wp.pubkeyhash, pubkeyhash, sizeof(wp.pubkeyhash));
    ifr(list.map(&psig->peers, world.peer.findpubkeyhash, &wp));
    if (wp.found && wp.found->pubkey.n > 0) {
        RSA *pub = NULL;
        ifr(rsa_pubkey_from_pem(&wp.found->pubkey, &pub));
        int ret = slot_add(f, (unsigned char *)pubkeyhash, pub, cek);
        RSA_free(pub);
        return ret;
    }
    printf("Unknown recipient %.*s\n", SHA256HEX, pubkeyhash);
    printf("The peer must be online, or import their public key as\n");
    printf("%s/%.*s.pub.pem (or a group key as %.*s.grp) and restart\n",
           psig->cfg.dir.keys, SHA256HEX, pubkeyhash,
           SHA256HEX, pubkeyhash);
    return -1;
}

static int recipients_slots(struct file_s *f, unsigned char *cek,
                            const char *to)
{
    if (!f || !cek || !to) return -1;
    char list[4096];
    if (strlen(to) >= sizeof(list)) return -1;
    snprintf(list, sizeof(list), "%s", to);
    char *save = NULL;
    char *tok  = strtok_r(list, ",", &save);
    while (tok) {
        if (strlen(tok) != SHA256HEX) {
            printf("Invalid recipient pubkeyhash '%s' (need %d hex chars)\n",
                   tok, SHA256HEX);
            return -1;
        }
        ifr(recipient_slot(f, cek, tok));
        tok = strtok_r(NULL, ",", &save);
    }
    return 0;
}

static int desc_encrypt(struct file_s *f, const char *name,
                        unsigned char *cek)
{
    if (!f || !name || !cek) return -1;
    unsigned char iv[AES_IV_SIZE];
    if (RAND_bytes(iv, sizeof(iv)) != 1) return -1;
    unsigned char tag[AES_TAG_SIZE];
    unsigned char buffer[sizeof(iv) + sizeof(tag) + 256];
    int nct = aes_encrypt((unsigned char *)name, strlen(name), NULL, 0,
                          cek, iv,
                          buffer + sizeof(iv) + sizeof(tag), tag);
    if (nct < 1) return -1;
    memcpy(buffer, iv, sizeof(iv));
    memcpy(buffer + sizeof(iv), tag, sizeof(tag));
    size_t         nb64;
    unsigned char *b64 = base64_encode(buffer, sizeof(iv) + sizeof(tag) + nct,
                                       &nb64);
    if (!b64 || nb64 >= sizeof(f->meta.description.enc)) {
        free(b64);
        return -1;
    }
    snprintf(f->meta.description.enc, sizeof(f->meta.description.enc),
             "%.*s", (int)nb64, b64);
    free(b64);
    return 0;
}

static int init(struct transaction_s *t, struct transaction_param_s *param,
                unsigned char *dst_hash)
{
    if (!t || !param || !dst_hash) return -1;
    memset(&t->action.add, 0, sizeof(t->action.add));
    struct file_s *f = &t->action.add;
    int ret;
    size_t size;
    if (os.filesize(param->action.add.pathname, &size) != 0) return -1;
    f->meta.size = size;
    unsigned char *cek = NULL;
    if (param->action.add.encrypted) {
        f->enc.scheme = FILE_SCHEME_CEK;
        if (RAND_bytes(f->enc.cek.key, sizeof(f->enc.cek.key)) != 1) return -1;
        f->enc.cek.ready = true;
        cek = f->enc.cek.key;
        ifr(slot_add(f, psig->cfg.keys.local.hash.public,
                     psig->cfg.keys.local.rsa.public, cek));
        if (param->action.add.to)
            ifr(recipients_slots(f, cek, param->action.add.to));
    } else
        f->enc.scheme = FILE_SCHEME_PLAIN;
    ret = file_chunks(param->action.add.pathname, size,
                      &f->chunks.array,
                      &f->chunks.size,
                      cek);
    if (ret != 0) return -1;
    ret = hash_chunks(t, f->chunks.hash);
    if (ret != 0) return -1;

    snprintf(f->meta.name,
             sizeof(f->meta.name),
             "%.*s",
             (int )sizeof(f->chunks.hash),
             f->chunks.hash);
    if (cek) {
        ifr(desc_encrypt(f, param->action.add.name, cek));
    } else
        snprintf(f->meta.description.enc,
                 (int )sizeof(f->meta.description.enc),
                 "%s", param->action.add.name);
    snprintf(f->meta.tags, sizeof(f->meta.tags), "%s",
             param->action.add.tags);
    ret = hash_meta(t, f->meta.hash);
    if (ret != 0) return -1;

    memcpy(t->action.add.pubkeyhash, psig->cfg.keys.local.hash.public,
           sizeof(psig->cfg.keys.local.hash.public));
    hash(t->action.add.meta.hash,
         t->action.add.chunks.hash,
         t->action.add.pubkeyhash,
         t->action.add.hash);

    memcpy(dst_hash, t->action.add.hash, sizeof(t->action.add.hash));
    t->action.add.parent = t;
    return 0;
}

static int find(struct transaction_s *t, unsigned char *file_hash,
                void **found)
{
    if (!t || !file_hash || !found) return -1;
    struct file_s *f = &t->action.add;
    if (dmemcmp(f->meta.name, strlen(f->meta.name), file_hash, SHA256HEX))
        *(struct file_s **)found = f;
    return 0;
}

static int validate(struct transaction_s *t, unsigned char *dst_hash,
                    bool *valid)
{
    if (!t || !dst_hash || !valid) return -1;
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

    hash(hmeta, hchunks, t->action.add.pubkeyhash, dst_hash);
    if (memcmp(t->action.add.hash, dst_hash, sizeof(t->action.add.hash)) != 0) {
        *valid = false;
    }

    return 0;
}

static int load(struct transaction_s *t, json_object *tobj)
{
    if (!t || !tobj) return -1;
    struct file_s *f = &t->action.add;
    json_object *fadd;
    json_object_object_get_ex(tobj, "fileadd", &fadd);

    json_object *obj;
    json_object *fmeta;
    json_object_object_get_ex(fadd,  "meta", &fmeta);
    BIND_STRLEN(f->meta.description.enc, "desc", obj, fmeta);
    BIND_STRLEN(f->meta.tags,        "tags", obj, fmeta);
    BIND_STR(f->meta.hash,           "hash", obj, fmeta);
    BIND_STRLEN(f->meta.name,        "name", obj, fmeta);
    BIND_INT64(f->meta.size,         "size", obj, fmeta);

    // Blocks predating the enc block use the legacy privkey-derived scheme.
    f->enc.scheme = FILE_SCHEME_LEGACY;
    json_object *jenc;
    if (json_object_object_get_ex(fadd, "enc", &jenc)) {
        json_object *jscheme;
        json_object_object_get_ex(jenc, "scheme", &jscheme);
        int scheme = json_object_get_int(jscheme);
        if (scheme < FILE_SCHEME_PLAIN || scheme > FILE_SCHEME_CEK) return -1;
        f->enc.scheme = scheme;
        json_object *jslots;
        json_object_object_get_ex(jenc, "slots", &jslots);
        if (json_object_get_type(jslots) == json_type_array) {
            array_list *slots_array = json_object_get_array(jslots);
            int i;
            for (i = 0; i < array_list_length(slots_array); i++) {
                json_object *slot_item = array_list_get_idx(slots_array, i);
                f->enc.slots.array = realloc(f->enc.slots.array,
                                             ++f->enc.slots.size *
                                             sizeof(struct file_key_slot_s));
                if (!f->enc.slots.array) return -1;
                struct file_key_slot_s *s =
                    &f->enc.slots.array[f->enc.slots.size - 1];
                memset(s, 0, sizeof(*s));
                BIND_STR(s->to,     "to",  obj, slot_item);
                BIND_STRLEN(s->key, "key", obj, slot_item);
            }
        }
    }

    f->meta.description.flag = DESC_NONE;
    f->meta.finalized        = false;

    BIND_STR(f->hash, "hash", obj, fadd);
    BIND_STR(f->pubkeyhash, "pubkeyhash", obj, fadd);

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
            memset(f->chunks.array[i].tag, 0, sizeof(f->chunks.array[i].tag));
            BIND_STRLEN(f->chunks.array[i].tag, "tag",  obj, chunk_item);
            memset(f->chunks.array[i].timeiter, 0, sizeof(f->chunks.array[i].timeiter));
            BIND_STRLEN(f->chunks.array[i].timeiter, "timeiter", obj, chunk_item);

            json_object *chash;
            json_object_object_get_ex(chunk_item, "hash", &chash);

            BIND_STR(f->chunks.array[i].hash.chunk,   "chunk",   obj, chash);
            BIND_STR(f->chunks.array[i].hash.content, "content", obj, chash);
        }
    }
    f->parent = t;
    return 0;
}

static int save(struct transaction_s *t, json_object **parent)
{
    if (!t || !parent) return -1;
    struct file_s *f = &t->action.add;

    *parent = json_object_new_object();
    json_object *fhash = json_object_new_string_len((const char *)f->hash, sizeof(f->hash));
    json_object_object_add(*parent, "hash", fhash);
    json_object *pkhash = json_object_new_string_len((const char *)f->pubkeyhash, sizeof(f->pubkeyhash));
    json_object_object_add(*parent, "pubkeyhash", pkhash);

    json_object *meta = json_object_new_object();
    json_object_object_add(*parent, "meta", meta);
    json_object *meta_name = json_object_new_string(f->meta.name);
    json_object_object_add(meta, "name", meta_name);
    json_object *meta_size = json_object_new_int64(f->meta.size);
    json_object_object_add(meta, "size", meta_size);
    json_object *meta_desc = json_object_new_string(f->meta.description.enc);
    json_object_object_add(meta, "desc", meta_desc);
    json_object *meta_tags = json_object_new_string(f->meta.tags);
    json_object_object_add(meta, "tags", meta_tags);
    json_object *meta_hash = json_object_new_string_len((const char *)f->meta.hash,
                                                        sizeof(f->meta.hash));
    json_object_object_add(meta, "hash", meta_hash);

    if (f->enc.scheme != FILE_SCHEME_LEGACY) {
        json_object *jenc = json_object_new_object();
        json_object_object_add(*parent, "enc", jenc);
        json_object *jscheme = json_object_new_int(f->enc.scheme);
        json_object_object_add(jenc, "scheme", jscheme);
        json_object *jslots = json_object_new_array();
        json_object_object_add(jenc, "slots", jslots);
        int si;
        for (si = 0; si < f->enc.slots.size; si++) {
            struct file_key_slot_s *s = &f->enc.slots.array[si];
            json_object *slot = json_object_new_object();
            json_object_array_add(jslots, slot);
            json_object *sto = json_object_new_string_len((const char *)s->to,
                                                          sizeof(s->to));
            json_object_object_add(slot, "to", sto);
            json_object *skey = json_object_new_string(s->key);
            json_object_object_add(slot, "key", skey);
        }
    }

    json_object *chunks = json_object_new_object();
    json_object_object_add(*parent, "chunks", chunks);
    json_object *chunks_hash = json_object_new_string_len((const char *)f->chunks.hash,
                                                          sizeof(f->chunks.hash));
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
        json_object *chunk_tag = json_object_new_string((const char *)fc->tag);
        json_object_object_add(chunk, "tag", chunk_tag);
        json_object *timeiter = json_object_new_string((const char *)fc->timeiter);
        json_object_object_add(chunk, "timeiter", timeiter);

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

static int dump(struct transaction_s *t, json_object **obj)
{
    if (!t || !obj) return -1;
    struct file_s *f = &t->action.add;
    *obj = json_object_new_object();
    json_object *name = json_object_new_string((const char *)f->meta.name);
    json_object *size = json_object_new_int64(f->meta.size);
    json_object *tags = json_object_new_string((const char *)f->meta.tags);
    json_object_object_add(*obj, "name", name);
    json_object_object_add(*obj, "size", size);
    json_object_object_add(*obj, "tags", tags);

    int isfinalized(char *desc, int ndesc, bool *flag) {
        if (!flag || !desc) return -1;
        char fullpath[1024];
        snprintf(fullpath, sizeof(fullpath), "%s/%.*s",
                                             psig->cfg.dir.finalized,
                                             ndesc, desc);
        return os.fileexists(fullpath, flag);
    }

    bool bdecryptable = false;
    if (CMPFLAG(f->meta.description.flag, (DESC_TRYDEC|DESC_DECODED))) {
        json_object *description = json_object_new_string(f->meta.description.dec);
        json_object_object_add(*obj, "description", description);
        ifr(isfinalized(f->meta.description.dec, strlen(f->meta.description.dec),
                        &f->meta.finalized));
        bdecryptable = true;
    } else if (!(f->meta.description.flag & DESC_TRYDEC)) {
        int            ndesc;
        unsigned char *desc;
        f->meta.description.flag = DESC_TRYDEC;
        ifr(decode_desc(f, &desc, &ndesc));
        if (desc && ndesc < sizeof(f->meta.description.dec)) {
            strncpy(f->meta.description.dec, (char *)desc, ndesc);
            json_object *description = json_object_new_string_len((const char *)desc, ndesc);
            json_object_object_add(*obj, "description", description);
            ifr(isfinalized((char *)desc, ndesc, &f->meta.finalized));
            free(desc);
            f->meta.description.flag |= DESC_DECODED;
            bdecryptable = true;
        };
    }

    json_object *decryptable = json_object_new_boolean(bdecryptable);
    json_object_object_add(*obj, "decryptable", decryptable);

    json_object *encrypted = json_object_new_boolean(f->enc.scheme !=
                                                     FILE_SCHEME_PLAIN);
    json_object_object_add(*obj, "encrypted", encrypted);
    json_object *scheme = json_object_new_int(f->enc.scheme);
    json_object_object_add(*obj, "scheme", scheme);

    json_object *finalized = json_object_new_boolean(f->meta.finalized);
    json_object_object_add(*obj, "finalized", finalized);

    json_object *chunks = json_object_new_array();
    json_object_object_add(*obj, "chunks", chunks);
    int incomplete;
    int i;
    for (i = 0, incomplete = 0; i < f->chunks.size; i++) {
        char chunkpath[256];
        snprintf(chunkpath, sizeof(chunkpath), "%s/%.*s",
                 psig->cfg.dir.download,
                 (int )sizeof(f->chunks.array[i].hash.content),
                 f->chunks.array[i].hash.content);
        bool cexists;
        ifr(os.fileexists(chunkpath, &cexists));
        json_object *chunk = json_object_new_object();
        json_object *cname = json_object_new_string_len((const char *)f->chunks.array[i].hash.content,
                             (int )sizeof(f->chunks.array[i].hash.content));
        json_object *cdownloaded = json_object_new_boolean(cexists ? true : false);
        if (!cexists) incomplete++;
        json_object *csize = json_object_new_int64(f->chunks.array[i].size);
        json_object_object_add(chunk, "chunk", cname);
        json_object_object_add(chunk, "size", csize);
        json_object_object_add(chunk, "downloaded", cdownloaded);
        json_object_array_add(chunks, chunk);
    }

    json_object *chunks_done  = json_object_new_int(f->chunks.size - incomplete);
    json_object *chunks_total = json_object_new_int(f->chunks.size);
    json_object *complete = json_object_new_boolean(incomplete == 0 ? true : false);
    json_object_object_add(*obj, "complete", complete);
    json_object_object_add(*obj, "chunks_done", chunks_done);
    json_object_object_add(*obj, "chunks_total", chunks_total);
    return 0;
}

static int clean(struct transaction_s *t)
{
    if (!t) return -1;
    file_chunks_free(t->action.add.chunks.array);
    free(t->action.add.enc.slots.array);
    return 0;
}

const struct transaction_sub_s transaction_file_add = {
    .init        = init,
    .validate    = validate,
    .find        = find,
    .dump        = dump,
    .clean       = clean,
    .data.load   = load,
    .data.save   = save,
};
