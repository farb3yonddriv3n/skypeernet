#include <common.h>

int file_chunks_alloc(struct file_s *f, const int size)
{
    f->chunks.array = malloc(size * sizeof(*(f->chunks.array)));
    if (!f->chunks.array) return -1;
    f->chunks.size = size;
    return 0;
}

int file_chunks(const char *filename, size_t nbytes,
                struct file_chunk_s **chunks,
                size_t *nchunks, unsigned char *cek)
{
    if (nbytes < 1 || !chunks || !nchunks || !filename) return -1;

    int i;
    *chunks  = NULL;
    *nchunks = 0;
    size_t chunk_size = psig->cfg.net.max.chunk_size;
    for (i = 0; (i * chunk_size) < nbytes; i++) {
        *chunks = realloc(*chunks, sizeof(**chunks) * (i + 1));
        if (*chunks == NULL) return -1;
        struct file_chunk_s *fc = &((*chunks)[i]);
        fc->size = (nbytes < chunk_size) ? nbytes :
                   (((i + 1) * chunk_size) > nbytes ?
                   nbytes - (i * chunk_size) :
                   chunk_size);
        fc->part = i;
        unsigned char *fpart;
        size_t         nfpart;
        if (os.filepart(filename, i * chunk_size, fc->size, (char **)&fpart,
                        &nfpart) != 0) return -1;
        unsigned char *fpartenc = fpart;
        int nfpartenc = nfpart;
        memset(fc->timeiter, 0, sizeof(fc->timeiter));
        memset(fc->tag, 0, sizeof(fc->tag));
        if (cek) {
            fpartenc = malloc(fc->size);
            if (!fpartenc) return -1;
            unsigned char tag[AES_TAG_SIZE];
            ifr(os.gettimeiter(fc->timeiter, sizeof(fc->timeiter), i));
            nfpartenc = aes_encrypt(fpart, nfpart, NULL, 0,
                                    cek,
                                    (unsigned char *)fc->timeiter,
                                    fpartenc, tag);
            if (nfpartenc < 1) return -1;
            fc->size = nfpartenc;
            size_t         nencoded;
            unsigned char *encoded = base64_encode(tag, sizeof(tag), &nencoded);
            if (!encoded) return -1;
            snprintf(fc->tag, sizeof(fc->tag), "%.*s", (int)nencoded, encoded);
            free(encoded);
        }
        sha256hex(fpartenc, nfpartenc, fc->hash.content);
        char cfilename[256];
        snprintf(cfilename, sizeof(cfilename), "%s/%.*s", psig->cfg.dir.download,
                                                          SHA256HEX, fc->hash.content);
        ifr(os.filewrite(cfilename, "wb", (char *)fpartenc, nfpartenc));
        if (fpartenc != fpart) free(fpartenc);
        free(fpart);
        char buffer[1024];
        snprintf(buffer, sizeof(buffer), "%ld,%d,%.*s",
                 fc->size, fc->part,
                 (int)sizeof(fc->hash.content), fc->hash.content);
        sha256hex((unsigned char *)buffer, strlen(buffer), fc->hash.chunk);
        (*nchunks)++;
    }

    return 0;
}

void file_chunks_free(struct file_chunk_s *chunks)
{
    free(chunks);
}

int file_cek_resolve(struct file_s *f, unsigned char **cek)
{
    if (!f || !cek) return -1;
    *cek = NULL;
    if (f->enc.scheme != FILE_SCHEME_CEK) return 0;
    if (f->enc.cek.ready) {
        *cek = f->enc.cek.key;
        return 0;
    }
    int i;
    for (i = 0; i < f->enc.slots.size; i++) {
        struct file_key_slot_s *s = &f->enc.slots.array[i];
        if (memcmp(s->to, psig->cfg.keys.local.hash.public,
                   sizeof(s->to)) != 0) continue;
        size_t         nwrapped;
        unsigned char *wrapped = base64_decode((unsigned char *)s->key,
                                               strnlen(s->key, sizeof(s->key)),
                                               &nwrapped);
        if (!wrapped) return -1;
        unsigned char *dec;
        int            ndec;
        int ret = rsa_decrypt(psig->cfg.keys.local.rsa.private,
                              wrapped, nwrapped, &dec, &ndec);
        free(wrapped);
        if (ret != 0) return -1;
        if (ndec != FILE_CEK_SIZE) {
            free(dec);
            return -1;
        }
        memcpy(f->enc.cek.key, dec, FILE_CEK_SIZE);
        free(dec);
        f->enc.cek.ready = true;
        *cek = f->enc.cek.key;
        return 0;
    }
    return 0;
}
