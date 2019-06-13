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
                size_t *nchunks)
{
    if (nbytes < 1) return -1;

    int i;
    *chunks  = NULL;
    *nchunks = 0;
    for (i = 0; (i * CHUNK_SIZE) < nbytes; i++) {
        *chunks = realloc(*chunks, sizeof(**chunks) * (i + 1));
        if (*chunks == NULL) return -1;
        struct file_chunk_s *fc = &((*chunks)[i]);
        fc->size = (nbytes < CHUNK_SIZE) ? nbytes :
                   (((i + 1) * CHUNK_SIZE) > nbytes ?
                   nbytes - (i * CHUNK_SIZE) :
                   CHUNK_SIZE);
        fc->part = i;
        unsigned char *fpart;
        size_t         nfpart;
        if (os.filepart(filename, i * CHUNK_SIZE, fc->size, (char **)&fpart,
                        &nfpart) != 0) return -1;
        unsigned char fpartenc[CHUNK_SIZE];
        unsigned char tag[AES_TAG_SIZE];
        int nfpartenc = aes_encrypt(fpart, nfpart, psig->cfg.keys.local.aes.key,
                                    sizeof(psig->cfg.keys.local.aes.key),
                                    psig->cfg.keys.local.aes.key,
                                    psig->cfg.keys.local.aes.key,
                                    fpartenc, tag);
        if (nfpartenc < 1) return -1;
        fc->size = nfpartenc;
        unsigned char *tagenc;
        int            ntagenc;
        ifr(rsa_encrypt(psig->cfg.keys.local.rsa.public, (unsigned char *)tag,
                        sizeof(tag), &tagenc, &ntagenc));
        size_t nencoded;
        unsigned char *encoded = base64_encode(tagenc, ntagenc, &nencoded);
        if (!encoded) return -1;
        snprintf(fc->tag, sizeof(fc->tag), "%.*s", (int)nencoded, encoded);
        sha256hex(fpartenc, nfpartenc, fc->hash.content);
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
