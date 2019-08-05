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
        unsigned char *fpartenc = malloc(fc->size);
        if (!fpartenc) return -1;
        unsigned char tag[AES_TAG_SIZE];
        int nfpartenc = aes_encrypt(fpart, nfpart, psig->cfg.keys.local.aes.key,
                                    sizeof(psig->cfg.keys.local.aes.key),
                                    psig->cfg.keys.local.aes.key,
                                    psig->cfg.keys.local.aes.key,
                                    fpartenc, tag);
        if (nfpartenc < 1) return -1;
        fc->size = nfpartenc;
        size_t         nencoded;
        unsigned char *encoded = NULL;
        ifr(encx(&encoded, &nencoded, tag, sizeof(tag), &nfpartenc));
        if (!encoded) return -1;
        snprintf(fc->tag, sizeof(fc->tag), "%.*s", (int)nencoded, encoded);
        free(encoded);
        sha256hex(fpartenc, nfpartenc, fc->hash.content);
        char cfilename[256];
        snprintf(cfilename, sizeof(cfilename), "%s/%.*s", psig->cfg.dir.download,
                                                          SHA256HEX, fc->hash.content);
        ifr(os.filewrite(cfilename, "wb", (char *)fpartenc, nfpartenc));
        free(fpart);
        free(fpartenc);
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
