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
        char *fpart;
        size_t nfpart;
        if (os.filepart(filename, i * CHUNK_SIZE, fc->size, &fpart,
                        &nfpart) != 0) return -1;
        sha256hex((unsigned char *)fpart, nfpart, fc->hash.content);
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
