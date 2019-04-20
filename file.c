#include <common.h>

int file_chunks(const char *bytes, size_t nbytes,
                struct file_chunk_s **chunks, int *nchunks)
{
    if (nbytes < 1) return -1;

    int i;
    *chunks  = NULL;
    *nchunks = 0;
    for (i = 0; (i * CHUNK_SIZE) < nbytes; i++) {
        *chunks = realloc(*chunks, sizeof(**chunks) * (i + 1));
        if (*chunks == NULL) return -1;
        struct file_chunk_s *fc = &((*chunks)[i]);
        fc->ptr  = (const unsigned char *)(bytes + (i * CHUNK_SIZE));
        fc->size = (nbytes < CHUNK_SIZE) ? nbytes :
                   (((i + 1) * CHUNK_SIZE) > nbytes ?
                   nbytes - (i * CHUNK_SIZE) :
                   CHUNK_SIZE);
        fc->part = i;
        sha256hex(fc->ptr, fc->size, fc->part_hash);

        char buffer[1024];
        snprintf(buffer, sizeof(buffer), "%ld,%d,%.*s",
                 fc->size, fc->part,
                 (int)sizeof(fc->part_hash), fc->part_hash);
        sha256hex((unsigned char *)buffer, strlen(buffer), fc->chunk_hash);
        (*nchunks)++;
    }

    return 0;
}

void file_chunks_free(struct file_chunk_s *chunks)
{
    free(chunks);
}
