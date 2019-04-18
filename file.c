#include <common.h>

int file_chunks(const char *bytes, const int nbytes,
                struct file_chunk_s **chunks, int *nchunks)
{
    if (nbytes < 1) return -1;

    int i;
    *chunks  = NULL;
    *nchunks = 0;
    for (i = 0; (i * CHUNK_SIZE) < nbytes; i++) {
        *chunks = realloc(*chunks, sizeof(**chunks) * (i + 1));
        if (*chunks == NULL) return -1;
        (*chunks)[i].ptr   = (const unsigned char *)(bytes + (i * CHUNK_SIZE));
        (*chunks)[i].n     = (nbytes < CHUNK_SIZE) ? nbytes :
                             (((i + 1) * CHUNK_SIZE) > nbytes ?
                              nbytes - (i * CHUNK_SIZE) :
                              CHUNK_SIZE);
        (*chunks)[i].chunk = i;
        SHA256((*chunks)[i].ptr, (*chunks)[i].n, (*chunks)[i].hash);
        (*nchunks)++;
    }

    return 0;
}

void file_chunks_free(struct file_chunk_s *chunks)
{
    free(chunks);
}
