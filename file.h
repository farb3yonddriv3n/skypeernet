#ifndef FILE_H_
#define FILE_H_

#define FILE_READONLY    FLAG(0) // read-only
#define FILE_PRIVATE_KEY FLAG(1) // this file is used to decode other file
#define FILE_DELETED     FLAG(2) // considered to be deleted
#define FILE_ENCRYPTED   FLAG(3) // file chunks are encrypted

#define CHUNK_SIZE (512 * 10) // 512kB

struct file_chunk_s {
    const unsigned char *ptr;
    int                  n;
    int                  chunk;
    unsigned char        hash[SHA256_DIGEST_LENGTH];
};

struct file_s {
    struct {
        char                *name;
        uint64_t             size;
        char                *description;
        char                *type;
        struct file_chunk_s *chunks;
    } meta;
    struct {
        char public_key[1024];
    } owner;
    struct {
        char public_key[1024];
        char private_key[1024];
    } encrypted;

    unsigned int flags;
};

int file_chunks(const char *bytes, const int nbytes,
                struct file_chunk_s **chunks, int *nchunks);
void file_chunks_free(struct file_chunk_s *chunks);

#endif
