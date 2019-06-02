#ifndef FILE_H_
#define FILE_H_

#define FILE_READONLY    FLAG(0) // read-only
#define FILE_PRIVATE_KEY FLAG(1) // this file is used to decode other file
#define FILE_DELETED     FLAG(2) // considered to be deleted
#define FILE_ENCRYPTED   FLAG(3) // file chunks are encrypted

#define CHUNK_SIZE (500 * 1024) // 512kB

struct file_chunk_s {
    size_t size;
    int    part;
    struct {
        unsigned char content[SHA256HEX];
        unsigned char chunk[SHA256HEX];
    } hash;
};

struct file_s {
    unsigned char hash[SHA256HEX];
    struct {
        char          name[128];
        size_t        size;
        char          description[1024];
        char          type[32];
        unsigned char hash[SHA256HEX];
    } meta;
    struct {
        unsigned char hash[SHA256HEX];
    } content;
    struct {
        struct file_chunk_s *array;
        size_t               size;
        unsigned char        hash[SHA256HEX];
    } chunks;
    struct {
        char public_key[1024];
    } owner;
    struct {
        char public_key[1024];
        char private_key[1024];
    } encrypted;

    unsigned int flags;
};

int file_chunks_alloc(struct file_s *f, const int size);
int file_chunks(const char *filename, size_t nbytes,
                struct file_chunk_s **chunks,
                size_t *nchunks);
void file_chunks_free(struct file_chunk_s *chunks);

#endif
