#ifndef FILE_H_
#define FILE_H_

#define FILE_READONLY    FLAG(0) // read-only
#define FILE_PRIVATE_KEY FLAG(1) // this file is used to decode other file
#define FILE_DELETED     FLAG(2) // considered to be deleted
#define FILE_ENCRYPTED   FLAG(3) // file chunks are encrypted

struct file_chunk_s {
    size_t size;
    int    part;
    char   tag[512];
    char   timeiter[32];
    struct {
        unsigned char content[SHA256HEX];
        unsigned char chunk[SHA256HEX];
    } hash;
};

enum {
    DESC_NONE    = 0,
    DESC_TRYDEC  = 1,
    DESC_DECODED = 2,
};

struct file_s {
    unsigned char hash[SHA256HEX];
    unsigned char pubkeyhash[SHA256HEX];
    struct {
        char          name[128];
        size_t        size;
        struct {
            char      enc[1024];
            char      dec[1024];
            int       flag;
        } description;
        bool          finalized;
        char          tags[1024];
        unsigned char hash[SHA256HEX];
    } meta;
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

    struct transaction_s *parent;
    unsigned int flags;
};

int file_chunks_alloc(struct file_s *f, const int size);
int file_chunks(const char *filename, size_t nbytes,
                struct file_chunk_s **chunks,
                size_t *nchunks);
void file_chunks_free(struct file_chunk_s *chunks);

#endif
