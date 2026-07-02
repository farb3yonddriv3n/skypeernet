#ifndef FILE_H_
#define FILE_H_

#define FILE_READONLY    FLAG(0) // read-only
#define FILE_PRIVATE_KEY FLAG(1) // this file is used to decode other file
#define FILE_DELETED     FLAG(2) // considered to be deleted
#define FILE_ENCRYPTED   FLAG(3) // file chunks are encrypted

#define FILE_CEK_SIZE 32

/**
 * @brief File encryption scheme.
 *
 * PLAIN  - chunks stored/transferred unencrypted, anyone can finalize
 * LEGACY - AES key derived from the sharer's RSA private key; the
 *          downloader must import that private key (pre-CEK blocks)
 * CEK    - random per-file AES-256 content key, wrapped per recipient
 *          with the recipient's RSA public key (key slots)
 */
enum file_scheme_e {
    FILE_SCHEME_PLAIN  = 0,
    FILE_SCHEME_LEGACY = 1,
    FILE_SCHEME_CEK    = 2,
};

/**
 * @brief One CEK grant: the content key wrapped for one recipient.
 */
struct file_key_slot_s {
    unsigned char to[SHA256HEX]; /**< Recipient's public key hash (hex) */
    char          key[1024];     /**< base64(RSA-OAEP(recipient_pub, CEK)) */
};

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
        enum file_scheme_e scheme;
        struct {
            struct file_key_slot_s *array;
            int                     size;
        } slots;
        struct {
            bool          ready;                /**< CEK resolved/cached */
            unsigned char key[FILE_CEK_SIZE];
        } cek;
    } enc;

    struct transaction_s *parent;
    unsigned int flags;
};

int file_chunks_alloc(struct file_s *f, const int size);
int file_chunks(const char *filename, size_t nbytes,
                struct file_chunk_s **chunks,
                size_t *nchunks, unsigned char *cek);
void file_chunks_free(struct file_chunk_s *chunks);
int file_cek_resolve(struct file_s *f, unsigned char **cek);

#endif
