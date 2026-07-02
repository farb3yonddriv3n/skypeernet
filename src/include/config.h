#ifndef CONFIG_H_
#define CONFIG_H_

/**
 * @brief Kind of key held in a config_key_s entry.
 */
enum config_key_e {
    CONFIG_KEY_LEGACY = 0, /**< RSA private key (own identity or legacy import) */
    CONFIG_KEY_PUBLIC,     /**< Peer's public key (.pub.pem, CEK recipient) */
    CONFIG_KEY_GROUP,      /**< Symmetric group key (.grp, sharedname = SHA256(key)) */
};

struct config_key_s {
    unsigned char      sharedname[SHA256HEX];
    enum config_key_e  type;
    struct {
        RSA *public;
        RSA *private;
    } rsa;
    struct {
        sn public;
        sn private;
    } str;
    struct {
        unsigned char public[SHA256HEX];
        unsigned char private[SHA256HEX];
    } hash;
    struct {
        unsigned char key[SHA256_DIGEST_LENGTH];
    } aes;
};

struct config_s {
    struct {
        struct config_key_s local;
        struct {
            struct config_key_s *array;
            int                  size;
        } shared;
    } keys;
    struct {
        struct {
            char ip[32];
            unsigned short port;
        } tracker;
        struct {
            double retry;
            double peers_reachable;
        } interval;
        struct {
            int task_buffer;
            int send_queue;
            int send_retry;
            int upload;
            int download;
            int peer_unreachable;
            int chunk_size;
        } max;
        int proxy;
        int sendfactor;
        bool *sendtable;
        unsigned short local_port;
    } net;
    struct {
        char download[128];
        char block[128];
        char keys[128];
        char finalized[128];
        char tcp[128];
    } dir;
    struct {
        struct {
            char read[256];
            char write[256];
        } pipes;
    } api;
    struct {
        struct list_s ports;
        char description[256];
    } tcp;
    int miningtarget;
};

int config_init(struct config_s *cfg);
void config_free(struct config_s *cfg);
int config_groupkey_add(struct config_s *cfg, unsigned char *gkhash,
                        unsigned char *gk);
int config_keysdump(struct config_s *cfg);
int config_keyexists(struct config_s *cfg, unsigned char *sharedname,
                     struct config_key_s **exists);

#endif
