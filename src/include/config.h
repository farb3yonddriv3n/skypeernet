#ifndef CONFIG_H_
#define CONFIG_H_

struct config_key_s {
    unsigned char sharedname[SHA256HEX];
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
    } net;
    struct {
        char download[128];
        char block[128];
        char keys[128];
        char finalized[128];
    } dir;
    struct {
        struct {
            char read[256];
            char write[256];
        } pipes;
    } api;
};

int config_init(struct config_s *cfg);
void config_free(struct config_s *cfg);
int config_keysdump(struct config_s *cfg);
int config_keyexists(struct config_s *cfg, unsigned char *sharedname,
                     struct config_key_s **exists);

#endif
