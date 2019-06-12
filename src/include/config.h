#ifndef CONFIG_H_
#define CONFIG_H_

struct config_key_s {
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
};

struct config_s {
    struct {
        struct config_key_s local;
        struct {
            struct config_key_s *array;
            int                  size;
        } shared;
        struct config_key_s *active;
    } keys;
    struct {
        struct {
            char ip[32];
            unsigned short port;
        } tracker;
        struct {
            double retry;
            double peers_reachable;
            int    resend;
        } interval;
        struct {
            int task_buffer;
            int send_queue;
            int send_retry;
            int upload;
            int download;
            int peer_unreachable;
        } max;
    } net;
    struct {
        char download[128];
        char block[128];
        char keys[128];
    } dir;
};

int config_init(struct config_s *cfg);
void config_free(struct config_s *cfg);

#endif
