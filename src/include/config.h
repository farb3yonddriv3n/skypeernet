#ifndef CONFIG_H_
#define CONFIG_H_

struct config_s {
    struct {
        RSA *public;
        RSA *private;
    } rsakey;
    struct {
        sn public;
        sn private;
    } key;
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
    char download_dir[128];
    char block_dir[128];
};

int config_init(struct config_s *cfg);
void config_free(struct config_s *cfg);

#endif
