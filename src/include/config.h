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
            double tracker_resend;
            double peer_resend;
            double peers_reachable;
        } interval;
        struct {
            int task_buffer;
            int send_retry;
        } max;
    } net;
};

int config_init(struct config_s *cfg);
void config_free(struct config_s *cfg);

#endif
