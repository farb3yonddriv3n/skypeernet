#ifndef PAYLOAD_H_
#define PAYLOAD_H_

struct module_handler_s {
    int (*write)(struct data_s *d, void *userdata);
    int (*size)(struct data_s *d, void *userdata);
};

struct module_payload_s {
    struct {
        int (*peer)(struct peer_s *p, enum command_e cmd,
                    int host, unsigned short port);
        int (*tracker)(struct tracker_s *p, enum command_e cmd,
                       int host, unsigned short port);
    } send;
};

extern const struct module_payload_s payload;

int announce_write_peer(struct data_s *d, void *userdata);
int announce_write_tracker(struct data_s *d, void *userdata);
int announce_size(int *sz, void *userdata);

int message_write(struct data_s *d, void *userdata);
int message_size(int *sz, void *userdata);

int ack_write_peer(struct data_s *d, void *userdata);
int ack_write_tracker(struct data_s *d, void *userdata);
int ack_size(int *sz, void *userdata);

#endif
