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
int announce_size(struct data_s *d, void *userdata);

int message_write(struct data_s *d, void *userdata);
int message_size(struct data_s *d, void *userdata);

//extern const struct module_handler_s message;
//extern const struct module_handler_s announce;

#endif
