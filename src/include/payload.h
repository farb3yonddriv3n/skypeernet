#ifndef PAYLOAD_H_
#define PAYLOAD_H_

#define MAX_ANNOUNCED_PORTS 8 /* Prevent (un)intentional ports floding */

struct module_handler_s {
    int (*write)(struct data_s *d, void *userdata);
    int (*size)(struct data_s *d, void *userdata);
};

struct module_payload_s {
    int (*send)(void *parent, enum command_e cmd,
                int host, unsigned short port,
                unsigned int tidx, unsigned int parts,
                unsigned char *filename,
                struct tcp_s *tcp);
    int (*recv)(struct peer_s *p);
};

int announce_peer_write_peer(struct data_s *d, void *userdata);
int announce_twp(struct data_s *d, void *userdata);
int announce_twt(struct data_s *d, void *userdata);
int announce_pwp(struct data_s *d, void *userdata);
int announce_size(int *sz, void *userdata);
int announce_trt(struct peer_s *p);
int announce_trp(struct peer_s *p);
int announce_prp(struct peer_s *p);

int message_write(struct data_s *d, void *userdata);
int message_size(int *sz, void *userdata);
int message_read(struct peer_s *p);

int ack_write(struct data_s *d, void *userdata);
int ack_size(int *sz, void *userdata);
int ack_read(struct peer_s *ins);
int ack_reply(struct peer_s *ins);

int file_write(struct data_s *d, void *userdata);
int file_size(int *sz, void *userdata);
int file_read(struct peer_s *p);

int fileask_write(struct data_s *d, void *userdata);
int fileask_read(struct peer_s *p);
int fileask_size(int *sz, void *userdata);

int ping_write(struct data_s *d, void *userdata);
int ping_size(int *sz, void *userdata);
int ping_read(struct peer_s *p);

int auth_write(struct data_s *d, void *userdata);
int auth_read(struct peer_s *p);
int auth_size(int *sz, void *userdata);

int authrpl_write(struct data_s *d, void *userdata);
int authrpl_read(struct peer_s *p);
int authrpl_size(int *sz, void *userdata);

extern const struct module_payload_s payload;

#endif
