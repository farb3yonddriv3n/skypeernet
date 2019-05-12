#ifndef DATA_H_
#define DATA_H_

#define DATA_SIZE_INT   (sizeof(int))
#define DATA_SIZE_SHORT (sizeof(short))

struct data_s {
    enum command_e command;
    sn             payload;
};

struct tracker_s;

struct module_data_s {
    int (*init)(struct data_s *d, enum command_e cmd,
                int (*callback)(struct data_s*, void*),
                int (*callback_size)(int*, void*),
                void *userdata);
    int (*send)(struct data_s *d, int sd, struct sockaddr_in *addr,
                int addr_len, int host, unsigned short port,
                struct list_s *nbl, struct net_ev_s *ne);
    int (*get)(struct packet_s *p, char *buffer, int nbuffer);
    int (*size)(struct data_s *d, size_t *sz);
    struct {
        int (*integer)(struct data_s *d, const int src);
        int (*shortint)(struct data_s *d, const short src);
        int (*raw)(struct data_s *d, char *src, const int nsrc);
    } write;
};

extern const struct module_data_s data;

#endif
