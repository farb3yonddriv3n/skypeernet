#ifndef DATA_H_
#define DATA_H_

enum data_e {
    DATA_PEER_ANNOUNCE,
};

struct data_s {
    int command;
    sn  payload;
};

struct tracker_s;

struct module_data_s {
    int (*init)(struct data_s *d, enum data_e cmd,
                int (*callback)(struct data_s*, void*),
                void *userdata, size_t sz);
    int (*send)(struct data_s *d, int sd, struct sockaddr_in *addr,
                int addr_len, int index, int host, unsigned short port,
                struct nb_s **nb, int *nnb);
    int (*prepare)(struct data_s *d, char *buffer, int nbuffer,
                   size_t ds);
    int (*size)(struct data_s *d, size_t *sz);
    struct {
        int (*integer)(struct data_s *d, const int src);
        int (*shortint)(struct data_s *d, const short src);
    } write;
};

extern const struct module_data_s data;

#endif
