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
    int (*send)(struct data_s *d, struct tracker_s *t,
                int host, unsigned short port);
    int (*prepare)(struct data_s *d, char *buffer, int nbuffer,
                   size_t ds);
    int (*size)(struct data_s *d, size_t *sz);
    struct {
        int (*integer)(struct data_s *d, const int src);
    } write;
};

extern const struct module_data_s data;

#endif
