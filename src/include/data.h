#ifndef DATA_H_
#define DATA_H_

#define DATA_SIZE 0 //sizeof(int)
#define DATA_SIZE_INT   (DATA_SIZE + sizeof(int))
#define DATA_SIZE_SHORT (DATA_SIZE + sizeof(short))

struct data_s {
    enum command_e command;
    size_t         size;
    sn             payload;
};

struct tracker_s;

struct module_data_s {
    int (*init)(struct data_s *d, enum command_e cmd,
                int (*callback)(struct data_s*, void*),
                int (*callback_size)(struct data_s*, void*),
                void *userdata);
    int (*send)(struct data_s *d, int sd, struct sockaddr_in *addr,
                int addr_len, int index, int host, unsigned short port,
                struct nb_s **nb, int *nnb);
    int (*prepare)(struct data_s *d, char *buffer, int nbuffer,
                   size_t ds);
    int (*size)(struct data_s *d, size_t *sz);
    int (*get)(struct packet_s *p, char *buffer, int nbuffer);
    struct {
        int (*integer)(struct data_s *d, const int src);
        int (*shortint)(struct data_s *d, const short src);
        int (*raw)(struct data_s *d, char *src, const int nsrc);
    } write;
};

extern const struct module_data_s data;

#endif
