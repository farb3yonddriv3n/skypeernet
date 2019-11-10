#ifndef DATA_H_
#define DATA_H_

#define DATA_SIZE_DOUBLE (sizeof(double))
#define DATA_SIZE_INT    (sizeof(int))
#define DATA_SIZE_SHORT  (sizeof(short))
#define DATA_SIZE_BYTE   (sizeof(char))

struct data_s {
    enum command_e command;
    sn             payload;
};

struct module_data_s {
    int (*init)(struct data_s *d, enum command_e cmd,
                int (*callback)(struct data_s*, void*),
                int (*callback_size)(int*, void*),
                void *userdata);
    int (*send)(struct data_s *d, struct peer_s *ins,
                int host, unsigned short port,
                unsigned int tidx, unsigned int parts,
                unsigned char *filename, struct tcp_s *tcp);
    int (*get)(struct packet_s *p, char *buffer, int nbuffer);
    int (*size)(struct data_s *d, size_t *sz);
    int (*submit)(struct peer_s *p, struct packet_s *pck,
                  int host, unsigned short port);
    struct {
        int (*tdouble)(struct data_s *d, const double src);
        int (*integer)(struct data_s *d, const int src);
        int (*shortint)(struct data_s *d, const short src);
        int (*byte)(struct data_s *d, const char src);
        int (*raw)(struct data_s *d, char *src, const int nsrc);
    } write;
};

extern const struct module_data_s data;

#endif
