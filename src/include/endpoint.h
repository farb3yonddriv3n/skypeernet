#ifndef ENDPOINT_H_
#define ENDPOINT_H_

struct endpoint_s {
    struct {
        int            host;
        unsigned short port;
    } remote;
    struct {
        int            cidx;
        unsigned short src;
        unsigned short dst;
    } tcp;
    struct gc_gen_client_s client;
    struct endpoint_s      *found;
    struct peer_s          *peer;
};

struct module_endpoint_s {
    int (*request)(struct peer_s *p, struct header_s *header, int host,
                   unsigned short port, char *data, int ndata);
};

extern struct module_endpoint_s endpoint;

#endif
