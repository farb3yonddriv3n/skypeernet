#ifndef TUNNEL_H_
#define TUNNEL_H_

struct tunnel_s {
    struct peer_s *peer;
    struct {
        int            host;
        unsigned short port;
        unsigned char  pubkeyhash[SHA256HEX];
    } remote;
    struct {
        unsigned short src;
        unsigned short dst;
    } tcp;
    struct gc_gen_server_s *server;
};

struct module_tunnel_s {
    int (*open)(struct peer_s *p, unsigned char *pubkeyhash,
                unsigned short *port_local,
                unsigned short dstport);
    int (*response)(struct peer_s *p, struct header_s *h, char *buf, int len);
    int (*dump)(struct peer_s *p, json_object **obj);
};

extern const struct module_tunnel_s tunnel;

#endif
