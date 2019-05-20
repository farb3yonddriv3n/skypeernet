#ifndef WORLD_H_
#define WORLD_H_

struct world_peer_s {
    int                  host;
    unsigned short       port;
    struct world_peer_s *found;
};

struct module_world_s {
    int (*handle)(struct peer_s *ins);
    struct {
        int (*del)(struct peer_s *p, int host, unsigned short port);
    } peer;
};

extern const struct module_world_s world;

#endif
