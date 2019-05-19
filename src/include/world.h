#ifndef WORLD_H_
#define WORLD_H_

struct world_peer_s {
    int            host;
    unsigned short port;
    bool           found;
};

struct module_world_s {
    int (*handle)(struct peer_s *ins);
};

extern const struct module_world_s world;

#endif
