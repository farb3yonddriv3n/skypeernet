#ifndef WORLD_H_
#define WORLD_H_

struct world_peer_s {
    int            host;
    unsigned short port;
};

struct module_world_s {
    int (*parse)(struct peer_s *p);
    struct {
        int (*read)(struct peer_s *p, struct world_peer_s *wp, char *buffer, int nbuffer);
        int (*add)(struct world_peer_s *wp);
    } peer;
};

extern const struct module_world_s world;

#endif
