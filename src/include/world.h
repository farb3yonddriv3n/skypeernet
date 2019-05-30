#ifndef WORLD_H_
#define WORLD_H_

enum world_peer_e {
    WORLD_PEER_NONE,
    WORLD_PEER_TRACKER,
    WORLD_PEER_PEER,
};

struct world_peer_s {
    enum world_peer_e    type;
    int                  host;
    unsigned short       port;
    struct world_peer_s *found;
    unsigned int         unreachable;
    sn                   key;
};

struct module_world_s {
    struct {
        int (*reachable)(struct peer_s *p, int host, unsigned short port);
        int (*unreachable)(struct peer_s *p, int host, unsigned short port);
        void (*check)(struct ev_loop *loop, struct ev_timer *timer, int revents);
    } peer;
};

extern const struct module_world_s world;

#endif
