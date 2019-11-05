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
    sn                   pubkey;
    unsigned char        pubkeyhash[SHA256HEX];
    unsigned char        authstr[SHA256HEX];
    bool                 authed;
    int                  version;
    struct {
        char description[256];
        struct list_s ports;
    } tcp;
};

struct module_world_s {
    struct {
        int (*reachable)(struct peer_s *p, int host, unsigned short port);
        int (*unreachable)(struct peer_s *p, int host, unsigned short port);
        void (*check)(struct ev_loop *loop, struct ev_timer *timer, int revents);
        int (*isreachable)(struct peer_s *p, int host, unsigned short port,
                           bool *reachable);
        int (*findpubkeyhash)(struct list_s *l, void *existing, void *uwp);
        int (*findauthstr)(struct list_s *l, void *existing, void *uwp);
        int (*find)(struct list_s *l, void *existing, void *uwp);
        int (*broadcast)(struct peer_s *p, struct world_peer_s *wp);
        int (*auth)(struct peer_s *p, struct world_peer_s *wp);
        int (*add)(struct peer_s *p, struct world_peer_s *wp,
                   bool *added);
    } peer;
};

extern const struct module_world_s world;

#endif
