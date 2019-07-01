#ifndef API_H_
#define API_H_

enum api_e {
    API_LISTPEERS,
    API_MESSAGE,
    API_LISTFILES,
    API_PEER_ONLINE,
    API_PEER_OFFLINE,
};

struct module_api_s {
    int (*write)(struct peer_s *p, enum api_e cmd, json_object *payload);
    int (*read)();
    //int (*clean)();
    struct {
        void (*read)(EV_P_ ev_io *w, int revents);
        void (*write)(EV_P_ ev_io *w, int revents);
    } ev;
};

int api_message_write(struct peer_s *p, int host, unsigned short port,
                      char *msg, int len);
int api_peer_online(struct peer_s *p, struct world_peer_s *wp);
int api_peer_offline(struct peer_s *p, struct world_peer_s *wp);

extern const struct module_api_s api;

#endif
