#ifndef PEER_H_
#define PEER_H_

struct peer_s {
    struct net_s       net;
    struct net_ev_s    ev;
    struct net_send_s  send;
    struct net_recv_s  recv;
    struct {
        int            host;
        unsigned short port;
    } tracker;
};

#endif
