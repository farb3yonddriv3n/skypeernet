#ifndef TRACKER_H_
#define TRACKER_H_

#define TRACKER_PORT        5775
#define TRACKER_HOST        "192.168.88.14"

#define PEER_NEW (1 << 0)

struct tracker_peer_s {
    int            host;
    unsigned short port;
    unsigned int   flags;
    struct tracker_peer_s *prev;
    struct tracker_peer_s *next;
};

struct tracker_s {
    struct net_s       net;
    struct net_ev_s    ev;
    struct net_send_s  send;
    struct net_recv_s  recv;
    struct {
        struct tracker_peer_s *list;
        int                    count;
    } peers;
    struct packet_s    received;
};

#endif
