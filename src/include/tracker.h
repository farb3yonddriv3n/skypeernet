#ifndef TRACKER_H_
#define TRACKER_H_

#define TRACKER_PORT        5775
#define TRACKER_HOST        "192.168.88.12"

enum instance_e {
    INSTANCE_TRACKER,
    INSTANCE_PEER,
};

enum buffer_e {
    BUFFER_MESSAGE,
};

struct send_buffer_s {
    unsigned int pidx;
    unsigned int gidx;
    enum buffer_e type;
    union {
        struct {
            const char *str;
        } message;
    } u;
};

struct cache_s {
    int            group;
    int            total;
    int            host;
    unsigned short port;
    struct {
        int *idx;
        int  size;
    } received;
};

struct recv_buffer_s {
    struct list_s packets;
    struct list_s cache;
    sn available;
};

struct instance_s {
    enum instance_e    type;
    struct net_s       net;
    struct net_ev_s    ev;
    struct net_send_s  send;
    struct net_recv_s  recv;
    struct packet_s    received;
    struct list_s      peers;
    struct send_buffer_s send_buffer;
    struct recv_buffer_s recv_buffer;
};

struct tracker_s {
    enum instance_e      type;
    struct net_s         net;
    struct net_ev_s      ev;
    struct net_send_s    send;
    struct net_recv_s    recv;
    struct packet_s      received;
    struct list_s        peers;
    struct send_buffer_s send_buffer;
    struct recv_buffer_s recv_buffer;
};

#endif
