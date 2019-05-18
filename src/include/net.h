#ifndef NET_H_
#define NET_H_

#define ADDR_IP(m_dst) m_dst.sin_addr.s_addr
#define ADDR_PORT(m_dst) m_dst.sin_port

enum net_status_e {
    NET_INIT,
    NET_ACK_WAITING,
    NET_ONESHOT,
};

struct nb_s {
    struct peer_s *peer;
    int idx;
    int grp;
    int sd;
    snb buffer;
    struct {
        struct sockaddr_in addr;
        socklen_t          len;
    } remote;
    struct ev_io     *write;
    enum net_status_e status;
    int               attempt;
};

struct net_send_s {
    struct list_s nbl;
};

struct net_send_timer_s {
    struct nb_s     *nb;
    struct list_s   *nbl;
    struct net_ev_s *nev;
};

struct net_recv_s {
    char data[UDP_PACKET];
};

struct net_addr_s {
    struct sockaddr_in addr;
    socklen_t          len;
};

struct net_s {
    int               sd;
    int               pidx;
    struct net_addr_s self;
    struct net_addr_s remote;
    struct {
        int host;
        unsigned short port;
    } tracker;
};

struct net_ev_s {
    struct ev_loop *loop;
    struct ev_io    read;
    struct ev_io    write;
    struct ev_io    stdinwatch;
    struct ev_timer send;
};

struct module_net_s {
    int (*receive)(int sd, char *data, int len,
                   struct sockaddr_in *addr, socklen_t *naddr);
    int (*ack)(struct net_ev_s *ev, struct net_send_s *ns, int idx);
    int (*dispatch)(struct net_ev_s *ev, struct net_send_s *ns);
    void (*timeout)(struct ev_loop *loop, struct ev_timer *timer, int revents);

    struct {
        int (*clean)(void *ptr);
    } nb;
};

extern const struct module_net_s net;

#endif
