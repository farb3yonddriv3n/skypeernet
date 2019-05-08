#ifndef NET_H_
#define NET_H_

#define NET_IP(m_dst) m_dst.sin_addr.s_addr
#define NET_PORT(m_dst) m_dst.sin_port

struct nb_s {
    int sd;
    snb buffer;
    struct {
        struct sockaddr_in addr;
        socklen_t          len;
    } remote;
};

struct net_send_s {
    struct nb_s *data;
    int          len;
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
};

int net_recv(int sd, char *data, int len,
             struct sockaddr_in *addr, socklen_t *naddr);
int net_send(struct nb_s **nb, int *nnb);

#endif
