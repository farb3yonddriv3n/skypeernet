#include <common.h>

static int announce_cb(struct data_s *d, void *userdata)
{
    struct peer_s *new = (struct peer_s *)userdata;
    if (data.write.integer(d, new->net.self.addr.sin_addr.s_addr) != 0) return -1;
    if (data.write.integer(d, new->net.self.addr.sin_port)        != 0) return -1;
    return 0;
}

static void write_cb(EV_P_ ev_io *w, int revents)
{
    struct peer_s *p = w->data;
    net_send(&p->send.data, &p->send.len);
    ev_io_stop(p->ev.loop, &p->ev.write);
}

static void read_cb(EV_P_ ev_io *w, int revents)
{
    struct peer_s *p = w->data;
    socklen_t bytes = recvfrom(p->net.sd, p->recv.data, sizeof(p->recv.data), 0,
                               (struct sockaddr *)&p->net.remote.addr,
                               &p->net.remote.len);
    if (bytes == -1) abort();
    bool valid;
    packet.validate(p->recv.data, sizeof(p->recv.data), &valid);
}

static int announce(struct peer_s *p)
{
    struct data_s d;
    if (data.init(&d, DATA_PEER_ANNOUNCE, announce_cb,
                  (void *)p, sizeof(int) * 2) != 0)
        return -1;
    if (data.send(&d, p->net.sd, &p->net.remote.addr,
                  p->net.remote.len, 0,
                  p->net.remote.addr.sin_addr.s_addr,
                  p->net.remote.addr.sin_port,
                  &p->send.data, &p->send.len) != 0) return -1;
    ev_io_start(p->ev.loop, &p->ev.write);
    return 0;
}

static int init(struct peer_s *p)
{
    memset(p, 0, sizeof(*p));
    if ((p->net.sd = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP)) == -1)
        return -1;

    p->net.self.addr.sin_family      = AF_INET;
    p->net.self.addr.sin_addr.s_addr = htonl(INADDR_ANY);
    p->net.self.len                  = sizeof(p->net.self.addr);

    p->net.remote.addr.sin_family    = AF_INET;
    p->net.remote.addr.sin_port      = htons(TRACKER_PORT);
    p->net.remote.len                = sizeof(p->net.remote.addr);
    if (inet_aton(TRACKER_HOST, &p->net.remote.addr.sin_addr) == 0)
        return -1;

    p->tracker.host = p->net.remote.addr.sin_addr.s_addr;
    p->tracker.port = p->net.remote.addr.sin_port;

    p->ev.loop = ev_default_loop(0);
    ev_io_init(&p->ev.read,  read_cb,  p->net.sd, EV_READ);
    ev_io_init(&p->ev.write, write_cb, p->net.sd, EV_WRITE);
    p->ev.read.data  = (void *)p;
    p->ev.write.data = (void *)p;

    return 0;
}

int main()
{
    struct peer_s p;
    if (init(&p) != 0) return -1;
    if (announce(&p) != 0) return -1;
    ev_io_start(p.ev.loop, &p.ev.read);
    ev_loop(p.ev.loop, 0);
    close(p.net.sd);
    return 0;
}
