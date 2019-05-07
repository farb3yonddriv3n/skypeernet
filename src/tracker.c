#include <common.h>

static int peer_exists(struct tracker_peer_s *list, struct tracker_peer_s *p,
                       bool *exists)
{
    struct tracker_peer_s *pt;
    for (pt = list; pt != NULL; pt = pt->next) {
        if (pt->host == p->host &&
            pt->port == p->port) {
            *exists = true;
            return 0;
        }
    }
    *exists = false;
    return 0;
}

static int peer_add(struct tracker_s *t, struct tracker_peer_s *p)
{
    if (!t || !p) return -1;
    struct tracker_peer_s *peer_new = malloc(sizeof(*peer_new));
    if (!peer_new) return -1;
    memcpy(peer_new, p, sizeof(*p));
    peer_new->prev = NULL;
    peer_new->next = t->peers.list;
    if (t->peers.list) t->peers.list->prev = peer_new;
    t->peers.list  = peer_new;
    t->peers.count++;
    return 0;
}

static int announce_cb(struct data_s *d, void *userdata)
{
    struct tracker_peer_s *new = (struct tracker_peer_s *)userdata;
    if (data.write.integer(d, new->host) != 0) return -1;
    if (data.write.shortint(d, new->port) != 0) return -1;
    return 0;
}

static int announce(struct tracker_s *t)
{
    struct tracker_peer_s *l, *n;
    for (n = t->peers.list; n != NULL; n = n->next) {
        if (!(n->flags & PEER_NEW)) continue;
        struct data_s d;
        if (data.init(&d, DATA_PEER_ANNOUNCE, announce_cb,
                      (void *)n, sizeof(n->host) + sizeof(n->port)) != 0)
            return -1;
        for (l = t->peers.list; l != NULL; l = l->next) {
            if (data.send(&d, t->net.sd, &t->net.remote.addr, t->net.remote.len, 0,
                          l->host, l->port, &t->send.data, &t->send.len) != 0)
                return -1;
        }
    }
    ev_io_start(t->ev.loop, &t->ev.write);
    return 0;
}

static void peer_update(struct tracker_s *t, int host, unsigned short port)
{
    bool exists;
    struct tracker_peer_s p = { .host = host, .port = port, .flags = PEER_NEW };
    if (peer_exists(t->peers.list, (void *)&p, &exists) != 0) return;
    if (exists) return;
    if (peer_add(t, &p) != 0) return;
    announce(t);
}

static void write_cb(EV_P_ ev_io *w, int revents)
{
    struct tracker_s *t = w->data;
    net_send(&t->send.data, &t->send.len);
    ev_io_stop(t->ev.loop, &t->ev.write);
}

static void read_cb(EV_P_ ev_io *w, int revents)
{
    struct tracker_s *t = w->data;
    if (!t) return;

    net_recv(t->net.sd, t->recv.data, sizeof(t->recv.data),
             &t->net.remote.addr, &t->net.remote.len);
    bool valid;
    if (packet.deserialize.init(t->recv.data, sizeof(t->recv.data),
                                &valid) != 0) return;
    if (!valid) return;
    peer_update(t, t->net.remote.addr.sin_addr.s_addr,
                t->net.remote.addr.sin_port);
}

static int init(struct tracker_s *t)
{
    memset(t, 0, sizeof(*t));
    t->net.sd = socket(PF_INET, SOCK_DGRAM, 0);
    t->net.self.len = sizeof(t->net.self.addr);
    t->net.self.addr.sin_family = AF_INET;
    t->net.self.addr.sin_port = htons(TRACKER_PORT);
    t->net.self.addr.sin_addr.s_addr = INADDR_ANY;
    if (bind(t->net.sd, (struct sockaddr *)&t->net.self.addr,
             sizeof(t->net.self.addr)) != 0)
        return -1;

    t->ev.loop = ev_default_loop(0);
    ev_io_init(&t->ev.read,  read_cb,  t->net.sd, EV_READ);
    ev_io_init(&t->ev.write, write_cb, t->net.sd, EV_WRITE);
    t->ev.read.data  = t;
    t->ev.write.data = t;

    return 0;
}

int main()
{
    struct tracker_s t;
    if (init(&t) != 0) return -1;
    ev_io_start(t.ev.loop, &t.ev.read);
    ev_loop(t.ev.loop, 0);
    close(t.net.sd);
    return 0;
}
