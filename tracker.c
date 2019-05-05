#include <common.h>

static int peer_exists(struct peer_s *list, struct peer_s *p,
                       bool *exists)
{
    struct peer_s *pt;
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

static int peer_add(struct tracker_s *t, struct peer_s *p)
{
    if (!t || !p) return -1;
    struct peer_s *peer_new = malloc(sizeof(*peer_new));
    if (!peer_new) return -1;
    memcpy(peer_new, p, sizeof(*p));
    peer_new->prev = NULL;
    peer_new->next = t->peers.list;
    if (t->peers.list) t->peers.list->prev = peer_new;
    t->peers.list  = peer_new;
    t->peers.count++;
    return 0;
}

static void peer_update(struct tracker_s *t, int host, unsigned short port)
{
    bool exists;
    struct peer_s p = { .host = host, .port = port, .flags = PEER_NEW };
    if (peer_exists(t->peers.list, (void *)&p, &exists) != 0) return;
    if (exists) return;
    if (peer_add(t, &p) != 0) return;
    ev_io_start(t->loop, &t->write);
}

static int announce_cb(struct data_s *d, void *userdata)
{
    struct peer_s *new = (struct peer_s *)userdata;
    if (data.write.integer(d, new->host) != 0) return -1;
    if (data.write.integer(d, new->port) != 0) return -1;
    return 0;
}

static int announce(struct tracker_s *t)
{
    struct peer_s *l, *n;
    for (n = t->peers.list; n != NULL; n = n->next) {
        if (!(n->flags & PEER_NEW)) continue;
        struct data_s d;
        if (data.init(&d, DATA_PEER_ANNOUNCE, announce_cb,
                      (void *)n, sizeof(n->host) + sizeof(n->port)) != 0)
            return -1;
        for (l = t->peers.list; l != NULL; l = l->next) {
            if (data.send(&d, t, l->host, l->port) != 0) return -1;
        }
    }

    return 0;
}

static void write_cb(EV_P_ ev_io *w, int revents)
{
    struct tracker_s *t = w->data;
    announce(t);
}

static void read_cb(EV_P_ ev_io *w, int revents)
{
    struct tracker_s *t = w->data;
    if (!t) return;
    // Invalidate only packet header
    memset(t->data, 0, sizeof(struct header_s));
    socklen_t bytes = recvfrom(t->sd, t->data, sizeof(t->data), 0,
                               (struct sockaddr *)&t->addr, (socklen_t *)&t->addr_len);

    bool valid;
    if (packet.deserialize.init(t->data, sizeof(t->data), &valid) != 0) return;
    if (!valid) return;

    peer_update(t, t->addr.sin_addr.s_addr, t->addr.sin_port);
}

#include <ev.h>

int tracker()
{
    int port = TRACKER_PORT;
    struct tracker_s t;
    memset(&t, 0, sizeof(t));
    t.sd = socket(PF_INET, SOCK_DGRAM, 0);
    t.addr_len = sizeof(t.addr);
    bzero(&t.addr, sizeof(t.addr));
    t.addr.sin_family = AF_INET;
    t.addr.sin_port = htons(port);
    t.addr.sin_addr.s_addr = INADDR_ANY;
    if (bind(t.sd, (struct sockaddr *)&t.addr, sizeof(t.addr)) != 0) {
        return -1;
    }
    t.loop = ev_default_loop(0);
    ev_io_init(&t.read, read_cb, t.sd, EV_READ);
    ev_io_init(&t.write, write_cb, t.sd, EV_WRITE);
    t.read.data = (void *)&t;
    t.write.data = (void *)&t;
    ev_io_start(t.loop, &t.read);
    ev_loop(t.loop, 0);
    close(t.sd);
    return 0;
}
