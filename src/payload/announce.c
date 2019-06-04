#include <common.h>

int announce_pwp(struct data_s *d, void *userdata)
{
    struct peer_s *p = (struct peer_s *)userdata;
    if (!p || !d) return -1;
    if (data.write.integer(d, ADDR_IP(p->net.self.addr))            != 0) return -1;
    if (data.write.shortint(d, ADDR_PORT(p->net.self.addr))         != 0) return -1;
    if (data.write.raw(d, p->cfg.key.public.s, p->cfg.key.public.n) != 0) return -1;
    return 0;
}

int announce_twp(struct data_s *d, void *userdata)
{
    struct peer_s *t = (struct peer_s *)userdata;
    if (!t || !d) return -1;
    if (t->send_buffer.type != BUFFER_TRACKER_ANNOUNCE_PEER) return -1;
    if (data.write.integer(d, t->send_buffer.u.tracker_peer.host)  != 0) return -1;
    if (data.write.shortint(d, t->send_buffer.u.tracker_peer.port) != 0) return -1;
    if (data.write.raw(d, (char *)t->send_buffer.u.tracker_peer.key->s,
                       t->send_buffer.u.tracker_peer.key->n) != 0) return -1;
    return 0;
}

int announce_twt(struct data_s *d, void *userdata)
{
    struct peer_s *t = (struct peer_s *)userdata;
    if (!t || !d) return -1;
    if (data.write.integer(d, ADDR_IP(t->net.remote.addr))    != 0) return -1;
    if (data.write.shortint(d, ADDR_PORT(t->net.remote.addr)) != 0) return -1;
    if (data.write.raw(d, t->cfg.key.public.s, t->cfg.key.public.n) != 0) return -1;
    return 0;
}

static int announce_read(struct world_peer_s *wp, char *src, int nsrc)
{
    sn_initr(bf, src, nsrc);
    if (sn_read((void *)&wp->host, sizeof(wp->host), &bf) != 0) return -1;
    if (sn_read((void *)&wp->port, sizeof(wp->port), &bf) != 0) return -1;
    int keysize = nsrc - sizeof(wp->host) - sizeof(wp->port);
    sn_bytes_init_new(wp->key, keysize);
    if (sn_read((void *)wp->key.s, keysize, &bf) != 0) return -1;
    sha256hex((unsigned char *)wp->key.s, keysize, wp->keyhash);
    return 0;
}

int announce_size(int *sz, void *userdata)
{
    if (!sz || !userdata) return -1;
    struct peer_s *p = (struct peer_s *)userdata;
    *sz = DATA_SIZE_INT + DATA_SIZE_SHORT + p->cfg.key.public.n;
    return 0;
}

static int peer_broadcast(struct peer_s *p, struct world_peer_s *wp)
{
    if (!p || !wp) return -1;
    if (p->type != INSTANCE_TRACKER) return -1;
    int cb(struct list_s *l, void *existing, void *uwp) {
        struct world_peer_s *ex = (struct world_peer_s *)existing;
        struct world_peer_s *wp = (struct world_peer_s *)uwp;
        if (wp->host == ex->host &&
            wp->port == ex->port) {
            return 0;
        }
        int item(int dst, unsigned short dstport,
                 int src, unsigned short srcport,
                 sn *key) {
            //ADDR_IP(p->net.remote.addr)   = src;
            //ADDR_PORT(p->net.remote.addr) = srcport;
            p->send_buffer.type = BUFFER_TRACKER_ANNOUNCE_PEER;
            p->send_buffer.u.tracker_peer.key  = key;
            p->send_buffer.u.tracker_peer.host = src;
            p->send_buffer.u.tracker_peer.port = srcport;
            if (payload.send(p, COMMAND_TRACKER_ANNOUNCE_PEER,
                             dst, dstport, 0, 0) != 0) return -1;
            return 0;
        }
        ifr(item(wp->host, wp->port, ex->host, ex->port, &ex->key));
        ifr(item(ex->host, ex->port, wp->host, wp->port, &wp->key));
        return 0;
    }
    ifr(list.map(&p->peers, cb, wp));
    return 0;
}

int peer_find(struct list_s *l, void *existing, void *uwp) {
    struct world_peer_s *ex = (struct world_peer_s *)existing;
    struct world_peer_s *wp = (struct world_peer_s *)uwp;
    if (wp->host == ex->host &&
        wp->port == ex->port) {
        wp->found = ex;
        return 1;
    }
    return 0;
}

static int wp_clean(void *uwp)
{
    if (!uwp) return -1;
    struct world_peer_s *wp = (struct world_peer_s *)uwp;
    if (wp->key.s) free(wp->key.s);
    free(wp);
    return 0;
}

static int peer_add(struct peer_s *p, struct world_peer_s *wp,
                    bool *added)
{
    if (!p || !wp) return -1;
    wp->found = NULL;
    *added = false;
    ifr(list.map(&p->peers, peer_find, wp));
    if (wp->found) return wp_clean(wp);
    ifr(list.add(&p->peers, wp, wp_clean));
    *added = true;
    return 0;
}

int announce_trt(struct peer_s *p)
{
    struct world_peer_s *wp = malloc(sizeof(*wp));
    if (!wp) return -1;
    memset(wp, 0, sizeof(*wp));
    if (p->type != INSTANCE_PEER) return -1;
    ifr(announce_read(wp, p->recv_buffer.available->data.s,
                      p->recv_buffer.available->data.n));
    ADDR_IP(p->net.self.addr)   = wp->host;
    ADDR_PORT(p->net.self.addr) = wp->port;
    wp->type = WORLD_PEER_TRACKER;
    wp->host = ADDR_IP(p->net.remote.addr);
    wp->port = ADDR_PORT(p->net.remote.addr);
    bool added;
    ifr(peer_add(p, wp, &added));
    if (added && p->user.cb.online)
        ifr(p->user.cb.online(p, wp));
    return 0;
}

int announce_trp(struct peer_s *p)
{
    struct world_peer_s *wp = malloc(sizeof(*wp));
    if (!wp) return -1;
    memset(wp, 0, sizeof(*wp));
    if (p->type != INSTANCE_PEER) return -1;
    ifr(announce_read(wp, p->recv_buffer.available->data.s,
                      p->recv_buffer.available->data.n));
    wp->type = WORLD_PEER_PEER;
    bool added;
    ifr(peer_add(p, wp, &added));
    if (added && p->user.cb.online)
        ifr(p->user.cb.online(p, wp));
    return 0;
}

int announce_prp(struct peer_s *p)
{
    struct world_peer_s *wp = malloc(sizeof(*wp));
    if (!wp) return -1;
    memset(wp, 0, sizeof(*wp));
    if (p->type != INSTANCE_TRACKER) return -1;
    ifr(announce_read(wp, p->recv_buffer.available->data.s,
                      p->recv_buffer.available->data.n));
    wp->type = WORLD_PEER_PEER;
    wp->host = ADDR_IP(p->net.remote.addr);
    wp->port = ADDR_PORT(p->net.remote.addr);
    if (payload.send(p, COMMAND_TRACKER_ANNOUNCE_TRACKER,
                     wp->host, wp->port, 0, 0) != 0) return -1;
    bool added;
    ifr(peer_add(p, wp, &added));
    if (added) return peer_broadcast(p, wp);
    return 0;
}

/*
const struct module_handler_s announce = {
    .tracker.write = announce_write_tracker,
    .peer.write    = announce_write_peer,
    .size          = announce_size,
};
*/
