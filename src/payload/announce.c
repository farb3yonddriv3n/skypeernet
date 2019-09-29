#include <common.h>

int announce_pwp(struct data_s *d, void *userdata)
{
    struct peer_s *p = (struct peer_s *)userdata;
    if (!p || !d) return -1;
    if (data.write.integer(d, ADDR_IP(p->net.self.addr))            != 0) return -1;
    if (data.write.shortint(d, ADDR_PORT(p->net.self.addr))         != 0) return -1;
    if (data.write.raw(d, p->cfg.keys.local.str.public.s,
                       p->cfg.keys.local.str.public.n) != 0) return -1;
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
    if (data.write.raw(d, t->cfg.keys.local.str.public.s,
                       t->cfg.keys.local.str.public.n) != 0) return -1;
    return 0;
}

static int announce_read(struct world_peer_s *wp, char *src, int nsrc)
{
    sn_initr(bf, src, nsrc);
    if (sn_read((void *)&wp->host, sizeof(wp->host), &bf) != 0) return -1;
    if (sn_read((void *)&wp->port, sizeof(wp->port), &bf) != 0) return -1;
    int keysize = nsrc - sizeof(wp->host) - sizeof(wp->port);
    sn_bytes_init_new(wp->pubkey, keysize);
    if (sn_read((void *)wp->pubkey.s, keysize, &bf) != 0) return -1;
    sha256hex((unsigned char *)wp->pubkey.s, keysize, wp->pubkeyhash);
    return 0;
}

int announce_size(int *sz, void *userdata)
{
    if (!sz || !userdata) return -1;
    struct peer_s *p = (struct peer_s *)userdata;
    *sz = DATA_SIZE_INT + DATA_SIZE_SHORT + p->cfg.keys.local.str.public.n;
    return 0;
}

static int wp_clean(void *uwp)
{
    if (!uwp) return -1;
    struct world_peer_s *wp = (struct world_peer_s *)uwp;
    if (wp->pubkey.s) free(wp->pubkey.s);
    free(wp);
    return 0;
}

static int peer_add(struct peer_s *p, struct world_peer_s *wp,
                    bool *added)
{
    if (!p || !wp || !added) return -1;
    wp->found = NULL;
    *added = false;
    ifr(list.map(&p->peers, world.peer.find, wp));
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
    bool added;
    ifr(peer_add(p, wp, &added));
    if (added) {
        ifr(world.peer.auth(p, wp));
    }
    return 0;
}
