#include <common.h>

static int tcp_ports(struct peer_s *p, struct data_s *d, struct list_s *tcpports,
                     char *tcpdesc, int ntcpdesc)
{
    if (!p || !d) return -1;
    if (data.write.integer(d, ntcpdesc) != 0) return -1;
    if (ntcpdesc > 0) {
        if (data.write.raw(d, tcpdesc, ntcpdesc) != 0) return -1;
    }
    int append(struct list_s *l, void *ex, void *ud) {
        int port = *(int *)ex;
        struct data_s *d = (struct data_s *)ud;
        if (data.write.integer(d, port) != 0) return -1;
        return 0;
    }
    ifr(list.map(tcpports, append, d));
    return 0;
}

int announce_pwp(struct data_s *d, void *userdata)
{
    struct peer_s *p = (struct peer_s *)userdata;
    if (!p || !d) return -1;
    if (data.write.integer(d, ADDR_IP(p->net.self.addr))      != 0) return -1;
    if (data.write.shortint(d, ADDR_PORT(p->net.self.addr))   != 0) return -1;
    if (data.write.integer(d, p->cfg.keys.local.str.public.n) != 0) return -1;
    if (data.write.raw(d, p->cfg.keys.local.str.public.s,
                       p->cfg.keys.local.str.public.n) != 0) return -1;
    if (data.write.integer(d, SPN_VERSION) != 0) return -1;
    ifr(tcp_ports(p, d, &p->cfg.tcp.ports,
                  p->cfg.tcp.description, strlen(p->cfg.tcp.description)));
    return 0;
}

int announce_twp(struct data_s *d, void *userdata)
{
    struct peer_s *t = (struct peer_s *)userdata;
    if (!t || !d) return -1;
    if (t->send_buffer.type != BUFFER_TRACKER_ANNOUNCE_PEER) return -1;
    if (data.write.integer(d, t->send_buffer.u.tracker_peer.host)   != 0) return -1;
    if (data.write.shortint(d, t->send_buffer.u.tracker_peer.port)  != 0) return -1;
    if (data.write.integer(d, t->send_buffer.u.tracker_peer.key->n) != 0) return -1;
    if (data.write.raw(d, (char *)t->send_buffer.u.tracker_peer.key->s,
                       t->send_buffer.u.tracker_peer.key->n) != 0) return -1;
    if (data.write.integer(d, t->send_buffer.u.tracker_peer.version) != 0) return -1;
    ifr(tcp_ports(t, d, t->send_buffer.u.tracker_peer.tcpports,
                  t->send_buffer.u.tracker_peer.tcpdesc.s,
                  t->send_buffer.u.tracker_peer.tcpdesc.n));
    return 0;
}

int announce_twt(struct data_s *d, void *userdata)
{
    struct peer_s *t = (struct peer_s *)userdata;
    if (!t || !d) return -1;
    if (data.write.integer(d, ADDR_IP(t->net.remote.addr))    != 0) return -1;
    if (data.write.shortint(d, ADDR_PORT(t->net.remote.addr)) != 0) return -1;
    if (data.write.integer(d, t->cfg.keys.local.str.public.n) != 0) return -1;
    if (data.write.raw(d, t->cfg.keys.local.str.public.s,
                       t->cfg.keys.local.str.public.n) != 0) return -1;
    if (data.write.integer(d, SPN_VERSION) != 0) return -1;
    ifr(tcp_ports(t, d, &t->cfg.tcp.ports,
                  t->cfg.tcp.description, strlen(t->cfg.tcp.description)));
    return 0;
}

static int port_clean(void *ud)
{
    if (!ud) return -1;
    free(ud);
    return 0;
}

static int announce_read(struct world_peer_s *wp, char *src, int nsrc)
{
    sn_initr(bf, src, nsrc);
    if (sn_read((void *)&wp->host, sizeof(wp->host), &bf) != 0) return -1;
    if (sn_read((void *)&wp->port, sizeof(wp->port), &bf) != 0) return -1;
    int nkey;
    if (sn_read((void *)&nkey, sizeof(nkey), &bf) != 0) return -1;
    sn_bytes_init_new(wp->pubkey, nkey);
    if (sn_read((void *)wp->pubkey.s, nkey, &bf) != 0) return -1;
    sha256hex((unsigned char *)wp->pubkey.s, nkey, wp->pubkeyhash);
    if (sn_read((void *)&wp->version, sizeof(wp->version), &bf) != 0) return -1;
    int ndesc;
    if (sn_read((void *)&ndesc, sizeof(ndesc), &bf) != 0) return -1;
    if (ndesc > sizeof(wp->tcp.description)) return -1;
    if (sn_read((void *)wp->tcp.description, ndesc, &bf) != 0) return -1;
    int i, added;
    for (i = bf.offset, added = 0;
         (i < bf.n && added < MAX_ANNOUNCED_PORTS);
         i += DATA_SIZE_INT, added++) {
        int *tcpport = malloc(sizeof(*tcpport));
        if (!tcpport) return -1;
        if (sn_read((void *)tcpport, sizeof(*tcpport), &bf) != 0) return -1;
        ifr(list.add(&wp->tcp.ports, tcpport, port_clean));
    }
    return 0;
}

int announce_size(int *sz, void *userdata)
{
    if (!sz || !userdata) return -1;
    struct peer_s *p = (struct peer_s *)userdata;
    int portsz;
    *sz = DATA_SIZE_INT + DATA_SIZE_SHORT;
    *sz += p->cfg.keys.local.str.public.n + DATA_SIZE_INT;
    *sz += DATA_SIZE_INT;
    if (p->send_buffer.type == BUFFER_TRACKER_ANNOUNCE_PEER) {
        *sz += DATA_SIZE_INT;
        *sz += p->send_buffer.u.tracker_peer.tcpdesc.n;
        ifr(list.size(p->send_buffer.u.tracker_peer.tcpports, &portsz));
    } else {
        *sz += strlen(p->cfg.tcp.description) + DATA_SIZE_INT;
        ifr(list.size(&p->cfg.tcp.ports, &portsz));
    }
    *sz += (DATA_SIZE_INT * portsz);
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
    ifr(world.peer.add(p, wp, &added));
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
    ifr(world.peer.add(p, wp, &added));
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
    ifr(world.peer.add(p, wp, &added));
    if (added) {
        ifr(world.peer.auth(p, wp));
    }
    return 0;
}
