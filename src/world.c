#include <common.h>

static int ack_reply(struct peer_s *ins)
{
    return payload.send((struct peer_s *)ins, COMMAND_ACK,
                        ADDR_IP(ins->net.remote.addr),
                        ADDR_PORT(ins->net.remote.addr));
}

static int ack(struct peer_s *ins)
{
    int idx;
    sn_initr(bf, ins->recv_buffer.available->data.s,
             ins->recv_buffer.available->data.n);
    if (sn_read((void *)&idx, sizeof(idx), &bf) != 0) return -1;
    return net.ack(&ins->ev, &ins->send, idx);
}

static int wp_clean(void *wp)
{
    if (!wp) return -1;
    free(wp);
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

static int peer_del(struct peer_s *p, int host, unsigned short port)
{
    struct world_peer_s wp = { .host = host, .port = port, .found = NULL };
    ifr(list.map(&p->peers, peer_find, &wp));
    if (wp.found) return list.del(&p->peers, wp.found);
    return 0;
}

static int peer_add(struct peer_s *ins, struct world_peer_s *wp)
{
    if (!ins || !wp) return -1;
    wp->found = NULL;
    ifr(list.map(&ins->peers, peer_find, wp));
    if (wp->found) return wp_clean(wp);
    ifr(list.add(&ins->peers, wp, wp_clean));
    if (ins->type == INSTANCE_PEER) return 0;

    int broadcast(struct list_s *l, void *existing, void *uwp) {
        struct world_peer_s *ex = (struct world_peer_s *)existing;
        struct world_peer_s *wp = (struct world_peer_s *)uwp;
        if (wp->host == ex->host &&
            wp->port == ex->port) {
            return 0;
        }
        int item(int dst, unsigned short dstport,
                 int src, unsigned short srcport) {
            struct peer_s *t = (struct peer_s *)ins;
            ADDR_IP(t->net.remote.addr)   = dst;
            ADDR_PORT(t->net.remote.addr) = dstport;
            if (payload.send(t, COMMAND_TRACKER_ANNOUNCE_PEER,
                             src, srcport) != 0) return -1;
            return 0;
        }
        ifr(item(wp->host, wp->port, ex->host, ex->port));
        ifr(item(ex->host, ex->port, wp->host, wp->port));
        return 0;
    }
    ifr(list.map(&ins->peers, broadcast, wp));
    return 0;
}

static int announce_peer(struct peer_s *ins)
{
    struct world_peer_s *wp = malloc(sizeof(*wp));
    if (!wp) return -1;
    if (ins->type == INSTANCE_TRACKER) {
        wp->host = ADDR_IP(ins->net.remote.addr);
        wp->port = ADDR_PORT(ins->net.remote.addr);
    } else {
        sn_initr(bf, ins->recv_buffer.available->data.s,
                 ins->recv_buffer.available->data.n);
        if (sn_read((void *)&wp->host, sizeof(wp->host), &bf) != 0) return -1;
        if (sn_read((void *)&wp->port, sizeof(wp->port), &bf) != 0) return -1;
    }
    return peer_add(ins, wp);
}

static int message(struct peer_s *ins)
{
    printf("Message: [%.*s] from %x:%d\n", ins->recv_buffer.available->data.n,
                                           ins->recv_buffer.available->data.s,
                                           ADDR_IP(ins->net.remote.addr),
                                           ADDR_PORT(ins->net.remote.addr));
    return 0;
}

static int file(struct peer_s *ins)
{
    //ins->recv_buffer.file_size.received += ins->recv_buffer.available->data.n;
    syslog(LOG_INFO, "File received from %x:%d, %ld/%ld",
                     ADDR_IP(ins->net.remote.addr),
                     ADDR_PORT(ins->net.remote.addr),
                     ins->recv_buffer.available->file_size.received,
                     ins->recv_buffer.available->file_size.total);
    char fname[128];
    snprintf(fname, sizeof(fname), "tmp/%d_%d_%d_%d.part",
                                   ADDR_IP(ins->net.remote.addr),
                                   ADDR_PORT(ins->net.remote.addr),
                                   ins->received.header.group,
                                   ins->received.header.index);
    if (os.filewrite(fname, "wb", ins->recv_buffer.available->data.s,
                                  ins->recv_buffer.available->data.n) != 0) return -1;
    return 0;
}

static int file_send(struct peer_s *ins)
{
    sn_initr(bf, ins->recv_buffer.available->data.s,
             ins->recv_buffer.available->data.n);
    if (sn_read((void *)&ins->recv_buffer.available->file_size.total,
                 sizeof(ins->recv_buffer.available->file_size.total), &bf) != 0) return -1;
    ins->recv_buffer.available->file_size.received = 0;
    syslog(LOG_INFO, "About to receive file of %ld bytes from %x:%d",
                     ins->recv_buffer.available->file_size.total,
                     ADDR_IP(ins->net.remote.addr),
                     ADDR_PORT(ins->net.remote.addr));
    return 0;
}

static int cache_clean(void *uc)
{
    struct cache_s *c = (struct cache_s *)uc;
    if (c) {
        if (c->packets.received.idx) free(c->packets.received.idx);
        ifr(list.clean(&c->packets.all));
        sn_free(c->data);
        free(c);
    }
    return 0;
}

static int seal_clean(void *us)
{
    struct seal_s *s = (struct seal_s *)us;
    if (!s) return -1;
    if (s->group) free(s->group);
    free(s);
    return 0;
}

struct seal_find_s {
    int            host;
    unsigned short port;
    struct seal_s *found;
};

static int seal_find(struct list_s *lst, void *us, void *ud) {
    struct seal_s      *s  = (struct seal_s *)us;
    struct seal_find_s *sf = (struct seal_find_s *)ud;
    if (s->host == sf->host &&
        s->port == sf->port) {
        sf->found = s;
        return 1;
    }
    return 0;
}

static int seal_add(struct list_s *l, struct packet_s *p)
{
    if (!l || !p) return -1;
    struct seal_find_s sf = { .host  = p->internal.host,
                              .port  = p->internal.port,
                              .found = NULL };
    ifr(list.map(l, seal_find, &sf));
    if (sf.found) {
        sf.found->group = realloc(sf.found->group,
                                  ++(sf.found->size) * sizeof(int));
        if (!sf.found->group) return -1;
        sf.found->group[sf.found->size - 1] = p->header.group;
    } else {
        struct seal_s *s = malloc(sizeof(*s));
        if (!s) return -1;
        s->size  = 1;
        s->group = malloc(s->size * sizeof(int));
        if (!s->group) return -1;
        s->group[s->size - 1] = p->header.group;
        s->host               = p->internal.host;
        s->port               = p->internal.port;
        ifr(list.add(l, s, seal_clean));
    }
    return 0;
}

static int seal_check(struct list_s *l, struct packet_s *p,
                      bool *sealed)
{
    if (!l || !p || !sealed) return -1;
    *sealed = false;
    struct seal_find_s sf = { .host  = p->internal.host,
                              .port  = p->internal.port,
                              .found = NULL };
    ifr(list.map(l, seal_find, &sf));
    if (!sf.found) return 0;
    int i;
    for (i = 0; i < sf.found->size; i++) {
        if (sf.found->group[i] == p->header.group) {
            *sealed = true;
            break;
        }
    }
    return 0;
}

static int packet_recv_cache(struct recv_buffer_s *rb, struct cache_s *c,
                             struct packet_s *received, struct cache_s **available)
{
    if (!c || !received || !available) return -1;
    bool sealed;
    ifr(seal_check(&rb->sealed, received, &sealed));
    if (sealed) return 0;
    int i;
    for (i = 0; i < c->packets.received.size; i++)
        if (c->packets.received.idx[i] == received->header.index)
            return 0;
    struct packet_s *p = malloc(sizeof(*p));
    if (!p) return -1;
    memcpy(p, received, sizeof(*p));
    if (list.add(&c->packets.all, p, packet.clean) != 0) return -1;
    c->packets.received.idx = realloc(c->packets.received.idx,
                                      ++(c->packets.received.size) * sizeof(int));
    if (!c->packets.received.idx) return -1;
    c->packets.received.idx[c->packets.received.size - 1] = received->header.index;
    if (c->packets.received.size != c->packets.total) return 0;
    int avcb(struct list_s *l, void *item, void *ud) {
        struct packet_s *p   = item;
        sn              *avs = ud;
        if (avs->n < p->header.sequence + p->header.length) {
            avs->n = p->header.sequence + p->header.length;
            avs->s = realloc(avs->s, avs->n);
            if (!avs->s) return -1;
        }
        memcpy(avs->s + p->header.sequence,
               p->buffer.payload, p->header.length);
        if (list.del(l, p) != 0) return -1;
        return 0;
    }
    if (list.map(&c->packets.all, avcb,
                 &c->data) != 0) return -1;
    *available = c;
    ifr(seal_add(&rb->sealed, received));
    return 0;
}

static int packet_recv(struct recv_buffer_s *rb, struct packet_s *received,
                       struct cache_s **available)
{
    struct cache_find_s {
        int             group;
        int             host;
        unsigned short  port;
        struct cache_s *cache;
    };
    int find(struct list_s *l, void *uitem, void *ud) {
        struct cache_s      *cache = (struct cache_s *)uitem;
        struct cache_find_s *cf    = (struct cache_find_s *)ud;
        if (cache->group == cf->group &&
            cache->host  == cf->host &&
            cache->port  == cf->port) {
            cf->cache = cache;
            return 1;
        }
        return 0;
    }
    struct cache_find_s cf = { .group = received->header.group,
                               .host  = received->internal.host,
                               .port  = received->internal.port,
                               .cache = NULL };
    ifr(list.map(&rb->cache, find, &cf));
    if (!cf.cache) {
        cf.cache = malloc(sizeof(*(cf.cache)));
        if (!cf.cache) return -1;
        memset(cf.cache, 0, sizeof(*(cf.cache)));
        cf.cache->group = received->header.group;
        cf.cache->packets.total = received->header.total;
        cf.cache->host  = received->internal.host;
        cf.cache->port  = received->internal.port;
        if (list.add(&rb->cache, cf.cache, cache_clean) != 0) return -1;
    }
    return packet_recv_cache(rb, cf.cache, received, available);
}

static const struct { enum command_e cmd;
                      int (*exec)(struct peer_s*);
                      int (*reply)(struct peer_s*);
                    } world_map[] = {
    { COMMAND_NONE,                  NULL,          NULL },
    { COMMAND_ACK,                   ack,           NULL },
    { COMMAND_TRACKER_ANNOUNCE_PEER, announce_peer, ack_reply },
    { COMMAND_PEER_ANNOUNCE_PEER,    announce_peer, ack_reply },
    { COMMAND_MESSAGE,               message,       ack_reply },
    { COMMAND_FILE,                  file,          ack_reply },
    { COMMAND_FILE_SEND,             file_send,     ack_reply },
};

static int command_find(int *idx, enum command_e cmd)
{
    int i;
    for (i = 0; i < COUNTOF(world_map); i++) {
        if (world_map[i].cmd == cmd) {
            *idx = i;
            return 0;
        }
    }
    return -1;
}

static int handle(struct peer_s *ins)
{
    if (!ins) return -1;

    int idx;
    if (command_find(&idx, ins->received.header.command) != 0) return -1;

    ins->recv_buffer.available = NULL;
    if (packet_recv(&ins->recv_buffer,
                    &ins->received, &ins->recv_buffer.available) != 0) return -1;
    if (world_map[idx].reply)
        if (world_map[idx].reply(ins) != 0) return -1;
    if (!ins->recv_buffer.available) return 0;

    syslog(LOG_DEBUG, "Executing command: %d", ins->received.header.command);

    if (world_map[idx].exec)
        if (world_map[idx].exec(ins) != 0) return -1;

    return list.del(&ins->recv_buffer.cache,
                    ins->recv_buffer.available);
}

const struct module_world_s world = {
    .handle   = handle,
    .peer.del = peer_del,
};
