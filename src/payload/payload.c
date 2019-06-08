#include <common.h>

static const struct { enum command_e cmd;
                      int (*cb_write)(struct data_s*, void*);
                      int (*cb_size)(int*, void*);
                      int (*cb_read)(struct peer_s*);
                      int (*cb_reply)(struct peer_s*);
                    } cmds[] =
{
    { COMMAND_NONE,                     NULL,          NULL,          NULL,         NULL      },
    { COMMAND_ACK,                      ack_write,     ack_size,      ack_read,     NULL      },
    { COMMAND_TRACKER_ANNOUNCE_TRACKER, announce_twt,  announce_size, announce_trt, ack_reply },
    { COMMAND_TRACKER_ANNOUNCE_PEER,    announce_twp,  announce_size, announce_trp, ack_reply },
    { COMMAND_PEER_ANNOUNCE_PEER,       announce_pwp,  announce_size, announce_prp, ack_reply },
    { COMMAND_MESSAGE,                  message_write, message_size,  message_read, ack_reply },
    { COMMAND_FILE,                     file_write,    file_size,     file_read,    ack_reply },
    { COMMAND_FILEASK,                  fileask_write, fileask_size,  fileask_read, ack_reply },
    { COMMAND_PING,                     ping_write,    ping_size,     ping_read,    ack_reply },
};

static int exec(struct peer_s *parent, enum command_e cmd,
                int host, unsigned short port,
                int (*cb_write)(struct data_s*, void*),
                int (*cb_size)(int*, void*), unsigned int tidx,
                unsigned int parts, unsigned char *filename)
{
    struct data_s d;
    if (data.init(&d, cmd, cb_write, cb_size, parent)            != 0) return -1;
    if (data.send(&d, parent, host, port, tidx, parts, filename) != 0) return -1;
    return 0;
}

static int command_find(int *idx, enum command_e cmd)
{
    int i;
    for (i = 0; i < COUNTOF(cmds); i++) {
        if (cmds[i].cmd == cmd) {
            *idx = i;
            return 0;
        }
    }
    return -1;
}

static int payload_send(void *parent, enum command_e cmd,
                        int host, unsigned short port,
                        unsigned int tidx, unsigned int parts,
                        unsigned char *filename)
{
    int idx;
    ifr(command_find(&idx, cmd));
    return exec(parent, cmd, host, port,
                cmds[idx].cb_write,
                cmds[idx].cb_size,
                tidx, parts,
                filename);
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
        sf.found->group[sf.found->size - 1] = p->header.gidx;
    } else {
        struct seal_s *s = malloc(sizeof(*s));
        if (!s) return -1;
        s->size  = 1;
        s->group = malloc(s->size * sizeof(int));
        if (!s->group) return -1;
        s->group[s->size - 1] = p->header.gidx;
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
        if (sf.found->group[i] == p->header.gidx) {
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
    //bool sealed;
    //ifr(seal_check(&rb->sealed, received, &sealed));
    //if (sealed) return 0;
    int i;
    for (i = 0; i < c->packets.received.size; i++)
        if (c->packets.received.idx[i] == received->header.pidx)
            return 0;
    struct packet_s *p = malloc(sizeof(*p));
    if (!p) return -1;
    memcpy(p, received, sizeof(*p));
    if (list.add(&c->packets.all, p, packet.clean) != 0) return -1;
    c->packets.received.idx = realloc(c->packets.received.idx,
                                      ++(c->packets.received.size) * sizeof(int));
    if (!c->packets.received.idx) return -1;
    c->packets.received.idx[c->packets.received.size - 1] = received->header.pidx;
    if (c->packets.received.size != c->packets.total) return 0;
    int avcb(struct list_s *l, void *item, void *ud) {
        struct packet_s *p   = item;
        sn              *avs = ud;
        if (avs->n < p->header.offset + p->header.length) {
            avs->n = p->header.offset + p->header.length;
            avs->s = realloc(avs->s, avs->n);
            if (!avs->s) return -1;
        }
        memcpy(avs->s + p->header.offset,
               p->buffer.payload, p->header.length);
        if (list.del(l, p) != 0) return -1;
        return 0;
    }
    if (list.map(&c->packets.all, avcb,
                 &c->data) != 0) return -1;
    *available = c;
    //ifr(seal_add(&rb->sealed, received));
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
    struct cache_find_s cf = { .group = received->header.gidx,
                               .host  = received->internal.host,
                               .port  = received->internal.port,
                               .cache = NULL };
    ifr(list.map(&rb->cache, find, &cf));
    if (!cf.cache) {
        cf.cache = malloc(sizeof(*(cf.cache)));
        if (!cf.cache) return -1;
        memset(cf.cache, 0, sizeof(*(cf.cache)));
        cf.cache->group = received->header.gidx;
        cf.cache->packets.total = received->header.chunks;
        cf.cache->host  = received->internal.host;
        cf.cache->port  = received->internal.port;
        if (list.add(&rb->cache, cf.cache, cache_clean) != 0) return -1;
    }
    return packet_recv_cache(rb, cf.cache, received, available);
}

static int payload_recv(struct peer_s *p)
{
    int idx;
    ifr(command_find(&idx, p->received.header.command));
    p->recv_buffer.available = NULL;
    if (packet_recv(&p->recv_buffer,
                    &p->received, &p->recv_buffer.available) != 0) return -1;
    if (cmds[idx].cb_reply)
        ifr(cmds[idx].cb_reply(p));
    if (!p->recv_buffer.available) return 0;
    if (cmds[idx].cb_read)
        ifr(cmds[idx].cb_read(p));
    return list.del(&p->recv_buffer.cache,
                    p->recv_buffer.available);
}

const struct module_payload_s payload = {
    .send = payload_send,
    .recv = payload_recv,
};
