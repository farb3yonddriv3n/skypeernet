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
    sn_initr(bf, ins->recv_buffer.available.s, ins->recv_buffer.available.n);
    if (sn_read((void *)&idx, sizeof(idx), &bf) != 0) return -1;
    return net.ack(&ins->ev, &ins->send, idx);
}

static int wp_clean(void *wp)
{
    if (wp) free(wp);
    return 0;
}

static int peer_add(struct peer_s *ins, struct world_peer_s *wp)
{
    if (!ins || !wp) return -1;
    wp->found = false;
    int find(struct list_s *l, void *existing, void *uwp) {
        struct world_peer_s *ex = (struct world_peer_s *)existing;
        struct world_peer_s *wp = (struct world_peer_s *)uwp;
        if (wp->host == ex->host &&
            wp->port == ex->port) {
            wp->found = true;
            return 1;
        }
        return 0;
    }
    ifr(list.map(&ins->peers, find, wp));
    if (wp->found == true) return wp_clean(wp);
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
        sn_initr(bf, ins->recv_buffer.available.s, ins->recv_buffer.available.n);
        if (sn_read((void *)&wp->host, sizeof(wp->host), &bf) != 0) return -1;
        if (sn_read((void *)&wp->port, sizeof(wp->port), &bf) != 0) return -1;
    }
    return peer_add(ins, wp);
}

static int message(struct peer_s *ins)
{
    printf("Message: [%.*s] from %x:%d\n", ins->recv_buffer.available.n,
                                           ins->recv_buffer.available.s,
                                           ADDR_IP(ins->net.remote.addr),
                                           ADDR_PORT(ins->net.remote.addr));
    return 0;
}

static int file(struct peer_s *ins)
{
    ins->recv_buffer.file_size.received += ins->recv_buffer.available.n;
    syslog(LOG_INFO, "File received from %x:%d, %ld/%ld",
                     ADDR_IP(ins->net.remote.addr),
                     ADDR_PORT(ins->net.remote.addr),
                     ins->recv_buffer.file_size.received,
                     ins->recv_buffer.file_size.total);
    char fname[128];
    snprintf(fname, sizeof(fname), "tmp/%d_%d.part", ins->received.header.group,
                                                     ins->received.header.index);
    if (os.filewrite(fname, "wb", ins->recv_buffer.available.s,
                                  ins->recv_buffer.available.n) != 0) return -1;
    return 0;
}

static int file_send(struct peer_s *ins)
{
    sn_initr(bf, ins->recv_buffer.available.s, ins->recv_buffer.available.n);
    if (sn_read((void *)&ins->recv_buffer.file_size.total,
                 sizeof(ins->recv_buffer.file_size.total), &bf) != 0) return -1;
    ins->recv_buffer.file_size.received = 0;
    syslog(LOG_INFO, "About to receive file of %ld bytes from %x:%d",
                     ins->recv_buffer.file_size.total,
                     ADDR_IP(ins->net.remote.addr),
                     ADDR_PORT(ins->net.remote.addr));
    return 0;
}

#define RECV_GROUP_NOTFOUND 1
#define RECV_DUPLICATE      2
#define RECV_AVAILABLE      4

static int cache_clean(void *uc)
{
    struct cache_s *c = (struct cache_s *)uc;
    if (c) {
        if (c->received.idx) free(c->received.idx);
        free(c);
    }
    return 0;
}

static int packet_recv(struct recv_buffer_s *rb, struct packet_s *received,
                       bool *completed)
{
    if (!rb || !received || !completed) return -1;
    *completed = false;
    received->internal.flags |= RECV_GROUP_NOTFOUND;
    int map(struct list_s *l, void *item, void *ud) {
        struct cache_s  *cnt  = (struct cache_s *)item;
        struct packet_s *psrc = (struct packet_s *)ud;
        if (!cnt || !psrc) return -1;
        if (cnt->group == psrc->header.group &&
            cnt->host  == psrc->internal.host &&
            cnt->port  == psrc->internal.port) {
            psrc->internal.flags &= ~RECV_GROUP_NOTFOUND;
            // Same packet received more than once
            int i;
            for (i = 0; i < cnt->received.size; i++)
                if (cnt->received.idx[i] == psrc->header.index) {
                    psrc->internal.flags |= RECV_DUPLICATE;
                    return 1;
                }
            cnt->received.idx = realloc(cnt->received.idx,
                                        ++(cnt->received.size) * sizeof(int));
            if (!cnt->received.idx) return -1;
            cnt->received.idx[cnt->received.size - 1] = psrc->header.index;
            if ((cnt->received.size + 1) == cnt->total) {
                if (list.del(l, cnt) != 0) return -1;
                psrc->internal.flags |= RECV_AVAILABLE;
            }
            return 1;
        }
        return 0;
    }
    if (list.map(&rb->cache, map, received) != 0) return -1;

    if (received->internal.flags & RECV_DUPLICATE) return 0;

    struct packet_s *p = malloc(sizeof(*p));
    if (!p) return -1;
    memcpy(p, received, sizeof(*p));
    if (list.add(&rb->packets, p, packet.clean) != 0) return -1;

    if (received->internal.flags & RECV_AVAILABLE ||
        p->header.total == 1) {
        struct available_s { int group; int host; unsigned short port;
                             sn *dst; };
        int available(struct list_s *l, void *item, void *ud) {
            struct packet_s    *p   = item;
            struct available_s *avs = ud;
            if (p->header.group  == avs->group &&
                p->internal.host == avs->host &&
                p->internal.port == avs->port) {
                if (avs->dst->n < p->header.sequence + p->header.length) {
                    avs->dst->n = p->header.sequence + p->header.length;
                    avs->dst->s = realloc(avs->dst->s, avs->dst->n);
                    if (!avs->dst->s) return -1;
                }
                memcpy(avs->dst->s + p->header.sequence,
                       p->buffer.payload, p->header.length);
                if (list.del(l, p) != 0) return -1;
            }
            return 0;
        }
        struct available_s avs = { .group = p->header.group,
                                   .host  = p->internal.host,
                                   .port  = p->internal.port,
                                   .dst   = &rb->available };
        if (list.map(&rb->packets, available, &avs) != 0) return -1;
        *completed = true;
        return 0;
    }

    if (received->internal.flags & RECV_GROUP_NOTFOUND) {
        struct cache_s *cs = malloc(sizeof(*cs));
        if (!cs) return -1;
        memset(cs, 0, sizeof(*cs));
        cs->group = p->header.group;
        cs->total = p->header.total;
        cs->host  = p->internal.host;
        cs->port  = p->internal.port;
        if (list.add(&rb->cache, cs, cache_clean) != 0) return -1;
    }
    return 0;
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

    bool completed;
    if (packet_recv(&ins->recv_buffer,
                    &ins->received, &completed) != 0) return -1;
    if (world_map[idx].reply)
        if (world_map[idx].reply(ins) != 0) return -1;
    if (!completed) return 0;

    syslog(LOG_DEBUG, "Executing command: %d", ins->received.header.command);

    if (world_map[idx].exec)
        if (world_map[idx].exec(ins) != 0) return -1;

    free(ins->recv_buffer.available.s);
    ins->recv_buffer.available.s = NULL;
    ins->recv_buffer.available.n = 0;
    return 0;
}

const struct module_world_s world = {
    .handle     = handle,
};
