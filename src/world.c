#include <common.h>

static int ack_reply(struct instance_s *ins)
{
    if (ins->type == INSTANCE_PEER)
        return payload.send.peer((struct peer_s *)ins, COMMAND_ACK_PEER,
                                 ADDR_IP(ins->net.remote.addr),
                                 ADDR_PORT(ins->net.remote.addr));
    else if (ins->type == INSTANCE_TRACKER)
        return payload.send.tracker((struct tracker_s *)ins, COMMAND_ACK_TRACKER,
                                    ADDR_IP(ins->net.remote.addr),
                                    ADDR_PORT(ins->net.remote.addr));
    else return -1;
}

static int ack(struct instance_s *ins)
{
    int idx;
    sn_initr(bf, ins->received.buffer.payload, ins->received.header.length);
    if (sn_read((void *)&idx, sizeof(idx), &bf) != 0) return -1;
    return net.ack(&ins->ev, &ins->send, idx);
}

static int wp_clean(void *wp)
{
    free(wp);
    return 0;
}

static int peer_add(struct instance_s *ins, struct world_peer_s *wp)
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
    if (wp->found == true) return 0;
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
            struct tracker_s *t = (struct tracker_s *)ins;
            ADDR_IP(t->net.remote.addr)   = dst;
            ADDR_PORT(t->net.remote.addr) = dstport;
            if (payload.send.tracker(t, COMMAND_TRACKER_ANNOUNCE_PEER,
                                     src,
                                     srcport) != 0) return -1;
            return 0;
        }
        ifr(item(wp->host, wp->port, ex->host, ex->port));
        ifr(item(ex->host, ex->port, wp->host, wp->port));
        return 0;
    }
    ifr(list.map(&ins->peers, broadcast, wp));
    return 0;
}

static int announce_peer(struct instance_s *ins)
{
    struct world_peer_s *wp = malloc(sizeof(*wp));
    if (!wp) return -1;
    if (ins->type == INSTANCE_TRACKER) {
        wp->host = ADDR_IP(ins->net.remote.addr);
        wp->port = ADDR_PORT(ins->net.remote.addr);
    } else {
        sn_initr(bf, ins->received.buffer.payload, ins->received.header.length);
        if (sn_read((void *)&wp->host, sizeof(wp->host), &bf) != 0) return -1;
        if (sn_read((void *)&wp->port, sizeof(wp->port), &bf) != 0) return -1;
    }
    return peer_add(ins, wp);
}

static int message(struct instance_s *ins)
{
    printf("Message: [%.*s] from %x:%d\n", ins->received.header.length,
                                           ins->received.buffer.payload,
                                           ADDR_IP(ins->net.remote.addr),
                                           ADDR_PORT(ins->net.remote.addr));
    return 0;
}

static const struct { enum command_e cmd;
                      int (*exec)(struct instance_s*);
                      int (*reply)(struct instance_s*);
                    } world_map[] = {
    { COMMAND_NONE,                  NULL,          NULL },
    { COMMAND_ACK_TRACKER,           ack,           NULL },
    { COMMAND_ACK_PEER,              ack,           NULL },
    { COMMAND_TRACKER_ANNOUNCE_PEER, announce_peer, ack_reply },
    { COMMAND_PEER_ANNOUNCE_PEER,    announce_peer, ack_reply },
    { COMMAND_MESSAGE,               message,       ack_reply },
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

static int handle(struct instance_s *ins)
{
    if (!ins) return -1;
    int idx;
    if (command_find(&idx, ins->received.header.command) != 0) return -1;

    packet.dump(&ins->received);
    if (world_map[idx].exec)
        if (world_map[idx].exec(ins) != 0) return -1;
    if (world_map[idx].reply)
        return world_map[idx].reply(ins);
    return 0;
}

const struct module_world_s world = {
    .handle     = handle,
};
