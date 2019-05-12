#include <common.h>

static int peer_add(struct world_peer_s *wp)
{
    return 0;
}

static int peer_idx(struct peer_s *p, char *buffer, int nbuffer)
{
    int idx;
    sn_initr(bf, buffer, nbuffer);
    if (sn_read((void *)&idx, sizeof(idx), &bf) != 0) return -1;
    return net.ack(&p->ev, &p->send, idx);
}

static int peer_read(struct peer_s *p, struct world_peer_s *wp,
                     char *buffer, int nbuffer)
{
    /*
    sn_initr(bf, buffer, nbuffer);
    if (sn_read((void *)&wp->host, sizeof(wp->host), &bf) != 0) return -1;
    if (sn_read((void *)&wp->port, sizeof(wp->port), &bf) != 0) return -1;

    printf("peer read: %x:%d\n", wp->host, wp->port);

    return payload.send.peer(p, COMMAND_MESSAGE, wp->host, wp->port);
    */
    return 0;
}

static int parse(struct peer_s *p)
{
    switch (p->received.header.command) {
        case COMMAND_TRACKER_ANNOUNCE_PEER: {
            struct world_peer_s wp;
            if (peer_read(p, &wp, p->received.buffer.payload,
                          p->received.header.length) != 0) return -1;
            if (peer_add(&wp) != 0) return -1;
            } break;
        case COMMAND_MESSAGE: {
            printf("message: %.*s from %x:%d\n", p->received.header.length,
                                                 p->received.buffer.payload,
                                                 ADDR_IP(p->net.remote.addr),
                                                 ADDR_PORT(p->net.remote.addr));
            } break;
        case COMMAND_ACK_TRACKER:
        case COMMAND_ACK_PEER: {
             if (peer_idx(p, p->received.buffer.payload,
                          p->received.header.length) != 0) return -1;
            } break;
        default:
            return -1;
    }
    return 0;
}

const struct module_world_s world = {
    .parse     = parse,
    .peer.read = peer_read,
    .peer.add  = peer_add,
};
