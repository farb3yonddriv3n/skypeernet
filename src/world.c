#include <common.h>


static int peer_add(struct world_peer_s *wp)
{
    return 0;
}

static int peer_read(struct peer_s *p, struct world_peer_s *wp,
                     char *buffer, char nbuffer)
{
    sn_initr(bf, buffer, nbuffer);
    if (sn_read((void *)&wp->host, sizeof(wp->host), &bf) != 0) return -1;
    if (sn_read((void *)&wp->port, sizeof(wp->port), &bf) != 0) return -1;

    printf("peer read: %x:%d\n", wp->host, wp->port);

    return payload.send.peer(p, COMMAND_MESSAGE, wp->host, wp->port);
    /*
    struct data_s d;
    if (data.init(&d, COMMAND_MSG, msg_cb, msg_size,
                  (void *)p) != 0) return -1;
    if (data.send(&d, p->net.sd, &p->net.remote.addr,
                  p->net.remote.len, 0,
                  wp->host,
                  wp->port,
                  &p->send.data, &p->send.len) != 0) return -1;
    ev_io_start(p->ev.loop, &p->ev.write);
    return 0;
    */
}

static int parse(struct peer_s *pr, struct packet_s *p)
{
    switch (p->header.command) {
        case COMMAND_TRACKER_ANNOUNCE_PEER: {
            struct world_peer_s wp;
            if (peer_read(pr, &wp, p->buffer.payload, p->header.length) != 0) return -1;
            if (peer_add(&wp) != 0) return -1;
            } break;
        case COMMAND_MESSAGE: {
            printf("message: %.*s from %x:%d\n", p->header.length, p->buffer.payload,
                                      NET_IP(pr->net.remote.addr),
                                      NET_PORT(pr->net.remote.addr));
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
