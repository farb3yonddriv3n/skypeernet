#include <common.h>

static int append(sn *dst, char *src, int nsrc)
{
    //if (sn_bytes_append_raw(dst, (char *)&nsrc, sizeof(nsrc)) != 0) return -1;
    if (sn_bytes_append(dst, src, nsrc) != 0) return -1;
    return 0;
}

static int data_write_double(struct data_s *d, const double src)
{
    if (append(&d->payload, (char *)&src, sizeof(src)) != 0) return -1;
    return 0;
}

static int data_write_int(struct data_s *d, const int src)
{
    if (append(&d->payload, (char *)&src, sizeof(src)) != 0) return -1;
    return 0;
}

static int data_write_shortint(struct data_s *d, const short src)
{
    if (append(&d->payload, (char *)&src, sizeof(src)) != 0) return -1;
    return 0;
}

static int data_write_byte(struct data_s *d, const char src)
{
    if (append(&d->payload, (char *)&src, sizeof(src)) != 0) return -1;
    return 0;
}

static int data_write_raw(struct data_s *d, char *src, const int nsrc)
{
    if (append(&d->payload, src, nsrc) != 0) return -1;
    return 0;
}

static int init(struct data_s *d, enum command_e cmd,
                int (*callback)(struct data_s*, void*),
                int (*callback_size)(int*, void*),
                void *userdata)
{
    if (!d || !callback || !callback_size || !userdata) return -1;
    d->command = cmd;
    int sz;
    if (callback_size(&sz, userdata) != 0) return -1;
    sn_bytes_init_new(d->payload, sz);
    if (callback(d, userdata) != 0) return -1;
    return 0;
}

static int size(struct data_s *d, size_t *sz)
{
    if (!d) return -1;
    //if (d->payload.n != d->payload.offset) return -1;
    *sz = sizeof(d->command) + d->payload.n;
    return 0;
}

static int packet_set(snb *dst, struct packet_s *p)
{
    dst->n = sizeof(dst->s);
    dst->offset = 0;
    if (snb_bytes_append(dst, (char *)&p->header.pidx,    sizeof(p->header.pidx))    != 0)
        return -1;
    if (snb_bytes_append(dst, (char *)&p->header.gidx,    sizeof(p->header.gidx))    != 0)
        return -1;
    if (snb_bytes_append(dst, (char *)&p->header.tidx,    sizeof(p->header.tidx))    != 0)
        return -1;
    if (snb_bytes_append(dst, (char *)&p->header.offset,  sizeof(p->header.offset))  != 0)
        return -1;
    if (snb_bytes_append(dst, (char *)&p->header.chunks,  sizeof(p->header.chunks))  != 0)
        return -1;
    if (snb_bytes_append(dst, (char *)&p->header.parts,   sizeof(p->header.parts))   != 0)
        return -1;
    if (snb_bytes_append(dst, (char *)&p->header.length,  sizeof(p->header.length))  != 0)
        return -1;
    if (snb_bytes_append(dst, (char *)&p->header.command, sizeof(p->header.command)) != 0)
        return -1;
    if (snb_bytes_append(dst, (char *)&p->header.filename, sizeof(p->header.filename)) != 0)
        return -1;
    if (snb_bytes_append(dst, (char *)&p->header.tcp.reqtype, sizeof(p->header.tcp.reqtype)) != 0)
        return -1;
    if (snb_bytes_append(dst, (char *)&p->header.tcp.cidx, sizeof(p->header.tcp.cidx)) != 0)
        return -1;
    if (snb_bytes_append(dst, (char *)&p->header.tcp.port.src, sizeof(p->header.tcp.port.src)) != 0)
        return -1;
    if (snb_bytes_append(dst, (char *)&p->header.tcp.port.dst, sizeof(p->header.tcp.port.dst)) != 0)
        return -1;
    if (snb_bytes_append(dst, (char *)&p->header.src.host, sizeof(p->header.src.host)) != 0)
        return -1;
    if (snb_bytes_append(dst, (char *)&p->header.src.port, sizeof(p->header.src.port)) != 0)
        return -1;
    if (snb_bytes_append(dst, (char *)&p->header.dst.host, sizeof(p->header.dst.host)) != 0)
        return -1;
    if (snb_bytes_append(dst, (char *)&p->header.dst.port, sizeof(p->header.dst.port)) != 0)
        return -1;
    if (snb_bytes_append(dst, (char *)&p->header.ts, sizeof(p->header.ts)) != 0)
        return -1;
    if (snb_bytes_append(dst, (char *)&p->buffer.payload, p->header.length)          != 0)
        return -1;
    if (snb_bytes_append(dst, (char *)&p->buffer.hash,    sizeof(p->buffer.hash))    != 0)
        return -1;
    return 0;
}

static int packet_get(struct packet_s *p, char *buffer, int nbuffer)
{
    memset(p, 0, sizeof(*p));
    sn_initr(b, buffer, nbuffer);
    if (sn_read((void *)&p->header.pidx,    sizeof(p->header.pidx),    &b) != 0) return -1;
    if (sn_read((void *)&p->header.gidx,    sizeof(p->header.gidx),    &b) != 0) return -1;
    if (sn_read((void *)&p->header.tidx,    sizeof(p->header.tidx),    &b) != 0) return -1;
    if (sn_read((void *)&p->header.offset,  sizeof(p->header.offset),  &b) != 0) return -1;
    if (sn_read((void *)&p->header.chunks,  sizeof(p->header.chunks),  &b) != 0) return -1;
    if (sn_read((void *)&p->header.parts,   sizeof(p->header.parts),   &b) != 0) return -1;
    if (sn_read((void *)&p->header.length,  sizeof(p->header.length),  &b) != 0) return -1;
    if (sn_read((void *)&p->header.command, sizeof(p->header.command), &b) != 0) return -1;
    if (sn_read((void *)&p->header.filename, sizeof(p->header.filename), &b) != 0) return -1;
    if (sn_read((void *)&p->header.tcp.reqtype, sizeof(p->header.tcp.reqtype), &b) != 0) return -1;
    if (sn_read((void *)&p->header.tcp.cidx, sizeof(p->header.tcp.cidx), &b) != 0) return -1;
    if (sn_read((void *)&p->header.tcp.port.src, sizeof(p->header.tcp.port.src), &b) != 0) return -1;
    if (sn_read((void *)&p->header.tcp.port.dst, sizeof(p->header.tcp.port.dst), &b) != 0) return -1;
    if (sn_read((void *)&p->header.src.host, sizeof(p->header.src.host), &b) != 0) return -1;
    if (sn_read((void *)&p->header.src.port, sizeof(p->header.src.port), &b) != 0) return -1;
    if (sn_read((void *)&p->header.dst.host, sizeof(p->header.dst.host), &b) != 0) return -1;
    if (sn_read((void *)&p->header.dst.port, sizeof(p->header.dst.port), &b) != 0) return -1;
    if (sn_read((void *)&p->header.ts, sizeof(p->header.ts), &b) != 0) return -1;
    if (sn_read(p->buffer.payload, p->header.length, &b)       != 0) return -1;
    if (sn_read(p->buffer.hash,    sizeof(p->buffer.hash), &b) != 0) return -1;
    return 0;
}

static int map_columns(struct list_s *l, void *li, void *userdata)
{
    if (!l || !li || !userdata) return -1;
    struct nb_s *nb = (struct nb_s *)userdata;
    char buffer[128];
    int offset;
    ifr(unique_together(buffer, sizeof(buffer), &offset, 2,
                        (void *)&nb->pidx, sizeof(nb->pidx),
                        (void *)&nb->tidx, sizeof(nb->tidx)));
    return LISTADD(l, "pidx_tidx", buffer, offset);
}

static int data_submit(struct peer_s *p, struct packet_s *pck,
                       int host, unsigned short port)
{
    if (!p || !pck) return -1;
    struct nb_s *nb = malloc(sizeof(*nb));
    if (!nb) return -1;
    nb->peer = p;
    nb->pidx = pck->header.pidx;
    nb->gidx = pck->header.gidx;
    nb->tidx = pck->header.tidx;
    nb->cmd  = pck->header.command;
    nb->sd   = p->net.sd;
    ifr(packet_set(&nb->buffer, pck));
    memcpy(&nb->remote.addr, &p->net.remote.addr, sizeof(p->net.remote.addr));
    nb->remote.len = p->net.remote.len;
    ADDR_IP(nb->remote.addr)   = host;
    ADDR_PORT(nb->remote.addr) = port;
    nb->status = (nb->cmd== COMMAND_ACK ||
                  nb->cmd == COMMAND_PING ||
                  nb->cmd == COMMAND_PONG)
                 ? NET_ONESHOT : NET_INIT;
    nb->attempt = 0;
    if (nb->cmd == COMMAND_ACK ||
        nb->cmd == COMMAND_PING ||
        nb->cmd == COMMAND_PONG) {
        ifr(list.add(&p->send.instant, nb, net.nb.clean));
        ev_io_start(p->ev.loop, &p->ev.write_instant);
    } else {
        ifr(list.column.add(&p->send.nbl, nb, map_columns, net.nb.clean));
    }
    return 0;
}

static int data_send(struct data_s *d, struct peer_s *p, int host,
                     unsigned short port, unsigned int tidx,
                     unsigned int parts, unsigned char *filename,
                     struct tcp_s *tcp)
{
    if (!d || !p) return -1;
    struct packet_s *packets;
    int npackets;
    int dst_host = host;
    unsigned short dst_port = port;
    ifr(world.peer.shadow(p, &host, &port));
    ifr(packet.serialize.init(p, d->command, d->payload.s, d->payload.n, &packets,
                              &npackets, &p->send_buffer, tidx, parts,
                              filename, tcp, dst_host, dst_port));
    sn_bytes_delete(d->payload);
    int i;
    for (i = 0; i < npackets; i++) {
        ifr(data_submit(p, &packets[i], host, port));
    }
    if (packets) free(packets);
    return 0;
}

const struct module_data_s data = {
    .init           = init,
    .submit         = data_submit,
    .send           = data_send,
    .get            = packet_get,
    .size           = size,
    .write.tdouble  = data_write_double,
    .write.integer  = data_write_int,
    .write.shortint = data_write_shortint,
    .write.byte     = data_write_byte,
    .write.raw      = data_write_raw,
};
