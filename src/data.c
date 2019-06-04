#include <common.h>

static int append(sn *dst, char *src, int nsrc)
{
    //if (sn_bytes_append_raw(dst, (char *)&nsrc, sizeof(nsrc)) != 0) return -1;
    if (sn_bytes_append(dst, src, nsrc) != 0) return -1;
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
    if (sn_read(p->buffer.payload, p->header.length, &b)       != 0) return -1;
    if (sn_read(p->buffer.hash,    sizeof(p->buffer.hash), &b) != 0) return -1;
    return 0;
}

static int data_send(struct data_s *d, struct peer_s *p,
                     int host, unsigned short port,
                     unsigned int tidx, unsigned int parts)
{
    if (!d || !p) return -1;
    struct packet_s *packets;
    int npackets;
    if (packet.serialize.init(d->command, d->payload.s, d->payload.n, &packets,
                              &npackets, &p->send_buffer,
                              tidx, parts) != 0) return -1;
    sn_bytes_delete(d->payload);
    int i;
    for (i = 0; i < npackets; i++) {
        struct nb_s *nb = malloc(sizeof(*nb));
        if (!nb) return -1;
        nb->peer = p;
        nb->pidx = packets[i].header.pidx;
        nb->gidx = packets[i].header.gidx;
        nb->cmd  = packets[i].header.command;
        nb->sd   = p->net.sd;
        if (packet_set(&nb->buffer, &packets[i]) != 0) return -1;
        memcpy(&nb->remote.addr, &p->net.remote.addr, sizeof(p->net.remote.addr));
        nb->remote.len = p->net.remote.len;
        ADDR_IP(nb->remote.addr)   = host;
        ADDR_PORT(nb->remote.addr) = port;
        nb->status = (packets[i].header.command == COMMAND_ACK)
                     ? NET_ONESHOT : NET_INIT;
        nb->attempt = 0;
        if (nb->cmd == COMMAND_ACK) assert(list.add_head(&p->send.nbl, nb, net.nb.clean) == 0);
        else                        list.add(&p->send.nbl, nb, net.nb.clean);
    }
    if (packets) free(packets);
    ev_io_start(p->ev.loop, &p->ev.write);
    return 0;
}

const struct module_data_s data = {
    .init           = init,
    .send           = data_send,
    .get            = packet_get,
    .size           = size,
    .write.integer  = data_write_int,
    .write.shortint = data_write_shortint,
    .write.raw      = data_write_raw,
};
