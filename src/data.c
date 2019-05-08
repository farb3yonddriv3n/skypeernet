#include <common.h>

static int append(sn *dst, char *src, int nsrc)
{
    //if (sn_bytes_append_raw(dst, (char *)&nsrc, sizeof(nsrc)) != 0) return -1;
    if (sn_bytes_append_raw(dst, src, nsrc) != 0) return -1;
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
                int (*callback_size)(struct data_s*, void*),
                void *userdata)
{
    if (!d || !callback || !userdata) return -1;
    d->command = cmd;
    if (callback_size(d, userdata) != 0) return -1;
    sn_bytes_new(payload, d->size);
    sn_set(d->payload, payload);
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

/*
static int prepare(struct data_s *d, char *buffer, int nbuffer,
                   size_t ds)
{
    if (!d || !buffer) return -1;
    if (ds > nbuffer) return -1;

    memcpy(buffer, (void *)&d->command, sizeof(d->command));
    memcpy(buffer + sizeof(d->command), d->payload.s, d->payload.n);
    return 0;
}
*/
static int packet_set(snb *dst, struct packet_s *p)
{
    dst->n = sizeof(dst->s);
    dst->offset = 0;
    if (snb_bytes_append_raw(dst, (char *)&p->header.index,    sizeof(p->header.index))    != 0)
        return -1;
    if (snb_bytes_append_raw(dst, (char *)&p->header.sequence, sizeof(p->header.sequence)) != 0)
        return -1;
    if (snb_bytes_append_raw(dst, (char *)&p->header.total,    sizeof(p->header.total))    != 0)
        return -1;
    if (snb_bytes_append_raw(dst, (char *)&p->header.length,   sizeof(p->header.length))   != 0)
        return -1;
    if (snb_bytes_append_raw(dst, (char *)&p->header.command,  sizeof(p->header.command))  != 0)
        return -1;
    if (snb_bytes_append_raw(dst, (char *)&p->buffer.payload, p->header.length) != 0)
        return -1;
    if (snb_bytes_append_raw(dst, (char *)&p->buffer.hash, sizeof(p->buffer.hash)) != 0)
        return -1;
    return 0;
}

static int packet_get(struct packet_s *p, char *buffer, int nbuffer)
{
    sn_initr(b, buffer, nbuffer);
    if (sn_read((void *)&p->header.index,    sizeof(p->header.index), &b)    != 0) return -1;
    if (sn_read((void *)&p->header.sequence, sizeof(p->header.sequence), &b) != 0) return -1;
    if (sn_read((void *)&p->header.total,    sizeof(p->header.total), &b)    != 0) return -1;
    if (sn_read((void *)&p->header.length,   sizeof(p->header.length), &b)   != 0) return -1;
    if (sn_read((void *)&p->header.command,  sizeof(p->header.command), &b)  != 0) return -1;

    if (sn_read(p->buffer.payload, p->header.length, &b) != 0)       return -1;
    if (sn_read(p->buffer.hash,    sizeof(p->buffer.hash), &b) != 0) return -1;
    return 0;
}


static int data_send(struct data_s *d, int sd, struct sockaddr_in *addr,
                     int addr_len, int index, int host, unsigned short port,
                     struct nb_s **nb, int *nnb)
{
    if (!d || !addr) return -1;
    //char buffer[1024];
    //memcpy(buffer, d->payload.s, d->size);
    //if (data.prepare(d, buffer, sizeof(buffer), ds) != 0) return -1;
    struct packet_s *packets;
    int npackets;
    if (packet.serialize.init(d->command, d->payload.s, d->size, &packets,
                              &npackets, index) != 0) return -1;
    int i;
    for (i = 0; i < npackets; i++) {
        *nb = realloc(*nb, sizeof(**nb) * (++(*nnb)));
        int idx = *nnb - 1;
        (*nb)[idx].sd                   = sd;

        if (packet_set(&(*nb)[idx].buffer, &packets[i]) != 0)
            return -1;
        memcpy(&(*nb)[idx].remote.addr, addr, sizeof(*addr));
        (*nb)[idx].remote.len                  = addr_len;
        (*nb)[idx].remote.addr.sin_addr.s_addr = host;
        (*nb)[idx].remote.addr.sin_port        = port;
    }
    return 0;
}

const struct module_data_s data = {
    .init           = init,
    .send           = data_send,
    //.prepare        = prepare,
    .get            = packet_get,
    .size           = size,
    .write.integer  = data_write_int,
    .write.shortint = data_write_shortint,
    .write.raw      = data_write_raw,
};
