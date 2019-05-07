#include <common.h>

static int append(sn *dst, char *src, int nsrc)
{
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

static int init(struct data_s *d, enum data_e cmd,
                int (*callback)(struct data_s*, void*),
                void *userdata, size_t sz)
{
    if (!d || !callback || !userdata) return -1;
    d->command = cmd;
    sn_bytes_new(payload, sz);
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

static int prepare(struct data_s *d, char *buffer, int nbuffer,
                   size_t ds)
{
    if (!d || !buffer) return -1;
    if (ds > nbuffer) return -1;

    memcpy(buffer, (void *)&d->command, sizeof(d->command));
    memcpy(buffer + sizeof(d->command), d->payload.s, d->payload.n);
    return 0;
}

static int data_send(struct data_s *d, int sd, struct sockaddr_in *addr,
                     int addr_len, int index, int host, unsigned short port,
                     struct nb_s **nb, int *nnb)
{
    if (!d || !addr) return -1;
    size_t ds;
    if (data.size(d, &ds) != 0) return -1;
    char buffer[1024];
    if (data.prepare(d, buffer, sizeof(buffer), ds) != 0) return -1;
    int j;
    for (j = 0; j < ds; j++) {
    }
    struct packet_s *packets;
    int npackets;
    if (packet.serialize.init(buffer, ds, &packets, &npackets, index) != 0)
        return -1;
    int i;
    for (i = 0; i < npackets; i++) {
        *nb = realloc(*nb, sizeof(**nb) * (++(*nnb)));
        int idx = *nnb - 1;
        (*nb)[idx].sd                   = sd;
        (*nb)[idx].buffer.n             = sizeof(packets[i]);
        memcpy((*nb)[idx].buffer.s, (char *)&packets[i], sizeof(packets[i]));
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
    .prepare        = prepare,
    .size           = size,
    .write.integer  = data_write_int,
    .write.shortint = data_write_shortint,
};
