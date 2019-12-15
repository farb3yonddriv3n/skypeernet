#include <common.h>

int ping_write(struct data_s *d, void *userdata)
{
    struct peer_s *p = (struct peer_s *)userdata;
    if (!p || !d) return -1;
    if (p->send_buffer.type != BUFFER_PING) return -1;
    if (data.write.tdouble(d, p->send_buffer.u.ping.ts) != 0) return -1;
    return 0;
}

int ping_read(struct peer_s *p)
{
    if (!p) return -1;
    sn_initr(bf, p->recv_buffer.available->data.s,
             p->recv_buffer.available->data.n);
    double ts;
    if (sn_read((void *)&ts, sizeof(ts), &bf) != 0) return -1;
    if (p->user.cb.ping) {
        ifr(p->user.cb.ping(p, p->received.header.src.host,
                            p->received.header.src.host, ts));
    }
    return 0;
}

int ping_size(int *sz, void *userdata)
{
    if (!sz) return -1;
    *sz = DATA_SIZE_DOUBLE;
    return 0;
}
