#include <common.h>

int pong_write(struct data_s *d, void *userdata)
{
    struct peer_s *p = (struct peer_s *)userdata;
    if (!p || !d) return -1;
    if (p->send_buffer.type != BUFFER_PONG) return -1;
    ifr(data.write.tdouble(d, p->send_buffer.u.pong.ts));
    return 0;
}

int pong_read(struct peer_s *p)
{
    if (!p) return -1;
    sn_initr(bf, p->recv_buffer.available->data.s,
             p->recv_buffer.available->data.n);
    double ts;
    ifr(sn_read((void *)&ts, sizeof(ts), &bf));
    if (p->user.cb.pong) {
        ifr(p->user.cb.pong(p,
                            p->received.header.src.host,
                            p->received.header.src.port,
                            ts));
    }
    return 0;
}

int pong_size(int *sz, void *userdata)
{
    if (!sz) return -1;
    *sz = DATA_SIZE_DOUBLE;
    return 0;
}
