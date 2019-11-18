#include <common.h>

int query_write(struct data_s *d, void *userdata)
{
    struct peer_s *p = (struct peer_s *)userdata;
    if (p->send_buffer.type != BUFFER_QUERY) return -1;
    if (!p || !d) return -1;
    ifr(data.write.integer(d, p->send_buffer.u.query.host));
    ifr(data.write.shortint(d, p->send_buffer.u.query.port));
    return 0;
}

int query_read(struct peer_s *p)
{
    if (!p) return -1;
    sn_initr(bf, p->recv_buffer.available->data.s,
             p->recv_buffer.available->data.n);
    int host;
    unsigned short port;
    ifr(sn_read((void *)&host, sizeof(host), &bf));
    ifr(sn_read((void *)&port, sizeof(port), &bf));
    if (p->user.cb.query) {
        ifr(p->user.cb.query(p, p->received.header.src.host,
                             p->received.header.src.port,
                             host, port));
    }
    return 0;
}

int query_size(int *sz, void *userdata)
{
    if (!sz) return -1;
    *sz = DATA_SIZE_INT + DATA_SIZE_SHORT;
    return 0;
}
