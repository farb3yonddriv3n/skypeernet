#include <common.h>

int queryrpl_write(struct data_s *d, void *userdata)
{
    struct peer_s *p = (struct peer_s *)userdata;
    if (p->send_buffer.type != BUFFER_QUERY_REPLY) return -1;
    if (!p || !d) return -1;
    ifr(data.write.integer(d, p->send_buffer.u.query_reply.host));
    ifr(data.write.shortint(d, p->send_buffer.u.query_reply.port));
    ifr(data.write.byte(d, p->send_buffer.u.query_reply.reachable));
    return 0;
}

int queryrpl_read(struct peer_s *p)
{
    if (!p) return -1;
    sn_initr(bf, p->recv_buffer.available->data.s,
             p->recv_buffer.available->data.n);
    int host;
    unsigned short port;
    char reachable;
    ifr(sn_read((void *)&host, sizeof(host), &bf));
    ifr(sn_read((void *)&port, sizeof(port), &bf));
    ifr(sn_read((void *)&reachable, sizeof(reachable), &bf));
    if (p->user.cb.query_reply) {
        ifr(p->user.cb.query_reply(p, p->received.header.src.host,
                                   p->received.header.src.port,
                                   host, port, reachable));
    }
    return 0;
}

int queryrpl_size(int *sz, void *userdata)
{
    if (!sz) return -1;
    *sz = DATA_SIZE_INT + \
          DATA_SIZE_SHORT + \
          DATA_SIZE_BYTE;
    return 0;
}
