#include <common.h>

int ack_write(struct data_s *d, void *userdata)
{
    struct peer_s *p = (struct peer_s *)userdata;
    if (!p || !d) return -1;
    if (data.write.integer(d, p->received.header.pidx) != 0) return -1;
    return 0;
}

int ack_read(struct peer_s *p)
{
    int idx;
    sn_initr(bf, p->recv_buffer.available->data.s,
             p->recv_buffer.available->data.n);
    if (sn_read((void *)&idx, sizeof(idx), &bf) != 0) return -1;
    return net.ack(&p->ev, &p->send, idx);
}

int ack_size(int *sz, void *userdata)
{
    if (!sz) return -1;
    *sz = DATA_SIZE_INT;
    return 0;
}

int ack_reply(struct peer_s *p)
{
    return payload.send(p, COMMAND_ACK,
                        ADDR_IP(p->net.remote.addr),
                        ADDR_PORT(p->net.remote.addr), 0, 0);
}
