#include <common.h>

int ack_write(struct data_s *d, void *userdata)
{
    struct peer_s *p = (struct peer_s *)userdata;
    if (!p || !d) return -1;
    ifr(data.write.integer(d, SPN_VERSION));
    ifr(data.write.integer(d, p->received.header.tidx));
    ifr(data.write.integer(d, p->received.header.pidx));
    ifr(data.write.integer(d, p->received.header.command));
    ifr(data.write.tdouble(d, p->received.header.ts));
    return 0;
}

int ack_read(struct peer_s *p)
{
    if (!p) return -1;
    int tidx, pidx, ver, cmd;
    double ts;
    sn_initr(bf, p->recv_buffer.available->data.s,
             p->recv_buffer.available->data.n);
    ifr(sn_read((void *)&ver, sizeof(ver), &bf));
    ifr(sn_read((void *)&tidx, sizeof(tidx), &bf));
    ifr(sn_read((void *)&pidx, sizeof(pidx), &bf));
    ifr(sn_read((void *)&cmd, sizeof(cmd), &bf));
    ifr(sn_read((void *)&ts, sizeof(ts), &bf));
    ifr(net.ack(&p->send, tidx, pidx));
    return 0;
}

int ack_size(int *sz, void *userdata)
{
    if (!sz) return -1;
    *sz = (DATA_SIZE_INT * 4) + DATA_SIZE_DOUBLE;
    return 0;
}

int ack_reply(struct peer_s *p)
{
    if (!p) return -1;
    return payload.send(p, COMMAND_ACK,
                        ADDR_IP(p->net.remote.addr),
                        ADDR_PORT(p->net.remote.addr),
                        0, 0, NULL, NULL);
}
