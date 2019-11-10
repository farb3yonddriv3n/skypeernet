#include <common.h>

int message_write(struct data_s *d, void *userdata)
{
    struct peer_s *p = (struct peer_s *)userdata;
    if (!p || !d) return -1;
    if (p->send_buffer.type != BUFFER_MESSAGE) return -1;
    if (data.write.raw(d, (char *)p->send_buffer.u.message.str,
                       strlen(p->send_buffer.u.message.str)) != 0) return -1;
    return 0;
}

int message_read(struct peer_s *p)
{
    if (!p) return -1;
    if (p->user.cb.message)
        ifr(p->user.cb.message(p,
                               p->received.header.src.host,
                               p->received.header.src.port,
                               p->recv_buffer.available->data.s,
                               p->recv_buffer.available->data.n));
    return 0;
}

int message_size(int *sz, void *userdata)
{
    struct peer_s *p = (struct peer_s *)userdata;
    if (!p || !sz) return -1;
    if (p->send_buffer.type != BUFFER_MESSAGE) return -1;
    *sz = strlen(p->send_buffer.u.message.str);
    return 0;
}
