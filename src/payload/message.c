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
    printf("Message: [%.*s] from %x:%d\n", p->recv_buffer.available->data.n,
                                           p->recv_buffer.available->data.s,
                                           ADDR_IP(p->net.remote.addr),
                                           ADDR_PORT(p->net.remote.addr));
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
