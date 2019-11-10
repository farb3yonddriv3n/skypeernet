#include <common.h>

int pong_write(struct data_s *d, void *userdata)
{
    struct peer_s *p = (struct peer_s *)userdata;
    if (!p || !d) return -1;
    if (data.write.integer(d, SPN_VERSION) != 0) return -1;
    return 0;
}

int pong_read(struct peer_s *p)
{
    if (!p) return -1;
    if (p->user.cb.pong) {
        ifr(p->user.cb.pong(p,
                            p->received.header.src.host,
                            p->received.header.src.port));
    }
    return 0;
}

int pong_size(int *sz, void *userdata)
{
    if (!sz) return -1;
    *sz = DATA_SIZE_INT;
    return 0;
}
