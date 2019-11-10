#include <common.h>

int ping_write(struct data_s *d, void *userdata)
{
    struct peer_s *p = (struct peer_s *)userdata;
    if (!p || !d) return -1;
    if (data.write.integer(d, SPN_VERSION) != 0) return -1;
    return 0;
}

int ping_read(struct peer_s *p)
{
    return payload.send(p, COMMAND_PONG,
                        p->received.header.src.host,
                        p->received.header.src.port,
                        0, 0, NULL, NULL);
}

int ping_size(int *sz, void *userdata)
{
    if (!sz) return -1;
    *sz = DATA_SIZE_INT;
    return 0;
}
