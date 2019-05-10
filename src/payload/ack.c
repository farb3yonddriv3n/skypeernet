#include <common.h>

int ack_write_peer(struct data_s *d, void *userdata)
{
    struct peer_s *p = (struct peer_s *)userdata;
    if (!p || !d) return -1;
    if (data.write.integer(d, p->received.header.index) != 0) return -1;
    return 0;
}

int ack_write_tracker(struct data_s *d, void *userdata)
{
    struct tracker_s *t = (struct tracker_s *)userdata;
    if (!t || !d) return -1;
    if (data.write.integer(d, t->received.header.index) != 0) return -1;
    return 0;
}

int ack_size(int *sz, void *userdata)
{
    if (!sz) return -1;
    *sz = DATA_SIZE_INT;
    return 0;
}
