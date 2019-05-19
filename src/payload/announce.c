#include <common.h>

int announce_write_peer(struct data_s *d, void *userdata)
{
    struct peer_s *p = (struct peer_s *)userdata;
    if (!p || !d) return -1;
    if (data.write.integer(d, ADDR_IP(p->net.self.addr))    != 0) return -1;
    if (data.write.shortint(d, ADDR_PORT(p->net.self.addr)) != 0) return -1;
    return 0;
}

int announce_write_tracker(struct data_s *d, void *userdata)
{
    struct peer_s *t = (struct peer_s *)userdata;
    if (!t || !d) return -1;
    if (data.write.integer(d, ADDR_IP(t->net.remote.addr)) != 0) return -1;
    if (data.write.shortint(d, ADDR_PORT(t->net.remote.addr))       != 0) return -1;
    return 0;
}

int announce_size(int *sz, void *userdata)
{
    if (!sz) return -1;
    *sz = DATA_SIZE_INT + DATA_SIZE_SHORT;
    return 0;
}

/*
const struct module_handler_s announce = {
    .tracker.write = announce_write_tracker,
    .peer.write    = announce_write_peer,
    .size          = announce_size,
};
*/
