#include <common.h>

int announce_write_peer(struct data_s *d, void *userdata)
{
    struct peer_s *p = (struct peer_s *)userdata;
    if (!p || !d) return -1;
    if (data.write.integer(d, p->net.self.addr.sin_addr.s_addr) != 0) return -1;
    if (data.write.shortint(d, p->net.self.addr.sin_port)       != 0) return -1;
    return 0;
}

int announce_write_tracker(struct data_s *d, void *userdata)
{
    struct tracker_s *t = (struct tracker_s *)userdata;
    if (!t || !d) return -1;
    if (data.write.integer(d, t->net.remote.addr.sin_addr.s_addr) != 0) return -1;
    if (data.write.shortint(d, t->net.remote.addr.sin_port)       != 0) return -1;
    return 0;
}

int announce_size(struct data_s *d, void *userdata)
{
    if (!d) return -1;
    d->size = DATA_SIZE_INT + DATA_SIZE_SHORT;
    return 0;
}

/*
const struct module_handler_s announce = {
    .tracker.write = announce_write_tracker,
    .peer.write    = announce_write_peer,
    .size          = announce_size,
};
*/
