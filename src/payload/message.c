#include <common.h>

int message_write(struct data_s *d, void *userdata)
{
    struct peer_s *p = (struct peer_s *)userdata;
    if (!p || !d) return -1;
    if (data.write.raw(d, "hi   there", 10) != 0) return -1;
    return 0;
}

int message_size(struct data_s *d, void *userdata)
{
    if (!d) return -1;
    d->size = 10;
    return 0;
}

/*
const struct module_handler_s mesage = {
    .write = message_write,
    .size  = message_size,
};
*/
