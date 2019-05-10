#include <common.h>

int message_write(struct data_s *d, void *userdata)
{
    struct peer_s *p = (struct peer_s *)userdata;
    if (!p || !d) return -1;
    if (data.write.raw(d, "hi   there", 10) != 0) return -1;
    return 0;
}

int message_size(int *sz, void *userdata)
{
    if (!sz) return -1;
    *sz = 10;
    return 0;
}

/*
const struct module_handler_s mesage = {
    .write = message_write,
    .size  = message_size,
};
*/
