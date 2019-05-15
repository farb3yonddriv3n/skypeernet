#include <common.h>

int message_write(struct data_s *d, void *userdata)
{
    struct peer_s *p = (struct peer_s *)userdata;
    if (!p || !d) return -1;
    if (p->buffer.type != BUFFER_MESSAGE) return -1;
    if (data.write.raw(d, (char *)p->buffer.u.message.str,
                       strlen(p->buffer.u.message.str)) != 0) return -1;
    return 0;
}

int message_size(int *sz, void *userdata)
{
    struct peer_s *p = (struct peer_s *)userdata;
    if (!p || !sz) return -1;
    *sz = strlen(p->buffer.u.message.str);
    return 0;
}

/*
const struct module_handler_s mesage = {
    .write = message_write,
    .size  = message_size,
};
*/
