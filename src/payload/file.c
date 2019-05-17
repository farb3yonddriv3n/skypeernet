#include <common.h>

int file_write(struct data_s *d, void *userdata)
{
    struct peer_s *p = (struct peer_s *)userdata;
    if (!p || !d) return -1;
    if (p->send_buffer.type != BUFFER_FILE) return -1;
    if (data.write.raw(d, (char *)p->send_buffer.u.file.bin.s,
                       p->send_buffer.u.file.bin.n) != 0) return -1;
    return 0;
}

int file_size(int *sz, void *userdata)
{
    struct peer_s *p = (struct peer_s *)userdata;
    if (!p || !sz) return -1;
    if (p->send_buffer.type != BUFFER_FILE) return -1;
    *sz = p->send_buffer.u.file.bin.n;
    return 0;
}
