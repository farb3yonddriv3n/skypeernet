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

int file_read(struct peer_s *p)
{
    syslog(LOG_INFO, "File received from %x:%d, %ld/%ld",
                     ADDR_IP(p->net.remote.addr),
                     ADDR_PORT(p->net.remote.addr),
                     p->recv_buffer.available->file_size.received,
                     p->recv_buffer.available->file_size.total);
    char fname[256];
    snprintf(fname, sizeof(fname), "%d_%d_%010d_%010d_%010d_%010d.part",
                                   ADDR_IP(p->net.remote.addr),
                                   ADDR_PORT(p->net.remote.addr),
                                   p->received.header.tidx,
                                   p->received.header.gidx,
                                   p->received.header.pidx,
                                   p->received.header.parts);
    char fnamepath[512];
    snprintf(fnamepath, sizeof(fnamepath), "%s/%s/%s", p->cfg.download_dir, PARTS_DIR, fname);
    if (os.filewrite(fnamepath, "wb", p->recv_buffer.available->data.s,
                                      p->recv_buffer.available->data.n) != 0) return -1;
    char received[256];
    bool finalized;
    ifr(os.filejoin(&p->cfg, fname, received, sizeof(received), &finalized));
    if (finalized && p->user.cb.file)
        ifr(p->user.cb.file(p,
                            ADDR_IP(p->net.remote.addr),
                            ADDR_PORT(p->net.remote.addr),
                            received));
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
