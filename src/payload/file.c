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
    char startswith[256];
    snprintf(startswith, sizeof(startswith), "%08x_%05d_%010d_%010d",
                                   ADDR_IP(p->net.remote.addr),
                                   ADDR_PORT(p->net.remote.addr),
                                   p->received.header.tidx,
                                   p->received.header.gidx);
    bool exists;
    ifr(os.partexists(&p->cfg, startswith, &exists));
    if (exists) return 0;
    char fname[512];
    snprintf(fname, sizeof(fname), "%s_%010d_%010d.part",
                                   startswith,
                                   p->received.header.pidx,
                                   p->received.header.parts);
    char fnamepath[1024];
    snprintf(fnamepath, sizeof(fnamepath), "%s/%s/%s",
             p->cfg.download_dir, os.getpartsdir(), fname);
    if (os.filewrite(fnamepath, "wb", p->recv_buffer.available->data.s,
                                      p->recv_buffer.available->data.n) != 0) return -1;
    char received[256];
    bool finalized;
    ifr(os.filejoin(&p->cfg, fname, received, sizeof(received), &finalized));
    struct world_peer_s wp = { .host = ADDR_IP(p->net.remote.addr),
                               .port = ADDR_PORT(p->net.remote.addr),
                               .found = NULL };
    ifr(list.map(&p->peers, peer_find, &wp));
    if (!wp.found) return -1;
    if (finalized && p->user.cb.file)
        ifr(p->user.cb.file(p,
                            ADDR_IP(p->net.remote.addr),
                            ADDR_PORT(p->net.remote.addr),
                            wp.found->keyhash,
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
