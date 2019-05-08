#include <common.h>

int net_recv(int sd, char *data, int len,
             struct sockaddr_in *addr, socklen_t *naddr)
{
    // Invalidate only packet header
    memset(data, 0, sizeof(struct header_s));
    socklen_t bytes = recvfrom(sd, data, len, 0, (struct sockaddr *)addr, naddr);
    if (bytes == -1) return -1;
    return 0;
}

int net_send(struct nb_s **nb, int *nnb)
{
    int i;
    for (i = 0; i < *nnb; i++) {
        sendto((*nb)[i].sd,
               (*nb)[i].buffer.s,
               (*nb)[i].buffer.offset,
               0,
               (struct sockaddr *)&(*nb)[i].remote.addr,
               (*nb)[i].remote.len);
    }
    free(*nb);
    *nb = NULL;
    *nnb = 0;
    return 0;
}
