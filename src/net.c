#include <common.h>

int receive(int sd, char *data, int len,
             struct sockaddr_in *addr, socklen_t *naddr)
{
    // Invalidate only packet header
    memset(data, 0, sizeof(struct header_s));
    socklen_t bytes = recvfrom(sd, data, len, 0, (struct sockaddr *)addr, naddr);
    if (bytes == -1) return -1;
    return 0;
}

static int ack(struct net_ev_s *ev, struct net_send_s *ns, int idx)
{
    if (!ev || !ns) return -1;
    if (idx >= ns->len) return -1;

    struct nb_s *nb = &ns->data[idx];
    ev_timer_stop(ev->loop, &nb->timer);
    nb->status = NET_OK;
    if (++(ns->progress) == ns->len) {
        free(ns->data);
        ns->data = NULL;
        ns->len = ns->progress = 0;
    }
    return 0;
}

static int dispatch_idx(struct net_ev_s *ev, struct net_send_s *ns, int idx)
{
    struct nb_s *nb = &ns->data[idx];
    ssize_t bytes = sendto(nb->sd,
                           nb->buffer.s,
                           nb->buffer.offset,
                           0,
                           (struct sockaddr *)&nb->remote.addr,
                           nb->remote.len);
    if (bytes <= 0) printf("Dispatch error: %s\n", strerror(errno));
    nb->status = NET_ACK_WAITING;
    nb->write  = &ev->write;
    ev_timer_again(ev->loop, &nb->timer);
    return 0;
}

static int dispatch(struct net_ev_s *ev, struct net_send_s *ns)
{
    int i;
    ev_io_stop(ev->loop, &ev->write);
    for (i = 0; i < ns->len; i++) {
        struct nb_s *nb = &ns->data[i];
        if (nb->status != NET_INIT) continue;
        if (dispatch_idx(ev, ns, i) != 0) return -1;
    }
    return 0;
}

void timeout(struct ev_loop *loop, struct ev_timer *timer, int revents)
{
    struct net_send_timer_s *nst = (struct net_send_timer_s *)timer->data;
    if (!nst) return;
    struct nb_s **nb = nst->data;
    (*nb)[nst->idx].status = NET_INIT;
    ev_timer_stop(loop, &(*nb)[nst->idx].timer);
    ev_io_start(loop, (*nb)[nst->idx].write);
}

const struct module_net_s net = {
    .dispatch = dispatch,
    .receive  = receive,
    .ack      = ack,
    .timeout  = timeout,
};
