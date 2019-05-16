#include <common.h>

#define MAX_RETRY 100

struct cbdata_s {
    struct net_ev_s *ev;
    int              idx;
};

int receive(int sd, char *data, int len,
             struct sockaddr_in *addr, socklen_t *naddr)
{
    // Invalidate only packet header
    memset(data, 0, sizeof(struct header_s));
    socklen_t bytes = recvfrom(sd, data, len, 0, (struct sockaddr *)addr, naddr);
    if (bytes == -1) return -1;
    return 0;
}

static int nb_clean(void *nb)
{
    //free(nb);
    return 0;
}

static int ack_item(struct list_s *l, struct net_ev_s *ev,
                    struct nb_s *nb, int idx)
{
    ev_timer_stop(ev->loop, &nb->timer);
    return list.del(l, nb);
}

static int ack_cb(struct list_s *l, void *unb,
                  void *dcb)
{
    struct nb_s      *nb = (struct nb_s *)unb;
    struct cbdata_s *dsp = dcb;
    if (dsp->idx == nb->idx) {
        if (ack_item(l, dsp->ev, nb, dsp->idx) != 0) return -1;
        return 1;
    }
    return 0;
}

static int ack(struct net_ev_s *ev, struct net_send_s *ns, int idx)
{
    if (!ev || !ns) return -1;
    struct cbdata_s dsp = { .ev = ev, .idx = idx };
    if (list.map(&ns->nbl, ack_cb, &dsp) != 0) return -1;
    return 0;
}

static int dispatch_item(struct list_s *l, struct net_ev_s *ev,
                         struct nb_s *nb, int idx)
{
    ssize_t bytes = sendto(nb->sd,
                           nb->buffer.s,
                           nb->buffer.offset,
                           0,
                           (struct sockaddr *)&nb->remote.addr,
                           nb->remote.len);
    printf("Sending packet %d of %ld bytes to %x:%d retry: %d\n",
                                                     nb->idx, bytes,
                                                     ADDR_IP(nb->remote.addr),
                                                     ADDR_PORT(nb->remote.addr),
                                                     nb->retry);
    if (nb->status == NET_ONESHOT) return list.del(l, nb);
    if (bytes <= 0) printf("Dispatch error: %s\n", strerror(errno));
    nb->status = NET_ACK_WAITING;
    nb->write  = &ev->write;
    ev_timer_again(ev->loop, &nb->timer);
    return 0;
}

static int dispatch_idx_cb(struct list_s *l, void *unb,
                           void *dcb)
{
    struct nb_s      *nb = (struct nb_s *)unb;
    struct cbdata_s *dsp = dcb;
    if (dsp->idx == nb->idx) {
        if (dispatch_item(l, dsp->ev, nb, dsp->idx) != 0) return -1;
        return 1;
    }
    return 0;
}

static int dispatch_idx(struct list_s *l, struct net_ev_s *ev,
                        int idx)
{
    struct cbdata_s dsp = { .ev = ev, .idx = idx };
    if (list.map(l, dispatch_idx_cb, &dsp) != 0) return -1;
    return 0;
}

static int dispatch_cb(struct list_s *l, void *unb, void *uev)
{
    struct nb_s     *nb = (struct nb_s *)unb;
    struct net_ev_s *ev = (struct net_ev_s *)uev;
    if (nb->status != NET_INIT && nb->status != NET_ONESHOT) return 0;
    if (dispatch_idx(l, ev, nb->idx) != 0) return -1;
    return 0;
}

static int dispatch(struct net_ev_s *ev, struct net_send_s *ns)
{
    ev_io_stop(ev->loop, &ev->write);
    if (list.map(&ns->nbl, dispatch_cb, ev) != 0) return -1;
    return 0;
}

static int timeout_cb(struct list_s *l, void *unb,
                      void *dcb)
{
    struct nb_s      *nb = (struct nb_s *)unb;
    struct cbdata_s *dsp = dcb;
    if (dsp->idx == nb->idx) {
        nb->status = NET_INIT;
        if (++nb->retry == MAX_RETRY)
            list.del(l, nb);
        return 1;
    }
    return 0;
}

void timeout(struct ev_loop *loop, struct ev_timer *timer, int revents)
{
    struct net_send_timer_s *nst = (struct net_send_timer_s *)timer->data;
    if (!nst) return;
    struct cbdata_s dsp = { .ev = nst->nev, .idx = nst->nb->idx };
    if (list.map(nst->nbl, timeout_cb, &dsp) != 0) return;
    ev_timer_stop(loop, &nst->nb->timer);
    ev_io_start(loop, nst->nb->write);
}

const struct module_net_s net = {
    .dispatch = dispatch,
    .receive  = receive,
    .ack      = ack,
    .timeout  = timeout,
    .nb.clean = nb_clean,
};
