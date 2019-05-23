#include <common.h>

int receive(int sd, char *data, int len,
             struct sockaddr_in *addr, socklen_t *naddr)
{
    // Invalidate only packet header
    memset(data, 0, sizeof(struct header_s));
    socklen_t bytes = recvfrom(sd, data, len, 0, (struct sockaddr *)addr, naddr);
    //syslog(LOG_DEBUG, "Received %d bytes from %x:%d",
    //                  bytes, ADDR_IP((*addr)), ADDR_PORT((*addr)));
    if (bytes == -1) return -1;
    return 0;
}

static int nb_clean(void *unb)
{
    if (!unb) return -1;
    struct nb_s *nb = (struct nb_s *)unb;
    if (task.update(nb->peer) != 0) return -1;
    free(nb);
    return 0;
}

static int ack(struct net_ev_s *ev, struct net_send_s *ns, int idx)
{
    int cb(struct list_s *l, void *unb, void *dcb)
    {
        struct nb_s *nb  = (struct nb_s *)unb;
        int          idx = *(int *)dcb;
        if (idx == nb->pidx) {
            if (nb->cmd == COMMAND_PING)
                ifr(world.peer.reachable(nb->peer,
                                         ADDR_IP(nb->remote.addr),
                                         ADDR_PORT(nb->remote.addr)));
            if (list.del(l, nb) != 0) return -1;
            return 1;
        }
        return 0;
    }
    if (!ev || !ns) return -1;
    if (list.map(&ns->nbl, cb, &idx) != 0) return -1;
    return 0;
}

static int maxattempts(struct list_s *l, struct nb_s *nb, bool *giveup)
{
    if (!l || !nb || !giveup) return -1;
    *giveup = false;
    if (nb->attempt++ > MAX_RETRY) {
        if (nb->cmd == COMMAND_PING)
            ifr(world.peer.unreachable(nb->peer,
                                       ADDR_IP(nb->remote.addr),
                                       ADDR_PORT(nb->remote.addr)));
        *giveup = true;
        return list.del(l, nb);
    }
    return 0;
}

static int dispatch(struct net_ev_s *ev, struct net_send_s *ns)
{
    int cb(struct list_s *l, void *unb, void *uev)
    {
        int item(struct list_s *l, struct net_ev_s *ev,
                 struct nb_s *nb, int idx)
        {
            ssize_t bytes = sendto(nb->sd,
                                   nb->buffer.s,
                                   nb->buffer.offset,
                                   0,
                                   (struct sockaddr *)&nb->remote.addr,
                                   nb->remote.len);
            /*
            syslog(LOG_DEBUG, "Sending packet %d of group %d %ld bytes to %x:%d attempt %d",
                              nb->idx, nb->grp, bytes,
                              ADDR_IP(nb->remote.addr),
                              ADDR_PORT(nb->remote.addr),
                              nb->attempt);
                              */
            if (nb->status == NET_ONESHOT) return list.del(l, nb);
            if (bytes <= 0) syslog(LOG_ERR, "Dispatch error: %s", strerror(errno));
            nb->status = NET_ACK_WAITING;
            nb->write  = &ev->write;
            return 0;
        }

        struct nb_s     *nb = (struct nb_s *)unb;
        struct net_ev_s *ev = (struct net_ev_s *)uev;
        bool giveup;
        ifr(maxattempts(l, nb, &giveup));
        if (giveup) return 0;
        if (item(l, ev, nb, nb->pidx) != 0) return -1;
        return 0;
    }
    ev_io_stop(ev->loop, &ev->write);
    ev_timer_stop(ev->loop, &ev->send);
    if (list.map(&ns->nbl, cb, ev) != 0) return -1;
    ev_timer_again(ev->loop, &ev->send);
    return 0;
}

void net_send(struct ev_loop *loop, struct ev_timer *timer, int revents)
{
    struct peer_s *p = (struct peer_s *)timer->data;
    int sz;
    if (list.size(&p->send.nbl, &sz) != 0) return;
    if (sz > 0) ev_io_start(loop, &p->ev.write);
}

const struct module_net_s net = {
    .dispatch = dispatch,
    .receive  = receive,
    .ack      = ack,
    .send     = net_send,
    .nb.clean = nb_clean,
};
