#include <common.h>

static int receive(struct peer_s *p, int sd, char *data, int len,
                   struct sockaddr_in *addr, socklen_t *naddr)
{
    bool suspend;
    ifr(traffic.update.recv(p, 0, &suspend));
    if (suspend) return 0;
    // Invalidate only packet header
    memset(data, 0, sizeof(struct header_s));
    socklen_t bytes = recvfrom(sd, data, len, 0, (struct sockaddr *)addr, naddr);
    syslog(LOG_DEBUG, "Received %d bytes from %x:%d",
                      bytes, ADDR_IP((*addr)), ADDR_PORT((*addr)));
    if (bytes == -1) return -1;
    ifr(traffic.update.recv(p, bytes, &suspend));
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

static int attempts(struct list_s *l, struct nb_s *nb, bool *skip)
{
    if (!l || !nb || !skip) return -1;
    *skip = false;
    if (nb->attempt++ == 0) return 0;
    if (nb->attempt > nb->peer->cfg.net.max.send_retry) {
        if (nb->cmd == COMMAND_PING)
            ifr(world.peer.unreachable(nb->peer,
                                       ADDR_IP(nb->remote.addr),
                                       ADDR_PORT(nb->remote.addr)));
        *skip = true;
        return list.del(l, nb);
    } else if (nb->attempt % nb->peer->cfg.net.interval.resend != 0)
        *skip = true;
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
            syslog(LOG_DEBUG, "Sending packet %d of group %d %ld bytes to %x:%d attempt %d",
                              nb->pidx, nb->gidx, bytes,
                              ADDR_IP(nb->remote.addr),
                              ADDR_PORT(nb->remote.addr),
                              nb->attempt);
            if (nb->status == NET_ONESHOT) return list.del(l, nb);
            if (bytes <= 0) syslog(LOG_ERR, "Dispatch error: %s", strerror(errno));
            nb->status = NET_ACK_WAITING;
            nb->write  = &ev->write;
            return 0;
        }

        struct nb_s     *nb = (struct nb_s *)unb;
        struct net_ev_s *ev = (struct net_ev_s *)uev;
        bool skip;
        ifr(attempts(l, nb, &skip));
        if (skip) return 0;
        struct peer_s *p = nb->peer;
        bool suspend;
        ifr(traffic.update.send(p, nb->buffer.offset, &suspend));
        if (suspend) return 1;
        if (item(l, ev, nb, nb->pidx) != 0) return -1;
        return 0;
    }
    ev_io_stop(ev->loop, &ev->write);
    ev_timer_stop(ev->loop, &ev->send);
    if (list.map(&ns->nbl, cb, ev) != 0) return -1;
    ev_timer_again(ev->loop, &ev->send);
    return 0;
}

static void retry(struct ev_loop *loop, struct ev_timer *timer, int revents)
{
    struct peer_s *p = (struct peer_s *)timer->data;
    int sz;
    if (list.size(&p->send.nbl, &sz) != 0) return;
    if (sz > 0) ev_io_start(loop, &p->ev.write);
    bool suspend;
    if (traffic.update.recv(p, 0, &suspend) != 0) return;
    if (!suspend) net.resume(&p->ev);
}

static int resume(struct net_ev_s *ev)
{
    if (!ev) return -1;
    if (ev->read.started) return 0;
    ev_io_start(ev->loop, &ev->read.ev);
    ev->read.started = true;
    return 0;
}

static int suspend(struct net_ev_s *ev)
{
    if (!ev) return -1;
    if (!ev->read.started) return 0;
    ev_io_stop(ev->loop, &ev->read.ev);
    ev->read.started = false;
    return 0;
}

const struct module_net_s net = {
    .dispatch = dispatch,
    .receive  = receive,
    .ack      = ack,
    .retry    = retry,
    .resume   = resume,
    .suspend  = suspend,
    .nb.clean = nb_clean,
};
