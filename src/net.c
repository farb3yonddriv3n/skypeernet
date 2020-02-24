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
    //syslog(LOG_DEBUG, "Received %d bytes from %x:%d\n",
    //                  bytes, ADDR_IP((*addr)), ADDR_PORT((*addr)));
    if (bytes == -1) return -1;
    ifr(traffic.update.recv(p, bytes, &suspend));
    return 0;
}

static int nb_clean(void *unb)
{
    if (!unb) return -1;
    struct nb_s *nb = (struct nb_s *)unb;
    ifr(task.update(nb->peer));
    free(nb);
    return 0;
}

static int unique_key(char *key, int nkey, int *keylen, int pidx, int tidx)
{
    if (!key || !nkey) return -1;
    return unique_together(key, nkey, keylen, 2,
                           (void *)&pidx, sizeof(pidx),
                           (void *)&tidx, sizeof(tidx));
}

static int ack(struct net_send_s *ns, int tidx, int pidx)
{
    char key[128];
    int nkey;
    ifr(unique_key(key, sizeof(key), &nkey, pidx, tidx));
    return list.column.del(&ns->nbl, "pidx_tidx", key, nkey);
}

static int attempts(struct list_s *l, struct nb_s *nb, bool *skip)
{
    if (!l || !nb || !skip) return -1;
    *skip = false;
    if (!(nb->peer->cfg.net.sendtable[nb->attempt % nb->peer->cfg.net.max.send_retry]))
        *skip = true;
    if (++nb->attempt > nb->peer->cfg.net.max.send_retry) {
        *skip = true;
        return ack(&nb->peer->send, nb->tidx, nb->pidx);
    }
    return 0;
}

static int dispatch(struct list_s *l)
{
    int cb(struct list_s *l, void *unb, void *uev)
    {
        int item(struct list_s *l, struct nb_s *nb, int idx)
        {
            ssize_t bytes = sendto(nb->sd,
                                   nb->buffer.s,
                                   nb->buffer.offset,
                                   0,
                                   (struct sockaddr *)&nb->remote.addr,
                                   nb->remote.len);
            //syslog(LOG_DEBUG, "Sending packet %d of group %d and tidx %d %ld bytes to %x:%d attempt %d\n",
            //                  nb->pidx, nb->gidx, nb->tidx, bytes,
            //                  ADDR_IP(nb->remote.addr),
            //                  ADDR_PORT(nb->remote.addr),
            //                  nb->attempt);
            if (nb->status == NET_ONESHOT) return list.del(l, nb);
            if (bytes <= 0) syslog(LOG_ERR, "Dispatch error: %s", strerror(errno));
            return 0;
        }

        struct nb_s *nb = (struct nb_s *)unb;
        bool skip;
        ifr(attempts(l, nb, &skip));
        if (skip) return 0;
        struct peer_s *p = nb->peer;
        bool suspend;
        ifr(traffic.update.send(p, nb->buffer.offset, &suspend));
        if (suspend) return 1;
        ifr(item(l, nb, nb->pidx));
        return 0;
    }
    return list.map(l, cb, NULL);
}

static void retry(struct ev_loop *loop, struct ev_timer *timer, int revents)
{
    struct peer_s *p = (struct peer_s *)timer->data;
    bool suspend;
    if (traffic.update.recv(p, 0, &suspend) != 0) return;
    if (!suspend) net.resume(&p->ev);
    int sz;
    if (list.size(&p->send.nbl, &sz) != 0) return;
    if (sz > 0) {
        ev_timer_stop(loop, timer);
        ev_io_start(loop, &p->ev.write);
    }
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
