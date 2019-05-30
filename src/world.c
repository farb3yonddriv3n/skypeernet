#include <common.h>

static int reachable_set(struct peer_s *p, int host, unsigned short port,
                         bool reachable)
{
    struct world_peer_s wp = { .host = host, .port = port, .found = NULL };
    ifr(list.map(&p->peers, peer_find, &wp));
    if (wp.found) {
        if (!reachable && ++wp.found->unreachable == p->cfg.net.max.peer_unreachable) {
            ifr(list.del(&p->peers, wp.found));
        } else if (reachable) wp.found->unreachable = 0;
    }
    return 0;
}

static int peer_unreachable(struct peer_s *p, int host, unsigned short port)
{
    return reachable_set(p, host, port, false);
}

static int peer_reachable(struct peer_s *p, int host, unsigned short port)
{
    return reachable_set(p, host, port, true);
}

static void peer_check(struct ev_loop *loop, struct ev_timer *timer, int revents)
{
    struct peer_s *p = (struct peer_s *)timer->data;
    if (!p) return;
    ev_timer_stop(p->ev.loop, &p->ev.peers_reachable);
    int cb(struct list_s *l, void *un, void *ud) {
        struct peer_s       *p  = (struct peer_s *)ud;
        struct world_peer_s *wp = (struct world_peer_s *)un;
        syslog(LOG_DEBUG, "Checking peer's availability: %x:%d", wp->host, wp->port);
        return payload.send(p, COMMAND_PING,
                            wp->host,
                            wp->port, 0, 0);
    }
    assert(list.map(&p->peers, cb, p) == 0);
    ev_timer_again(p->ev.loop, &p->ev.peers_reachable);
}

const struct module_world_s world = {
    .peer.reachable   = peer_reachable,
    .peer.unreachable = peer_unreachable,
    .peer.check       = peer_check,
};
