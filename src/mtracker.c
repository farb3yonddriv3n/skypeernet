#include <common.h>

static int dfs_auth_reply(struct peer_s *p, int host,
                          unsigned short port,
                          char *data, int len)
{
    if (!p || !data) return -1;
    struct world_peer_s wp = { .found = NULL };
    if (len != sizeof(wp.authstr)) return -1;
    memcpy(wp.authstr, data, len);
    ifr(list.map(&p->peers, world.peer.findauthstr, &wp));
    if (wp.found && wp.found->authed != true) {
        wp.found->authed = true;
        p->send_buffer.type = BUFFER_NONE;
        if (payload.send(p, COMMAND_TRACKER_ANNOUNCE_TRACKER,
                         wp.found->host, wp.found->port,
                         0, 0, NULL, NULL) != 0) return -1;
        ifr(world.peer.broadcast(p, wp.found));
    }
    return 0;
}

static int init(struct peer_s *p, struct distfs_s *dfs)
{
    if (!p || !dfs) return -1;
    memset(dfs, 0, sizeof(*dfs));
    p->user.cb.authrpl = dfs_auth_reply;
    p->user.data       = dfs;
    dfs->peer          = p;
    return 0;
}

int main()
{
    openlog("distfs:tracker", LOG_PID|LOG_CONS, LOG_DAEMON);
    struct peer_s t;
    struct distfs_s dfs;
    if (peer.init.mtracker(&t) != 0) return -1;
    if (init(&t, &dfs) != 0) return -1;
    ev_io_start(t.ev.loop, &t.ev.stdinwatch);
    ifr(net.resume(&t.ev));
    ev_timer_again(t.ev.loop, &t.ev.peers_reachable);
    ev_loop(t.ev.loop, 0);
    ev_loop_destroy(t.ev.loop);
    close(t.net.sd);
    closelog();
    rl_callback_handler_remove();
    return 0;
}
