#include <common.h>

static void message(struct peer_s *p, int host,
                    unsigned short port,
                    char *msg, int len)
{
    printf("Message %.*s from %x:%d\n", len, msg, host, port);
}

static void file(struct peer_s *p, int host,
                 unsigned short port,
                 const char *received)
{
    printf("Received file: %s\n", received);
}

static void online(struct peer_s *p, struct world_peer_s *wp)
{
    printf("New peer: %x:%d of type %d\n", wp->host, wp->port, wp->type);
}

int main()
{
    openlog("distfs:peer", LOG_PID|LOG_CONS, LOG_USER);
    struct peer_s p;
    if (peer.init.mpeer(&p) != 0) return -1;
    p.user.message = message;
    p.user.file    = file;
    p.user.online  = online;
    if (payload.send(&p, COMMAND_PEER_ANNOUNCE_PEER,
                     p.tracker.host,
                     p.tracker.port, 0, 0) != 0) return -1;
    ev_io_start(p.ev.loop, &p.ev.stdinwatch);
    ifr(net.resume(&p.ev));
    ev_timer_again(p.ev.loop, &p.ev.peers_reachable);
    ev_loop(p.ev.loop, 0);
    ev_loop_destroy(p.ev.loop);
    close(p.net.sd);
    closelog();
    rl_callback_handler_remove();
    return 0;
}
