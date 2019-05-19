#include <common.h>

int main()
{
    openlog("distfs:peer", LOG_PID|LOG_CONS, LOG_USER);
    struct peer_s p;
    if (peer.init.mpeer(&p) != 0) return -1;
    if (payload.send(&p, COMMAND_PEER_ANNOUNCE_PEER,
                     p.tracker.host,
                     p.tracker.port) != 0) return -1;
    ev_io_start(p.ev.loop, &p.ev.stdinwatch);
    ev_io_start(p.ev.loop, &p.ev.read);
    ev_loop(p.ev.loop, 0);
    close(p.net.sd);
    closelog();
    rl_callback_handler_remove();
    return 0;
}
