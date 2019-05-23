#include <common.h>

int main()
{
    openlog("distfs:tracker", LOG_PID|LOG_CONS, LOG_DAEMON);
    struct peer_s t;
    if (peer.init.mtracker(&t) != 0) return -1;
    ev_io_start(t.ev.loop, &t.ev.stdinwatch);
    ev_io_start(t.ev.loop, &t.ev.read);
    ev_timer_again(t.ev.loop, &t.ev.peers_reachable);
    ev_loop(t.ev.loop, 0);
    close(t.net.sd);
    closelog();
    rl_callback_handler_remove();
    return 0;
}
