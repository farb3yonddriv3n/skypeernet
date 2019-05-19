#include <common.h>

int main()
{
    openlog("distfs:tracker", LOG_PID|LOG_CONS, LOG_DAEMON);
    struct peer_s p;
    if (peer.init.mtracker(&p) != 0) return -1;
    ev_io_start(p.ev.loop, &p.ev.read);
    ev_loop(p.ev.loop, 0);
    close(p.net.sd);
    closelog();
    return 0;
}
