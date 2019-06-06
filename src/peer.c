#include <common.h>

static struct peer_s *psig;

static void write_cb(EV_P_ ev_io *w, int revents)
{
    struct peer_s *p = w->data;
    net.dispatch(&p->send.nbl);
    ev_io_stop(p->ev.loop, &p->ev.write);
    ev_timer_again(p->ev.loop, &p->ev.send);
}

static void write_instant_cb(EV_P_ ev_io *w, int revents)
{
    struct peer_s *p = w->data;
    net.dispatch(&p->send.instant);
    ev_io_stop(p->ev.loop, &p->ev.write_instant);
    list.clean(&p->send.instant);
}

static void read_cb(EV_P_ ev_io *w, int revents)
{
    struct peer_s *p = w->data;
    if (net.receive(p, p->net.sd, p->recv.data, sizeof(p->recv.data),
                    &p->net.remote.addr, &p->net.remote.len) != 0) abort();
    if (p->type == INSTANCE_TRACKER &&
        ADDR_IP(p->net.remote.addr) == 0 &&
        ADDR_PORT(p->net.remote.addr) == 0) return;
    bool valid;
    if (packet.validate(p->recv.data, sizeof(p->recv.data), &valid,
                        ADDR_IP(p->net.remote.addr),
                        ADDR_PORT(p->net.remote.addr),
                        &p->received) != 0) return;
    if (!valid) return;
    if (payload.recv(p) != 0) abort();
}

static void rlhandler(char *line)
{
    if (line == NULL) {
    } else {
        if (*line != 0) {
            add_history(line);
        }
        if (cli(psig, line) != 0)
            printf("Command %s failed\n", line);
        free(line);
    }
}

static void stdin_cb(EV_P_ ev_io *w, int revents)
{
    if (revents & EV_READ) rl_callback_read_char();
}

static void sig_handler(int signo)
{
    if (signo != SIGINT) return;
    peer.clean(psig);
}

static int init_peer(struct peer_s *p)
{
    memset(p, 0, sizeof(*p));
    psig = p;
    p->type = INSTANCE_PEER;
    if (config_init(&p->cfg) != 0) return -1;
    if (os.dldir(&p->cfg) != 0) return -1;
    if ((p->net.sd = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP)) == -1)
        return -1;
    int nb = 1;
    ioctl(p->net.sd, FIONBIO, &nb);
    if (signal(SIGINT, sig_handler) == SIG_ERR) return -1;
    p->net.self.addr.sin_family      = AF_INET;
    p->net.self.addr.sin_addr.s_addr = htonl(INADDR_ANY);
    p->net.self.len                  = sizeof(p->net.self.addr);

    p->net.remote.addr.sin_family    = AF_INET;
    p->net.remote.addr.sin_port      = htons(p->cfg.net.tracker.port);
    p->net.remote.len                = sizeof(p->net.remote.addr);
    if (inet_aton(p->cfg.net.tracker.ip, &p->net.remote.addr.sin_addr) == 0)
        return -1;
    syslog(LOG_DEBUG, "Tracker %x:%d",
           ADDR_IP(p->net.remote.addr), ADDR_PORT(p->net.remote.addr));
    p->tracker.host = ADDR_IP(p->net.remote.addr);
    p->tracker.port = ADDR_PORT(p->net.remote.addr);

    rl_callback_handler_install("> ", (rl_vcpfunc_t *)&rlhandler);

    p->ev.loop = ev_default_loop(0);
    ev_io_init(&p->ev.stdinwatch, stdin_cb, fileno(stdin), EV_READ);
    ev_io_init(&p->ev.read.ev,    read_cb,  p->net.sd,     EV_READ);
    ev_io_init(&p->ev.write,      write_cb, p->net.sd,     EV_WRITE);
    ev_io_init(&p->ev.write_instant,      write_instant_cb, p->net.sd,     EV_WRITE);
    ev_timer_init(&p->ev.send,            net.retry,        .0,
                  p->cfg.net.interval.retry);
    ev_timer_init(&p->ev.peers_reachable, world.peer.check, .0,
                  p->cfg.net.interval.peers_reachable);
    p->ev.send.data = (void *)p;
    p->ev.peers_reachable.data = (void *)p;
    p->ev.stdinwatch.data = (void *)p;
    p->ev.read.ev.data  = (void *)p;
    p->ev.write.data = (void *)p;
    p->ev.write_instant.data = (void *)p;
    ev_timer_again(p->ev.loop, &p->ev.send);
    return 0;
}

static int init_tracker(struct peer_s *t)
{
    memset(t, 0, sizeof(*t));
    psig = t;
    if (signal(SIGINT, sig_handler) == SIG_ERR) return -1;
    t->type = INSTANCE_TRACKER;
    if (config_init(&t->cfg) != 0) return -1;
    t->net.sd = socket(PF_INET, SOCK_DGRAM, 0);
    t->net.self.len = sizeof(t->net.self.addr);
    t->net.self.addr.sin_family = AF_INET;
    t->net.self.addr.sin_port = htons(t->cfg.net.tracker.port);
    t->net.self.addr.sin_addr.s_addr = INADDR_ANY;
    if (bind(t->net.sd, (struct sockaddr *)&t->net.self.addr,
             sizeof(t->net.self.addr)) != 0) return -1;
    t->ev.loop = ev_default_loop(0);
    ev_io_init(&t->ev.stdinwatch, stdin_cb, fileno(stdin), EV_READ);
    ev_io_init(&t->ev.read.ev, read_cb,  t->net.sd, EV_READ);
    ev_io_init(&t->ev.write, write_cb, t->net.sd, EV_WRITE);
    ev_io_init(&t->ev.write_instant, write_instant_cb, t->net.sd, EV_WRITE);
    ev_timer_init(&t->ev.send,            net.retry,        .0,
                  t->cfg.net.interval.retry);
    ev_timer_init(&t->ev.peers_reachable, world.peer.check, .0,
                  t->cfg.net.interval.peers_reachable);
    t->ev.stdinwatch.data = t;
    t->ev.send.data  = t;
    t->ev.read.ev.data  = t;
    t->ev.write.data = t;
    t->ev.write_instant.data = t;
    t->ev.peers_reachable.data  = t;
    rl_callback_handler_install("> ", (rl_vcpfunc_t *)&rlhandler);
    ev_timer_again(t->ev.loop, &t->ev.send);
    return 0;
}

static int clean(struct peer_s *p)
{
    ev_io_stop(p->ev.loop, &p->ev.read.ev);
    ev_io_stop(p->ev.loop, &p->ev.write);
    ev_io_stop(p->ev.loop, &p->ev.stdinwatch);
    ev_timer_stop(p->ev.loop, &p->ev.send);
    ev_timer_stop(p->ev.loop, &p->ev.peers_reachable);
    list.clean(&p->peers);
    list.clean(&p->recv_buffer.cache);
    list.clean(&p->recv_buffer.sealed);
    list.clean(&p->tasks.list);
    config_free(&p->cfg);
    return 0;
}

const struct module_peer_s peer = {
    .init.mpeer    = init_peer,
    .init.mtracker = init_tracker,
    .clean         = clean,
};
