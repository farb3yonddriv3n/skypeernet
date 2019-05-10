#include <common.h>

static void write_cb(EV_P_ ev_io *w, int revents)
{
    struct peer_s *p = w->data;
    net.dispatch(&p->ev, &p->send);
}

static void read_cb(EV_P_ ev_io *w, int revents)
{
    struct peer_s *p = w->data;
    socklen_t bytes = recvfrom(p->net.sd, p->recv.data, sizeof(p->recv.data), 0,
                               (struct sockaddr *)&p->net.remote.addr,
                               &p->net.remote.len);
    if (bytes == -1) return;
    bool valid;
    if (packet.validate(p->recv.data, sizeof(p->recv.data), &valid,
                        &p->recieved) != 0)
        return;
    printf("received valid packet: %d\n", valid);
    if (!valid) return;
    if (world.parse(p) != 0) return;

    // Do not ack the ack command
    if (p->header.command != COMMAND_ACK)
        if (payload.send.peer(p, COMMAND_ACK,
                              ADDR_IP(p->net.remote.addr),
                              ADDR_PORT(p->net.remote.addr)) != 0) return;
}

void rlhandler(char *line)
{
    if (line == NULL) {
    } else {
        if (*line != 0) {
            add_history(line);
        }
        printf("\n%s\n", line);
        free(line);
    }
}

static void stdin_cb(EV_P_ ev_io *w, int revents)
{
    if (revents & EV_READ) rl_callback_read_char();
}

static int init(struct peer_s *p)
{
    memset(p, 0, sizeof(*p));
    if ((p->net.sd = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP)) == -1)
        return -1;

    p->net.self.addr.sin_family      = AF_INET;
    p->net.self.addr.sin_addr.s_addr = htonl(INADDR_ANY);
    p->net.self.len                  = sizeof(p->net.self.addr);

    p->net.remote.addr.sin_family    = AF_INET;
    p->net.remote.addr.sin_port      = htons(TRACKER_PORT);
    p->net.remote.len                = sizeof(p->net.remote.addr);
    if (inet_aton(TRACKER_HOST, &p->net.remote.addr.sin_addr) == 0)
        return -1;

    p->tracker.host = ADDR_IP(p->net.remote.addr);
    p->tracker.port = ADDR_PORT(p->net.remote.addr.sin_port);

    rl_callback_handler_install("> ", (rl_vcpfunc_t *)&rlhandler);

    p->ev.loop = ev_default_loop(0);
    ev_io_init(&p->ev.stdinwatch, stdin_cb, fileno(stdin), EV_READ);
    ev_io_init(&p->ev.read,       read_cb,  p->net.sd,     EV_READ);
    ev_io_init(&p->ev.write,      write_cb, p->net.sd,     EV_WRITE);
    ev_init(&p->send.timer, net_timer);
    p->send.timer.repeat = 2.0;
    p->send.timer.data = (void *)p;
    p->ev.stdinwatch.data = (void *)p;
    p->ev.read.data  = (void *)p;
    p->ev.write.data = (void *)p;
    return 0;
}

int main()
{
    struct peer_s p;
    if (init(&p) != 0) return -1;
    if (payload.send.peer(&p, COMMAND_PEER_ANNOUNCE_PEER,
                          p->tracker.host,
                          p->tracker.port) != 0) return -1;
    ev_io_start(p.ev.loop, &p.ev.stdinwatch);
    ev_io_start(p.ev.loop, &p.ev.read);
    ev_loop(p.ev.loop, 0);
    close(p.net.sd);
    return 0;
}
