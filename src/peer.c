#include <common.h>

static void write_cb(EV_P_ ev_io *w, int revents)
{
    struct peer_s *p = w->data;
    net_send(&p->send.data, &p->send.len);
    ev_io_stop(p->ev.loop, &p->ev.write);
}

static void read_cb(EV_P_ ev_io *w, int revents)
{
    struct peer_s *p = w->data;
    socklen_t bytes = recvfrom(p->net.sd, p->recv.data, sizeof(p->recv.data), 0,
                               (struct sockaddr *)&p->net.remote.addr,
                               &p->net.remote.len);
    if (bytes == -1) return;
    bool valid;
    struct packet_s pck;
    if (packet.validate(p->recv.data, sizeof(p->recv.data), &valid, &pck) != 0)
        return;
    printf("received valid packet: %d\n", valid);
    if (!valid) return;
    if (world.parse(p, &pck) != 0) {
        abort();
        return;
    }
}

void rlhandler(char *line)
{
    if (line==NULL) {
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
    if (revents & EV_READ) {
        rl_callback_read_char();
    }
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

    p->tracker.host = p->net.remote.addr.sin_addr.s_addr;
    p->tracker.port = p->net.remote.addr.sin_port;

    p->ev.loop = ev_default_loop(0);
    rl_callback_handler_install("> ", (rl_vcpfunc_t *)&rlhandler);
    ev_io_init(&p->ev.stdinwatch, stdin_cb, fileno(stdin), EV_READ);
    ev_io_init(&p->ev.read,       read_cb,  p->net.sd,     EV_READ);
    ev_io_init(&p->ev.write,      write_cb, p->net.sd,     EV_WRITE);
    p->ev.read.data  = (void *)p;
    p->ev.write.data = (void *)p;
    return 0;
}

int main()
{
    struct peer_s p;
    if (init(&p) != 0) return -1;
    if (payload.send.peer(&p, COMMAND_PEER_ANNOUNCE_PEER,
                          NET_IP(p.net.remote.addr),
                          NET_PORT(p.net.remote.addr)) != 0) return -1;
    ev_io_start(p.ev.loop, &p.ev.stdinwatch);
    ev_io_start(p.ev.loop, &p.ev.read);
    ev_loop(p.ev.loop, 0);
    close(p.net.sd);
    return 0;
}
