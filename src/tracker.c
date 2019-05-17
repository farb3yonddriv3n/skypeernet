#include <common.h>

static void write_cb(EV_P_ ev_io *w, int revents)
{
    struct tracker_s *t = w->data;
    net.dispatch(&t->ev, &t->send);
}

static void read_cb(EV_P_ ev_io *w, int revents)
{
    struct tracker_s *t = w->data;
    if (!t) return;
    net.receive(t->net.sd, t->recv.data, sizeof(t->recv.data),
                &t->net.remote.addr, &t->net.remote.len);
    if (ADDR_IP(t->net.remote.addr) == 0 &&
        ADDR_PORT(t->net.remote.addr) == 0) return;
    bool valid;
    if (packet.validate(t->recv.data, sizeof(t->recv.data), &valid,
                        ADDR_IP(t->net.remote.addr),
                        ADDR_PORT(t->net.remote.addr),
                        &t->received) != 0) return;
    if (!valid) return;
    world.handle((struct instance_s *)t);
}

static int init(struct tracker_s *t)
{
    memset(t, 0, sizeof(*t));
    t->type = INSTANCE_TRACKER;
    t->net.sd = socket(PF_INET, SOCK_DGRAM, 0);
    t->net.self.len = sizeof(t->net.self.addr);
    t->net.self.addr.sin_family = AF_INET;
    t->net.self.addr.sin_port = htons(TRACKER_PORT);
    t->net.self.addr.sin_addr.s_addr = INADDR_ANY;
    if (bind(t->net.sd, (struct sockaddr *)&t->net.self.addr,
             sizeof(t->net.self.addr)) != 0) return -1;
    t->ev.loop = ev_default_loop(0);
    ev_io_init(&t->ev.read,  read_cb,  t->net.sd, EV_READ);
    ev_io_init(&t->ev.write, write_cb, t->net.sd, EV_WRITE);
    t->ev.read.data  = t;
    t->ev.write.data = t;
    return 0;
}

int main()
{
    openlog("distfs:tracker", LOG_PID|LOG_CONS, LOG_DAEMON);
    struct tracker_s t;
    if (init(&t) != 0) return -1;
    ev_io_start(t.ev.loop, &t.ev.read);
    ev_loop(t.ev.loop, 0);
    close(t.net.sd);
    closelog();
    return 0;
}
