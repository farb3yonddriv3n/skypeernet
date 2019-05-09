#include <common.h>

static const struct { enum command_e cmd;
                      int (*cb_write)(struct data_s*, void*);
                      int (*cb_size)(struct data_s*, void*);
                    } cmds[] =
{
    { COMMAND_PEER_ANNOUNCE_PEER,    announce_write_peer,    announce_size },
    { COMMAND_TRACKER_ANNOUNCE_PEER, announce_write_tracker, announce_size },
    { COMMAND_MESSAGE,               message_write,          message_size },
};

static int exec(void *parent, enum command_e cmd,
                int host, unsigned short port,
                int (*cb_write)(struct data_s*, void*),
                int (*cb_size)(struct data_s*, void*),
                int sd, struct sockaddr_in *addr, int addr_len,
                struct nb_s **dt, int *ndata, struct ev_loop *loop,
                ev_io *write)
{
    struct data_s d;
    if (data.init(&d, cmd, cb_write, cb_size, parent) != 0) return -1;
    if (data.send(&d, sd, addr, addr_len, 0,
                  host, port, dt, ndata) != 0) return -1;
    ev_io_start(loop, write);
    return 0;
}

static int psend(void *parent, enum command_e cmd,
                 int host, unsigned short port,
                 int sd, struct sockaddr_in *addr, int addr_len,
                 struct nb_s **dt, int *ndata, struct ev_loop *loop,
                 struct ev_io *write)
{
    int i;
    for (i = 0; i < COUNTOF(cmds); i++) {
        if (cmd == cmds[i].cmd)
            return exec(parent, cmd, host, port,
                        cmds[i].cb_write,
                        cmds[i].cb_size, sd,
                        addr, addr_len, dt, ndata,
                        loop, write);
    }
    return -1;
}

static int send_peer(struct peer_s *p, enum command_e cmd,
                     int host, unsigned short port)
{
    return psend(p, cmd, host, port, p->net.sd, &p->net.remote.addr,
                 p->net.remote.len, &p->send.data, &p->send.len,
                 p->ev.loop, &p->ev.write);
}

static int send_tracker(struct tracker_s *t, enum command_e cmd,
                        int host, unsigned short port)
{
    return psend(t, cmd, host, port, t->net.sd, &t->net.remote.addr,
                 t->net.remote.len, &t->send.data, &t->send.len,
                 t->ev.loop, &t->ev.write);
}

const struct module_payload_s payload = {
    .send.peer    = send_peer,
    .send.tracker = send_tracker,
};
