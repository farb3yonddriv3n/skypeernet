#include <common.h>

static const struct { enum command_e cmd;
                      int (*cb_write)(struct data_s*, void*);
                      int (*cb_size)(int*, void*);
                    } cmds[] =
{
    { COMMAND_NONE,                  NULL,                   NULL },
    { COMMAND_ACK,                   ack_write,              ack_size },
    { COMMAND_TRACKER_ANNOUNCE_PEER, announce_write_tracker, announce_size },
    { COMMAND_PEER_ANNOUNCE_PEER,    announce_write_peer,    announce_size },
    { COMMAND_MESSAGE,               message_write,          message_size },
    { COMMAND_FILE,                  file_write,             file_size },
    { COMMAND_FILE_SEND,             file_send_write,        file_send_size },
};

static int exec(struct instance_s *parent, enum command_e cmd,
                int host, unsigned short port,
                int (*cb_write)(struct data_s*, void*),
                int (*cb_size)(int*, void*))
{
    struct data_s d;
    if (data.init(&d, cmd, cb_write, cb_size, parent) != 0) return -1;
    if (data.send(&d, parent, host, port) != 0) return -1;
    ev_io_start(parent->ev.loop, &parent->ev.write);
    return 0;
}

static int payload_send(void *parent, enum command_e cmd,
                        int host, unsigned short port)
{
    int i;
    for (i = 0; i < COUNTOF(cmds); i++) {
        if (cmd == cmds[i].cmd)
            return exec(parent, cmd, host, port,
                        cmds[i].cb_write,
                        cmds[i].cb_size);
    }
    return -1;
}

/*
static int send_peer(struct peer_s *p, enum command_e cmd,
                     int host, unsigned short port)
{
    return payload_send(p, cmd, host, port, &p->net, &p->send, &p->ev);
}

static int send_tracker(struct tracker_s *t, enum command_e cmd,
                        int host, unsigned short port)
{
    return payload_send(t, cmd, host, port, &t->net, &t->send, &t->ev);
}
*/
const struct module_payload_s payload = {
    .send = payload_send,
};
