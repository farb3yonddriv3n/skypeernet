#include <common.h>

struct api_buffer_s {
    char *buffer;
    int   length;
};

struct api_command_s {
    enum api_e cmd;
    int (*read)(struct peer_s *p, json_object *obj);
};

static int ab_clean(void *ud)
{
    if (!ud) return -1;
    struct api_buffer_s *ab = (struct api_buffer_s *)ud;
    free(ab->buffer);
    free(ab);
    return 0;
}

static void api_ev_write(EV_P_ ev_io *w, int revents)
{
    if (!w) return;
    struct peer_s *p = (struct peer_s *)w->data;
    ev_io_stop(p->ev.loop, &p->api.ev.write);
    if (revents & EV_READ) {
        p->api.pipes.write = -1;
        return;
    }
    int cb(struct list_s *l, void *ue, void *ud) {
        struct api_buffer_s *ab   = (struct api_buffer_s *)ue;
        int                  pipe = *(int *)ud;
        int r = write(pipe, ab->buffer, ab->length);
        if (r != ab->length) return -1;
        return 0;
    }
    list.map(&p->api.buffer, cb, &p->api.pipes.write);
    list.clean(&p->api.buffer);
}

static int write_reopen(struct peer_s *p, bool *success)
{
    if (!p || !success) return -1;
    *success = true;
    if (p->api.pipes.write > 2) return 0;
    ifr(os.pipe_open(p, p->cfg.api.pipes.write, O_WRONLY|O_NONBLOCK,
                     &p->api.pipes.write, &p->api.ev.write, EV_WRITE|EV_READ,
                     api.ev.write));
    if (p->api.pipes.write == -1) *success = false;
    return 0;
}

static int api_write(struct peer_s *p, enum api_e cmd, json_object *payload)
{
    if (!p || !payload) return -1;
    bool success;
    ifr(write_reopen(p, &success));
    if (!success) {
        json_object_put(payload);
        return 0;
    }
    json_object *obj  = json_object_new_object();
    json_object *jcmd = json_object_new_int(cmd);
    json_object_object_add(obj, "command", jcmd);
    json_object_object_add(obj, "payload", payload);
    const char *json = json_object_to_json_string(obj);
    if (!json) return -1;
    int len = strlen(json);
    struct api_buffer_s *ab = malloc(sizeof(*ab));
    if (!ab) return -1;
    ab->buffer = malloc(len);
    if (!ab->buffer) return -1;
    memcpy(ab->buffer, json, len);
    ab->length = len;
    json_object_put(obj);
    ifr(list.add(&p->api.buffer, ab, ab_clean));
    ev_io_start(p->ev.loop, &p->api.ev.write);
    return 0;
}

static int api_peer_change(struct peer_s *p, struct world_peer_s *wp,
                           enum api_e change)
{
    if (!p || !wp) return -1;
    json_object *obj = json_object_new_object();
    json_object *type = json_object_new_int(wp->type);
    json_object_object_add(obj, "type", type);
    char hoststr[32];
    snprintf(hoststr, sizeof(hoststr), "%x", wp->host);
    json_object *host = json_object_new_string(hoststr);
    json_object_object_add(obj, "host", host);
    json_object *port = json_object_new_int(wp->port);
    json_object_object_add(obj, "port", port);
    json_object *pubkeyhash = json_object_new_string_len((const char *)wp->pubkeyhash,
                                                         sizeof(wp->pubkeyhash));
    json_object_object_add(obj, "pubkeyhash", pubkeyhash);
    return api.write(p, change, obj);
}

int api_peer_online(struct peer_s *p, struct world_peer_s *wp)
{
    return api_peer_change(p, wp, API_PEER_ONLINE);
}

int api_peer_offline(struct peer_s *p, struct world_peer_s *wp)
{
    return api_peer_change(p, wp, API_PEER_OFFLINE);
}

int api_message_write(struct peer_s *p, int host, unsigned short port,
                      char *msg, int len)
{
    if (!p || !msg) return -1;
    json_object *obj = json_object_new_object();
    json_object *jhost = json_object_new_int(host);
    json_object_object_add(obj, "host", jhost);
    json_object *jport = json_object_new_int(port);
    json_object_object_add(obj, "port", jport);
    json_object *message = json_object_new_string_len((const char *)msg, len);
    json_object_object_add(obj, "message", message);
    return api.write(p, API_MESSAGE, obj);
}

static int api_listpeers_read(struct peer_s *p, json_object *obj)
{
    if (!p) return -1;
    json_object *pobj;
    ifr(cli.peers.list(p, &pobj));
    return api.write(p, API_LISTPEERS, pobj);
}

static int api_message_read(struct peer_s *p, json_object *obj)
{
    if (!p || !obj) return -1;
    char msg[256];
    char hoststr[64];
    unsigned short port;
    json_object *tmp;
    BIND_STRLEN(hoststr, "host",    tmp, obj);
    BIND_INT(port,       "port",    tmp, obj);
    BIND_STRLEN(msg,     "message", tmp, obj);
    long int host = strtol(hoststr, NULL, 10);
    p->send_buffer.type = BUFFER_MESSAGE;
    p->send_buffer.u.message.str = msg;
    return payload.send(p, COMMAND_MESSAGE,
                        host, port, 0, 0, NULL);
}

static int api_listfiles_read(struct peer_s *p, json_object *obj)
{
    if (!p) return -1;
    struct distfs_s *dfs = (struct distfs_s *)p->user.data;
    json_object *gobj;
    group.dump(dfs->blocks.remote, &p->cfg, &gobj);
    return api.write(p, API_LISTFILES, gobj);
}

static struct api_command_s cmds[] = {
    { API_LISTPEERS, api_listpeers_read },
    { API_MESSAGE,   api_message_read   },
    { API_LISTFILES, api_listfiles_read },
};

static int api_read(struct peer_s *p, const char *json, int len)
{
    if (!p || !json) return -1;
    json_object *obj;
    if (os.loadjson(&obj, (char *)json, len) == -1) return -1;
    enum api_e cmd;
    json_object *tmp;
    BIND_INT(cmd, "command", tmp, obj);
    int i, r = -1;
    for (i = 0; i < COUNTOF(cmds); i++) {
        if (cmds[i].cmd == cmd) {
            r = cmds[i].read(p, obj);
            break;
        }
    }
    json_object_put(obj);
    return r;
}

static void api_ev_read(EV_P_ ev_io *w, int revents)
{
    if (!w) {
        syslog(LOG_ERR, "API read error");
        return;
    }
    struct peer_s *p = w->data;
    char buffer[512];
    int n = read(p->api.pipes.read, buffer, sizeof(buffer));
    if (n == 0) {
        ev_io_stop(p->ev.loop, &p->api.ev.read);
        if (os.pipe_open(p, p->cfg.api.pipes.read, O_RDONLY|O_NONBLOCK,
                         &p->api.pipes.read, &p->api.ev.read, EV_READ,
                         api.ev.read) != 0)
            syslog(LOG_ERR, "Pipe cannot be opened for reading");
        return;
    }
    if (api_read(p, buffer, n) != 0)
        syslog(LOG_ERR, "API read error: %.*s", n, buffer);
}

const struct module_api_s api = {
    .read     = api_read,
    .write    = api_write,
    .ev.read  = api_ev_read,
    .ev.write = api_ev_write,
};
