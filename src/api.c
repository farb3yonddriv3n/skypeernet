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
        int n = write(pipe, (void *)&ab->length, sizeof(ab->length));
        if (n != sizeof(ab->length)) return -1;
        n = write(pipe, ab->buffer, ab->length);
        if (n != ab->length) return -1;
        return 0;
    }
    list.map(&p->api.write.buffer, cb, &p->api.pipes.write);
    list.clean(&p->api.write.buffer);
}

static int write_reopen(struct peer_s *p, bool *success)
{
    if (!p || !success) return -1;
    *success = true;
    if (p->api.pipes.write > 2) return 0;
    ifr(os.pipe_open(p, p->cfg.api.pipes.write, O_WRONLY|O_NONBLOCK,
                     &p->api.pipes.write, &p->api.ev.write, EV_WRITE,
                     api.ev.write));
    if (p->api.pipes.write == -1) *success = false;
    return 0;
}

static int api_write(struct peer_s *p, enum api_e cmd, json_object *payload,
                     json_object *request, int dfserr)
{
    if (!p) return -1;
    bool success;
    ifr(write_reopen(p, &success));
    if (!success) {
        if (payload) json_object_put(payload);
        return 0;
    }
    json_object *obj  = json_object_new_object();
    json_object *jcmd = json_object_new_int(cmd);
    json_object *err  = json_object_new_int(dfserr);
    double timems;
    ifr(os.gettimems(&timems));
    json_object *time = json_object_new_double(timems);
    json_object_object_add(obj, "command", jcmd);
    json_object_object_add(obj, "time",    time);
    json_object_object_add(obj, "payload", payload);
    json_object_object_add(obj, "error",   err);
    json_object_object_add(obj, "request", request);
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
    ifr(list.add(&p->api.write.buffer, ab, ab_clean));
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
    return api.write(p, change, obj, NULL, 0);
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
    char hoststr[32];
    snprintf(hoststr, sizeof(hoststr), "%x", host);
    json_object *jhost = json_object_new_string(hoststr);
    json_object_object_add(obj, "host", jhost);
    json_object *jport = json_object_new_int(port);
    json_object_object_add(obj, "port", jport);
    json_object *message = json_object_new_string_len((const char *)msg, len);
    json_object_object_add(obj, "message", message);
    return api.write(p, API_MESSAGE, obj, NULL, 0);
}

static int api_listpeers_read(struct peer_s *p, json_object *obj)
{
    if (!p) return -1;
    json_object *pobj;
    ifr(cli.peers.list(p, &pobj));
    return api.write(p, API_LISTPEERS, pobj, obj, 0);
}

int api_job_done(struct peer_s *p, unsigned char *filename,
                 int nfilename)
{
    if (!p || !filename) return -1;
    json_object *obj = json_object_new_object();
    json_object *name = json_object_new_string_len((const char *)filename, nfilename);
    json_object_object_add(obj, "name", name);
    return api.write(p, API_JOBDONE, obj, NULL, 0);
}

static int api_jobfinalize_read(struct peer_s *p, json_object *obj)
{
    if (!p || !obj) return -1;
    char **argv;
    int argc = 2;
    argv = malloc(sizeof(char *) * argc);
    char name[256];
    json_object *tmp;
    BIND_STRLEN(name, "name", tmp, obj);
    argv[0] = NULL;
    argv[1] = name;
    int dfserr = 0;
    ifr(dfs_job_finalize(p->user.data, argv, argc, &dfserr));
    free(argv);
    return api.write(p, API_JOBFINALIZE, NULL, obj, dfserr);
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

static int api_listfileslocal_read(struct peer_s *p, json_object *obj)
{
    if (!p) return -1;
    struct distfs_s *dfs = (struct distfs_s *)p->user.data;
    json_object *robj;
    ifr(root.dump(dfs->blocks.local, &p->cfg, &robj));
    return api.write(p, API_LISTFILES_LOCAL, robj, obj, 0);
}

static int api_listfilesremote_read(struct peer_s *p, json_object *obj)
{
    if (!p) return -1;
    struct distfs_s *dfs = (struct distfs_s *)p->user.data;
    json_object *gobj;
    ifr(group.dump(dfs->blocks.remote, &p->cfg, &gobj));
    return api.write(p, API_LISTFILES_REMOTE, gobj, obj, 0);
}

static int api_jobsdump_read(struct peer_s *p, json_object *obj)
{
    if (!p) return -1;
    struct distfs_s *dfs = (struct distfs_s *)p->user.data;
    json_object *jobj;
    ifr(job.dump(&p->cfg, &dfs->jobs, &jobj));
    return api.write(p, API_JOBSDUMP, jobj, obj, 0);
}

static int api_jobadd_read(struct peer_s *p, json_object *obj)
{
    if (!p || !obj) return -1;
    char **argv;
    int argc = 2;
    argv = malloc(sizeof(char *) * argc);
    char name[256];
    json_object *tmp;
    BIND_STRLEN(name, "name", tmp, obj);
    argv[0] = NULL;
    argv[1] = name;
    int dfserr = 0;
    ifr(dfs_job_add(p->user.data, argv, argc, &dfserr));
    free(argv);
    return api.write(p, API_JOBADD, NULL, obj, dfserr);
}

static int api_tshare_read(struct peer_s *p, json_object *obj)
{
    if (!p || !obj) return -1;
    char **argv;
    int argc = 2;
    argv = malloc(sizeof(char *) * argc);
    char name[256];
    json_object *tmp;
    BIND_STRLEN(name, "name", tmp, obj);
    argv[0] = NULL;
    argv[1] = name;
    int dfserr = 0;
    ifr(dfs_transaction_share(p->user.data, argv, argc, &dfserr));
    free(argv);
    return api.write(p, API_TSHARE, NULL, obj, dfserr);
}

static int api_bmine_read(struct peer_s *p, json_object *obj)
{
    if (!p || !obj) return -1;
    int dfserr = 0;
    ifr(dfs_block_mine(p->user.data, NULL, 0, &dfserr));
    return api.write(p, API_BMINE, NULL, obj, dfserr);
}

static int api_badvertise_read(struct peer_s *p, json_object *obj)
{
    if (!p || !obj) return -1;
    char **argv;
    int argc = 2;
    argv = malloc(sizeof(char *) * argc);
    argv[0] = NULL;
    argv[1] = "a";
    int dfserr = 0;
    ifr(dfs_block_xet(p->user.data, argv, argc, &dfserr));
    free(argv);
    return api.write(p, API_BADVERTISE, NULL, obj, dfserr);
}

static int api_bmining_read(struct peer_s *p, json_object *obj)
{
    if (!p) return -1;
    json_object *bobj = json_object_new_object();
    int dfserr = 0;
    bool locked;
    ifr(dfs_block_mining(p->user.data, &locked));
    json_object *jblocked = json_object_new_boolean(locked);
    json_object_object_add(bobj, "locked", jblocked);
    return api.write(p, API_BMINING, bobj, obj, dfserr);
}

static int api_roguedump_read(struct peer_s *p, json_object *obj)
{
    if (!p) return -1;
    json_object *jobj;
    ifr(rogue.dump(&p->rogue, &jobj));
    return api.write(p, API_ROGUEDUMP, jobj, obj, 0);
}

static int api_versiondump_read(struct peer_s *p, json_object *obj)
{
    if (!p) return -1;
    json_object *jobj;
    ifr(version.dump(&jobj));
    return api.write(p, API_VERSIONDUMP, jobj, obj, 0);
}

void api_update(struct ev_loop *loop, struct ev_timer *timer, int revents)
{
    struct peer_s *p = (struct peer_s *)timer->data;
    json_object *jobj;
    traffic.dump(p, &jobj);
    api.write(p, API_TRAFFICDUMP, jobj, NULL, 0);
    task.dump(p, &jobj);
    api.write(p, API_TASKDUMP, jobj, NULL, 0);
    struct distfs_s *dfs = (struct distfs_s *)p->user.data;
    job.dump(&p->cfg, &dfs->jobs, &jobj);
    api.write(p, API_JOBSDUMP, jobj, NULL, 0);
}

static struct api_command_s cmds[] = {
    { API_LISTPEERS,        api_listpeers_read       },
    { API_MESSAGE,          api_message_read         },
    { API_LISTFILES_LOCAL,  api_listfileslocal_read  },
    { API_LISTFILES_REMOTE, api_listfilesremote_read },
    { API_JOBSDUMP,         api_jobsdump_read        },
    { API_JOBADD,           api_jobadd_read          },
    { API_JOBFINALIZE,      api_jobfinalize_read     },
    { API_TSHARE,           api_tshare_read          },
    { API_BMINE,            api_bmine_read           },
    { API_BADVERTISE,       api_badvertise_read      },
    { API_BMINING,          api_bmining_read         },
    { API_ROGUEDUMP,        api_roguedump_read       },
    { API_VERSIONDUMP,      api_versiondump_read     },
};

static int api_read(struct peer_s *p, const char *json, int len)
{
    if (!p || !json) return -1;
    json_object *obj;
    if (os.loadjson(&obj, (char *)json, len) == -1) return -1;
    enum api_e cmd;
    json_object *tmp;
    BIND_INT(cmd, "command", tmp, obj);
    int i;
    for (i = 0; i < COUNTOF(cmds); i++) {
        if (cmds[i].cmd == cmd) {
            return cmds[i].read(p, obj);
        }
    }
    return -1;
}

// unalligned
static void api_ev_read(EV_P_ ev_io *w, int revents)
{
    if (!w) {
        syslog(LOG_ERR, "API read error");
        return;
    }
    struct peer_s *p = w->data;
    int n = read(p->api.pipes.read, p->api.read.buffer + p->api.read.size,
                 sizeof(p->api.read.buffer) - p->api.read.size);
    if (n <= 0) {
        ev_io_stop(p->ev.loop, &p->api.ev.read);
        if (os.pipe_open(p, p->cfg.api.pipes.read, O_RDONLY|O_NONBLOCK,
                         &p->api.pipes.read, &p->api.ev.read, EV_READ,
                         api.ev.read) != 0)
            syslog(LOG_ERR, "Pipe cannot be opened for reading");
        return;
    }

    p->api.read.size += n;
    while (p->api.read.size > 0) {
        if (p->api.read.dst == 0) {
            p->api.read.dst = *(int *)p->api.read.buffer;
            swap_memory((void *)&p->api.read.dst, sizeof(p->api.read.dst));
        }
        if ((p->api.read.size - sizeof(int)) > p->api.read.dst) {
            if (api_read(p, p->api.read.buffer + sizeof(int), p->api.read.dst) != 0)
                syslog(LOG_ERR, "API read error: %d bytes", n);
            p->api.read.size -= (p->api.read.dst + sizeof(int));
            memmove(p->api.read.buffer, p->api.read.buffer + p->api.read.dst + sizeof(int),
                    p->api.read.size);
            p->api.read.dst = 0;
        } else if ((p->api.read.size - sizeof(int)) == p->api.read.dst) {
            if (api_read(p, p->api.read.buffer + sizeof(int), p->api.read.dst) != 0)
                syslog(LOG_ERR, "API read error: %d bytes", n);
            p->api.read.dst = p->api.read.size = 0;
            break;
        } else
            break;
    }
}

const struct module_api_s api = {
    .read     = api_read,
    .write    = api_write,
    .ev.read  = api_ev_read,
    .ev.write = api_ev_write,
};
