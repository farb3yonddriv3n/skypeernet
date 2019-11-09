#include <common.h>

static int send_message(struct peer_s *p, char **argv, int argc)
{
    long int    host    = strtol(argv[1], NULL, 16);
    long int    port    = strtol(argv[2], NULL, 10);
    const char *message = argv[3];
    p->send_buffer.type = BUFFER_MESSAGE;
    p->send_buffer.u.message.str = message;
    return payload.send(p, COMMAND_MESSAGE,
                        host, port, 0, 0,
                        NULL, NULL);
}

static int whoami(struct peer_s *p, char **argv, int argc)
{
    printf("%x:%d \33[1;31m%.*s\33[m\n",
           ADDR_IP(p->net.self.addr),
           ADDR_PORT(p->net.self.addr),
           (int )sizeof(p->cfg.keys.local.hash.public),
           p->cfg.keys.local.hash.public);
    return 0;
}

static int send_file(struct peer_s *p, char **argv, int argc)
{
    long int    host = strtol(argv[1], NULL, 16);
    long int    port = strtol(argv[2], NULL, 10);
    const char *file = argv[3];
    return task.add(p, p->cfg.dir.download, (unsigned char *)file, strlen(file),
                    host, port, NULL, TASK_FILE_KEEP, 0);
}

static int cli_peers_list(struct peer_s *p, json_object **obj)
{
    if (!p || !obj) return -1;
    int cb(struct list_s *l, void *uwp, void *ud) {
        if (!uwp || !ud) return -1;
        struct world_peer_s *wp    = (struct world_peer_s *)uwp;
        json_object         *peers = (json_object *)ud;
        json_object         *obj   = json_object_new_object();
        char hexhost[32];
        snprintf(hexhost, sizeof(hexhost), "%x", wp->host);
        json_object *host = json_object_new_string(hexhost);
        json_object_object_add(obj, "host", host);
        json_object *port = json_object_new_int(wp->port);
        json_object_object_add(obj, "port", port);
        json_object *proxy = json_object_new_boolean(wp->flags & WORLD_PEER_PROXY);
        json_object_object_add(obj, "proxy", proxy);
        json_object *type = json_object_new_int(wp->type);
        json_object_object_add(obj, "type", type);
        json_object *unreachable = json_object_new_int(wp->unreachable);
        json_object_object_add(obj, "unreachable", unreachable);
        json_object *authed = json_object_new_boolean(wp->flags & WORLD_PEER_AUTHED);
        json_object_object_add(obj, "authed", authed);
        json_object *shadow = json_object_new_boolean(wp->flags & WORLD_PEER_SHADOW);
        json_object_object_add(obj, "shadow", shadow);
        json_object *offline = json_object_new_boolean(wp->flags & WORLD_PEER_OFFLINE);
        json_object_object_add(obj, "offline", offline);
        json_object *pubkeyhash = json_object_new_string_len((const char *)wp->pubkeyhash,
                                                             sizeof(wp->pubkeyhash));
        json_object_object_add(obj, "pubkeyhash", pubkeyhash);
        json_object *version = json_object_new_int(wp->version);
        json_object_object_add(obj, "version", version);
        json_object *tcpdesc = json_object_new_string((const char *)wp->tcp.description);
        json_object_object_add(obj, "tcpdescription", tcpdesc);
        json_object *ports = json_object_new_array();
        json_object_object_add(obj, "ports", ports);
        int fill(struct list_s *l, void *ex, void *ud) {
            json_object *ports = (json_object *)ud;
            json_object *port = json_object_new_int(*(int *)ex);
            json_object_array_add(ports, port);
            return 0;
        }
        ifr(list.map(&wp->tcp.ports, fill, ports));
        json_object_array_add(peers, obj);
        return 0;
    }
    *obj = json_object_new_object();
    json_object *peers = json_object_new_array();
    json_object_object_add(*obj, "peers", peers);
    return list.map(&p->peers, cb, peers);
}

static int peers_list(struct peer_s *p, char **argv, int argc)
{
    if (!p) return -1;
    json_object *obj;
    ifr(cli.peers.list(p, &obj));
    const char *json = json_object_to_json_string_ext(obj, JSON_C_TO_STRING_PRETTY);
    printf("%s\n", json);
    json_object_put(obj);
    return 0;
}

static int tokenize(char *line, char ***argv, int *argc)
{
    char *s, *arg;
    char *end = line + strlen(line);
    bool quotes = false;
    arg = s = line;
    for ( ; s <= end; s++) {
        if ((*s == ' ' && !quotes) || *s == '\0' || *s == '"') {
            if (*s == '"' && !quotes) {
                arg += 1;
                quotes = true;
                continue;
            }
            if (*s == '"' && quotes) {
                quotes = false;
                *s = '\0';
                continue;
            }
            *argv = realloc(*argv, ++(*argc) * sizeof(void *));
            if (!argv) return -1;
            (*argv)[*argc - 1] = arg;
            *s = '\0';
            arg = s + 1;
        }
    }
    return 0;
}

static int cli_traffic(struct peer_s *p, char **argv, int argc)
{
    if (!p) return -1;
    printf(" Download | Upload |\n");
    printf("   %4ldkB | %4ldkB |\n",
           p->traffic.recv.bytes / 1024,
           p->traffic.send.bytes / 1024);
    return 0;
}

static const struct { const char *alias[8];
                      int         nalias;
                      int         argc;
                      int         (*cb)(struct peer_s *p, char **argv, int argc);
                    } cmds[] = {
    { { "p",  "peers", "l", "list" }, 4, 0, peers_list },
    { { "m",  "msg" },                2, 3, send_message },
    { { "fs", "filesend" },           2, 3, send_file },
    { { "w",  "whoami" },             2, 0, whoami },
    { { "tf", "traffic" },            2, 0, cli_traffic },
};

static int init(struct peer_s *p, char *line)
{
    char **argv = NULL;
    int    argc = 0;
    if (tokenize(line, &argv, &argc) != 0) return -1;
    if (argc < 1) return 0;
    int i, j;
    bool found = false;
    for (i = 0; i < COUNTOF(cmds) && found == false; i++) {
        for (j = 0; j < cmds[i].nalias; j++) {
            if (strcmp(cmds[i].alias[j], argv[0]) == 0 &&
                cmds[i].argc == (argc - 1)) {
                if (cmds[i].cb(p, argv, argc) != 0) {
                    free(argv);
                    return -1;
                }
                found = true;
                break;
            }
        }
    }
    if (!found && p->user.cb.cli) {
        if (p->user.cb.cli(p, argv, argc) != 0) {
            free(argv);
            return -1;
        }
    }
    if (argv) free(argv);
    return 0;
}

const struct module_cli_s cli = {
    .init       = init,
    .peers.list = cli_peers_list,
};
