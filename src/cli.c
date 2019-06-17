#include <common.h>

static int send_message(struct peer_s *p, char **argv, int argc)
{
    long int    host    = strtol(argv[1], NULL, 16);
    long int    port    = strtol(argv[2], NULL, 10);
    const char *message = argv[3];
    p->send_buffer.type = BUFFER_MESSAGE;
    p->send_buffer.u.message.str = message;
    return payload.send(p, COMMAND_MESSAGE,
                        host, port, 0, 0, NULL);
}

static int whoami(struct peer_s *p, char **argv, int argc)
{
    printf("%x:%d %.*s\n", ADDR_IP(p->net.self.addr), ADDR_PORT(p->net.self.addr),
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
                    host, port, TASK_FILE_KEEP);
}

static int peers_list(struct peer_s *p, char **argv, int argc)
{
    int cb(struct list_s *l, void *uwp, void *ud) {
        struct world_peer_s *wp = (struct world_peer_s *)uwp;
        printf(" %8x | %5d | %4d | %11d | %.*s |\n",
               wp->host,
               wp->port,
               wp->type,
               wp->unreachable,
               (int )sizeof(wp->pubkeyhash), wp->pubkeyhash);
        return 0;
    }
    printf("     Peer |  Port | Type | Unreachable |                                            Pubkeyhash |\n");
    return list.map(&p->peers, cb, NULL);
}

static int tokenize(char *line, char ***argv, int *argc)
{
    char *s, *arg;
    char *end = line + strlen(line);
    arg = s = line;
    for ( ; s <= end; s++) {
        if (*s == ' ' || *s == '\0') {
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

int cli(struct peer_s *p, char *line)
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
        ifr(p->user.cb.cli(p, argv, argc));
    }
    if (argv) free(argv);
    return 0;
}
