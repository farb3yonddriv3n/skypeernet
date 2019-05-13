#include <common.h>

static int message(struct peer_s *p, char **argv, int argc)
{
    long int    host    = strtol(argv[1], NULL, 16);
    long int    port    = strtol(argv[2], NULL, 10);
    const char *message = argv[3];
    return payload.send.peer(p, COMMAND_MESSAGE, host, port);
}

static int peers_list(struct peer_s *p, char **argv, int argc)
{
    int cb(struct list_s *l, void *uwp, void *ud) {
        struct world_peer_s *wp = (struct world_peer_s *)uwp;
        printf("Host:Port %x:%d\n", wp->host, wp->port);
        return 0;
    }
    printf("List:\n");
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
            (*argv)[*argc - 1] = arg;
            *s = '\0';
            arg = s + 1;
        }
    }
    return 0;
}

static const struct { const char *alias[8];
                      int         nalias;
                      int         argc;
                      int         (*cb)(struct peer_s *p, char **argv, int argc);
                    } cmds[2] = {
    { { "p", "peers", "l", "list" }, 4, 0, peers_list },
    { { "m", "msg" },                2, 3, message },
};

int cli(struct peer_s *p, char *line)
{
    char **argv = NULL;
    int    argc = 0;
    if (tokenize(line, &argv, &argc) != 0) return -1;

    if (argc < 1) return 0;

    int i, j;
    for (i = 0; i < argc; i++) {
        for (j = 0; j < cmds[i].nalias; j++) {
            if (strcmp(cmds[i].alias[j], argv[0]) == 0 &&
                cmds[i].argc == (argc - 1))
                return cmds[i].cb(p, argv, argc);
        }
    }

    return -1;
    /*
    int msg(struct list_s *l, void *uwp, void *ud) {
        struct world_peer_s *wp = uwp;
        if (payload.send.peer(p, COMMAND_MESSAGE,
                              wp->host, wp->port) != 0) return -1;
        return 0;
    }
    return list.map(&p->peers, msg, NULL);
    */
}
