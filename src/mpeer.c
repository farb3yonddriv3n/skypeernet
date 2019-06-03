#include <common.h>

#define BLOCK_FILE "block_db"

enum distfs_cmd_e {
    DISTFS_HELLO,
};

#define MSG_HELLO      "hello"
#define MSG_HELLO_SIZE (sizeof(MSG_HELLO) - 1)

static int hello_write(struct distfs_s *dfs, int host, unsigned short port);

static const struct { enum distfs_cmd_e cmd;
                      int (*write)(struct distfs_s *dfs, int host, unsigned short port);
                      int (*read)(struct distfs_s *dfs);
                    } dfs_cmds[] = {
    { DISTFS_HELLO, hello_write, NULL },
};

static int message(struct peer_s *p, int host,
                   unsigned short port,
                   char *msg, int len)
{
    printf("Message %.*s from %x:%d\n", len, msg, host, port);
    if (dmemcmp(MSG_HELLO, MSG_HELLO_SIZE, msg, len)) {
        bool exists;
        ifr(os.fileexists(BLOCK_FILE, &exists));
        if (exists) return task.init(p, BLOCK_FILE, strlen(BLOCK_FILE), host, port);
    }
    return 0;
}

static int dfile(struct peer_s *p, int host,
                 unsigned short port,
                 const char *received)
{
    struct root_s *r;
    struct distfs_s *dfs = (struct distfs_s *)p->user.data;
    if (root.data.load.file(&r, received) == 0) {
        ifr(root.net.set(r, host, port));
        ifr(group.roots.add(dfs->blocks.remote, r));
    } else {
    }
    return 0;
}

static int distfs_cmd_find(enum distfs_cmd_e cmd, int *idx)
{
    int i;
    for (i = 0; i < COUNTOF(dfs_cmds); i++) {
        if (dfs_cmds[i].cmd == cmd) {
            *idx = i;
            return 0;
        }
    }
    return -1;
}

static int hello_write(struct distfs_s *dfs, int host, unsigned short port)
{
    if (!dfs) return -1;
    struct peer_s *p = (struct peer_s *)dfs->peer;
    p->send_buffer.type = BUFFER_MESSAGE;
    p->send_buffer.u.message.str = MSG_HELLO;
    return payload.send(p, COMMAND_MESSAGE,
                        host, port, 0, 0);
    return 0;
}

static int distfs_command_send(struct distfs_s *dfs, enum distfs_cmd_e cmd,
                               int host, unsigned short port)
{
    int idx;
    if (distfs_cmd_find(cmd, &idx) != 0) return -1;
    return dfs_cmds[idx].write(dfs, host, port);
}

static int online(struct peer_s *p, struct world_peer_s *wp)
{
    printf("New peer: %x:%d of type %d\n", wp->host, wp->port, wp->type);
    if (wp->type == WORLD_PEER_TRACKER) return 0;
    return distfs_command_send(p->user.data, DISTFS_HELLO, wp->host, wp->port);
}

static bool mining = false;

static void *mine_thread_fail(const char *file, const int line)
{
    syslog(LOG_ERR, "Error at %s:%d", file, line);
    mining = false;
    return NULL;
}

static void *mine(void *data)
{
    if (!data)
        return mine_thread_fail(__FILE__, __LINE__);
    struct distfs_s *dfs = data;
    // TODO: copy transactions and clean
    int cb(struct list_s *l, void *ut, void *ud) {
        struct transaction_s *t = (struct transaction_s *)ut;
        struct block_s       *b = (struct block_s *)ud;
        if (!l || !ut || !ud) return -1;
        ifr(block.transactions.add(b, t));
        return 0;
    }
    if (root.init(&dfs->blocks.local) != 0)
        return mine_thread_fail(__FILE__, __LINE__);
    if (root.data.load.file(&dfs->blocks.local, BLOCK_FILE))
        syslog(LOG_DEBUG, "DB file not found %s:%d", __FILE__, __LINE__);
    bool valid;
    if (root.validate(dfs->blocks.local, &valid) != 0)
        return mine_thread_fail(__FILE__, __LINE__);
    if (!valid)
        return mine_thread_fail(__FILE__, __LINE__);
    size_t size;
    if (root.blocks.size(dfs->blocks.local, &size) != 0)
        return mine_thread_fail(__FILE__, __LINE__);
    unsigned char *prev_block;
    if (size == 0) prev_block = DISTFS_BASE_ROOT_HASH;
    else           prev_block = dfs->blocks.local->hash;
    struct block_s *b;
    if (block.init(&b, prev_block) != 0)
        return mine_thread_fail(__FILE__, __LINE__);
    if (list.map(&dfs->transactions, cb, b) != 0)
        return mine_thread_fail(__FILE__, __LINE__);
    if (block.transactions.lock(b))
        return mine_thread_fail(__FILE__, __LINE__);
    if (block.mine(b) != 0)
        return mine_thread_fail(__FILE__, __LINE__);
    if (root.blocks.add(dfs->blocks.local, b) != 0)
        return mine_thread_fail(__FILE__, __LINE__);
    if (root.data.save.file(dfs->blocks.local, BLOCK_FILE) != 0)
        return mine_thread_fail(__FILE__, __LINE__);
    if (root.clean(dfs->blocks.local) != 0)
        return mine_thread_fail(__FILE__, __LINE__);
    if (list.clean(&dfs->transactions) != 0)
        return mine_thread_fail(__FILE__, __LINE__);
    mining = false;
    return NULL;
}

static int dfs_block_mine(struct distfs_s *dfs, char **argv, int argc)
{
    int size;
    ifr(list.size(&dfs->transactions, &size));
    if (size < 1 || mining) return 0;
    pthread_t m;
    mining = true;
    if (pthread_create(&m, NULL, mine, dfs) != 0) return -1;
    return 0;
}

static int dfs_list_files(struct distfs_s *dfs, char **argv, int argc)
{
    if (!dfs) return -1;
    ifr(group.dump(dfs->blocks.remote));
    return 0;
}

static const struct { const char *alias[8];
                      int         nalias;
                      int         argc;
                      int         (*cb)(struct distfs_s *dfs, char **argv, int argc);
                    } cmds[] = {
    { { "t",  "tadd"  }, 2, 2, dfs_transaction_add  },
    { { "tl", "tlist" }, 2, 0, dfs_transaction_list },
    { { "bm", "bmine" }, 2, 0, dfs_block_mine },
    { { "lf", "listfiles" }, 2, 0, dfs_list_files },
};

static int find_cmd(char *argv, int argc, int *idx)
{
    if (!argv || !idx) return -1;
    int i, j;
    for (i = 0; i < COUNTOF(cmds); i++) {
        for (j = 0; j < cmds[i].nalias; j++) {
            if (strcmp(cmds[i].alias[j], argv) == 0 &&
                argc - 1 == cmds[i].argc) {
                *idx = i;
                return 0;
            }
        }
    }
    return -1;
}

static int dfs_cli(struct peer_s *p, char **argv, int argc)
{
    if (!p || !argv) return -1;
    int idx;
    ifr(find_cmd(argv[0], argc, &idx));
    struct distfs_s *dfs = (struct distfs_s *)p->user.data;
    return cmds[idx].cb(dfs, argv, argc);
}

static int init(struct peer_s *p, struct distfs_s *dfs)
{
    if (!p || !dfs) return -1;
    memset(dfs, 0, sizeof(*dfs));
    p->user.cb.message = message;
    p->user.cb.file    = dfile;
    p->user.cb.online  = online;
    p->user.cb.cli     = dfs_cli;
    p->user.data       = dfs;
    dfs->peer          = p;
    ifr(group.init(&dfs->blocks.remote));
    return 0;
}

int main()
{
    openlog("distfs:peer", LOG_PID|LOG_CONS, LOG_USER);
    struct distfs_s dfs;
    struct peer_s p;
    if (peer.init.mpeer(&p) != 0) return -1;
    if (init(&p, &dfs) != 0) return -1;
    if (payload.send(&p, COMMAND_PEER_ANNOUNCE_PEER,
                     p.tracker.host,
                     p.tracker.port, 0, 0) != 0) return -1;
    ev_io_start(p.ev.loop, &p.ev.stdinwatch);
    ifr(net.resume(&p.ev));
    ev_timer_again(p.ev.loop, &p.ev.peers_reachable);
    ev_loop(p.ev.loop, 0);
    ev_loop_destroy(p.ev.loop);
    close(p.net.sd);
    closelog();
    rl_callback_handler_remove();
    return 0;
}
