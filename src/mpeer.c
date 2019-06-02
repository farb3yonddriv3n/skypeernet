#include <common.h>

#define BLOCK_FILE "block_db"

static int message(struct peer_s *p, int host,
                    unsigned short port,
                    char *msg, int len)
{
    printf("Message %.*s from %x:%d\n", len, msg, host, port);
    return 0;
}

static int file(struct peer_s *p, int host,
                 unsigned short port,
                 const char *received)
{
    printf("Received file: %s\n", received);
    return 0;
}

static int online(struct peer_s *p, struct world_peer_s *wp)
{
    printf("New peer: %x:%d of type %d\n", wp->host, wp->port, wp->type);
    return 0;
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
    if (root.init(&dfs->local_block) != 0)
        return mine_thread_fail(__FILE__, __LINE__);
    if (root.data.load.file(&dfs->local_block, BLOCK_FILE))
        syslog(LOG_DEBUG, "DB file not found %s:%d", __FILE__, __LINE__);
    bool valid;
    if (root.validate(dfs->local_block, &valid) != 0)
        return mine_thread_fail(__FILE__, __LINE__);
    if (!valid)
        return mine_thread_fail(__FILE__, __LINE__);
    size_t size;
    if (root.blocks.size(dfs->local_block, &size) != 0)
        return mine_thread_fail(__FILE__, __LINE__);
    unsigned char *prev_block;
    if (size == 0) prev_block = DISTFS_BASE_ROOT_HASH;
    else           prev_block = dfs->local_block->hash;
    struct block_s *b;
    if (block.init(&b, prev_block) != 0)
        return mine_thread_fail(__FILE__, __LINE__);
    if (list.map(&dfs->transactions, cb, b) != 0)
        return mine_thread_fail(__FILE__, __LINE__);
    if (block.transactions.lock(b))
        return mine_thread_fail(__FILE__, __LINE__);
    if (block.mine(b) != 0)
        return mine_thread_fail(__FILE__, __LINE__);
    if (root.blocks.add(dfs->local_block, b) != 0)
        return mine_thread_fail(__FILE__, __LINE__);
    if (root.data.save.file(dfs->local_block, BLOCK_FILE) != 0)
        return mine_thread_fail(__FILE__, __LINE__);
    if (root.clean(dfs->local_block) != 0)
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

static const struct { const char *alias[8];
                      int         nalias;
                      int         argc;
                      int         (*cb)(struct distfs_s *dfs, char **argv, int argc);
                    } cmds[] = {
    { { "t",  "tadd"  }, 2, 2, dfs_transaction_add  },
    { { "tl", "tlist" }, 2, 0, dfs_transaction_list },
    { { "bm", "bmine" }, 2, 0, dfs_block_mine },
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
    p->user.cb.file    = file;
    p->user.cb.online  = online;
    p->user.cb.cli     = dfs_cli;
    p->user.data       = dfs;
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
