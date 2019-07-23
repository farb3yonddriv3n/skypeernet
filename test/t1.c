#include <common.h>
#include <cu.h>

static int t_file_add(const char *filename, struct block_s *b)
{
    struct transaction_param_s param;
    param.type = TFILE_ADD;
    param.action.add.name = (char *)filename;
    char pathname[256];
    snprintf(pathname, sizeof(pathname), "%s/%s", psig->cfg.dir.finalized, filename);
    param.action.add.pathname = pathname;
    struct transaction_s *t;
    int ret = transaction.init(&t, &param);
    if (ret != 0) return ret;
    bool valid;
    ret = transaction.validate(t, &valid);
    if (ret != 0) return ret;
    A(valid, true);
    A(block.transactions.add(b, t), 0);
    return 0;
}

static struct root_s *root_add(const char *file)
{
    struct root_s *r;
    A(root.init(&r), 0);
    unsigned char prev_block[SHA256HEX];
    memcpy(prev_block, DISTFS_BASE_ROOT_HASH, sizeof(prev_block));
    struct block_s *b;
    A(block.init(&b, prev_block), 0);
    A(t_file_add(file, b), 0);
    A(block.transactions.lock(b), 0);
    A(block.mine(b), 0);
    bool valid;
    A(block.validate(b, &valid), 0);
    A(valid, true);
    A(root.blocks.add(r, b), 0);
    memcpy(prev_block, b->hash.pow, sizeof(b->hash.pow));
    return r;
}

void t1_group_mine_block_append_transactions()
{
    struct peer_s p;
    A(peer.init.mpeer(&p), 0);
    struct group_s *g[2];
    char *files[] = { FILE1, FILE2 };
    A(group.init(&g[0]), 0);
    int i;
    for (i = 0; i < COUNTOF(files); i++) {
        struct root_s *r = root_add(files[i]);
        CU_ASSERT(r != NULL);
        A(group.roots.add(g[0], r), 0);
    }
    A(group.db.save(g[0]), 0);
    A(group.init(&g[1]), 0);
    A(group.db.load(g[1]), 0);
    bool equal;
    A(group.compare(g[0], g[1], &equal), 0);
    A(equal, true);
    group.clean(g[0]);
    group.clean(g[1]);
    peer.clean(&p);
}
