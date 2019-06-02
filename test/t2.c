#include <common.h>
#include <cu.h>

static int t_file_add(const char *filename, struct block_s *b)
{
    struct transaction_param_s param;
    param.type = TFILE_ADD;
    param.action.add.name = (char *)filename;

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

static struct root_s *root_add(const int blocks, char **files,
                               const int nfiles)
{
    struct root_s *r;

    A(root.init(&r), 0);

    unsigned char prev_block[SHA256HEX];
    memcpy(prev_block, DISTFS_BASE_ROOT_HASH, sizeof(prev_block));

    int i;
    for (i = 0; i < blocks; i++) {
        struct block_s *b;
        A(block.init(&b, prev_block), 0);

        int t;
        for (t = 0; t < nfiles; t++) {
            A(t_file_add(files[t], b), 0);
        }

        A(block.transactions.lock(b), 0);

        A(block.mine(b), 0);

        bool valid;
        A(block.validate(b, &valid), 0);
        A(valid, true);

        A(root.blocks.add(r, b), 0);

        memcpy(prev_block, b->hash.pow, sizeof(b->hash.pow));
    }

    return r;
}

void t1_group_mine_block_append_transactions()
{
    struct config_s cfg;
    A(config_init(&cfg), 0);

    struct group_s *g[2];

    char *files[] = { F64BIN, F128BIN, F256BIN };

    // save
    A(group.init(&g[0]), 0);
    int i;
    for (i = 1; i < 3; i++) {
        struct root_s *r = root_add(i, files, COUNTOF(files));
        CU_ASSERT(r != NULL);
        A(group.roots.add(g[0], r), 0);
    }
    A(group.db.save(g[0]), 0);
    // load
    A(group.init(&g[1]), 0);
    A(group.db.load(g[1]), 0);

    bool equal;
    A(group.compare(g[0], g[1], &equal), 0);
    A(equal, true);

    group.clean(g[0]);
    group.clean(g[1]);

    config_free(&cfg);
}
