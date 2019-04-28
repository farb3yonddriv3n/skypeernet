#include <common.h>
#include <cu.h>

static int t_file_add(const char *filename, struct block_s *b)
{
    struct transaction_param_s param;
    param.type = TFILE_ADD;
    sn_setz(param.action.add.name, (char *)filename);

    struct transaction_s *t;
    int ret = transaction.init(&t, &param);
    if (ret != 0) return ret;

    bool valid;
    ret = transaction.validate(t, &valid);
    if (ret != 0) return ret;
    A(valid, true);

    A(block.transaction.add(b, t), 0);

    return 0;
}

void t2_mine_block_append_transactions()
{
    struct config_s *cfg;
    A(config_init(&cfg), 0);

    const char *files[] = { F64BIN, F512BIN };
    struct root_s r;

    root.init(&r);

    unsigned char prev_block[SHA256HEX];
    memset(prev_block, 97, sizeof(prev_block));

    int i;
    for (i = 0; i < 3; i++) {
        uint64_t nounce;
        unsigned char newblock[SHA256HEX];
        A(block.mine(prev_block, newblock, &nounce), 0);

        struct block_s *b;
        A(block.init(&b, prev_block, newblock, nounce, i), 0);

        int t;
        for (t = 0; t < COUNTOF(files); t++) {
            A(t_file_add(files[t], b), 0);
        }

        A(block.transaction.hash(b), 0);

        bool valid;
        A(block.validate(b, &valid), 0);
        A(valid, true);

        A(root.blocks.add(&r, b), 0);

        memcpy(prev_block, b->hash.current, sizeof(b->hash.current));
    }

    json_object *dst;
    A(root.data.save(&r, &dst), 0);
    json_object_put(dst);

    A(root.clean(&r), 0);

    config_free(cfg);
}
