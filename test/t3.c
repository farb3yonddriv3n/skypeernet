#include <common.h>
#include <cu.h>

void t3_block_append_transactions()
{
    struct config_s *cfg;
    A(config_init(&cfg), 0);

    struct transaction_s *t;
    struct transaction_param_s param;

    param.type = TFILE_ADD;
    sn_setz(param.action.add.name, F128BIN);

    A(transaction.init(&t, &param), 0);

    A(transaction.dump(t), 0);

    bool valid;
    A(transaction.validate(t, &valid), 0);
    A(valid, true);

    unsigned char prev_block[SHA256HEX];
    memset(prev_block, 97, sizeof(prev_block));
    uint64_t nounce;
    unsigned char newblock[SHA256HEX];
    A(block.mine(prev_block, newblock, &nounce), 0);

    struct block_s *b;
    A(block.init(&b, prev_block, newblock, nounce, 0), 0);

    A(block.transaction.add(b, t), 0);
    A(block.transaction.hash(b), 0);

    A(block.validate(b, &valid), 0);
    A(valid, true);

    json_object *bobj;
    A(block.data.save(b, &bobj), 0);
}
