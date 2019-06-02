#include <common.h>

static int transaction_clean(void *t)
{
    return transaction.clean((struct transaction_s *)t);
}

int dfs_transaction_add(struct distfs_s *dfs, char **argv, int argc)
{
    struct transaction_s *t;
    struct transaction_param_s param;
    param.type = TFILE_ADD;
    param.action.add.name = argv[2];
    ifr(transaction.init(&t, &param));
    return list.add(&dfs->transactions, t, NULL);
}

int dfs_transaction_list(struct distfs_s *dfs, char **argv, int argc)
{
    int cb(struct list_s *l, void *td, void *ud) {
        struct transaction_s *t = (struct transaction_s *)td;
        return transaction.dump(t);
    }
    return list.map(&dfs->transactions, cb, NULL);
}
