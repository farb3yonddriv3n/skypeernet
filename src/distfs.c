#include <common.h>

static int transaction_clean(void *t)
{
    return transaction.clean((struct transaction_s *)t);
}

int dfs_transaction_add(struct distfs_s *dfs, char **argv, int argc)
{
    if (!dfs || !argv || argc < 2) return -1;
    if (strlen(argv[1]) > 128) return -1;
    struct peer_s *p = dfs->peer;
    struct transaction_s *t;
    struct transaction_param_s param;
    param.type = TFILE_ADD;
    param.action.add.name = argv[1];
    char pathname[256];
    snprintf(pathname, sizeof(pathname), "%s/%s", p->cfg.dir.finalized,
                                                  argv[1]);
    param.action.add.pathname = pathname;
    ifr(transaction.init(&t, &param));
    return list.add(&dfs->transactions, t, NULL);
}

int dfs_transaction_share(struct distfs_s *dfs, char **argv, int argc)
{
    if (!dfs || !argv) return -1;
    struct file_s *f = NULL;
    if (strlen(argv[1]) != SHA256HEX) return -1;
    ifr(group.find.transaction(dfs->blocks.remote, (unsigned char *)argv[1],
                               (void **)&f, NULL, NULL));
    if (!f) {
        printf("Transaction's file hash %s not found\n", argv[1]);
        return 0;
    }
    json_object *obj;
    ifr(transaction.data.save(f->parent, &obj));
    struct transaction_s *t;
    ifr(transaction.data.load(&t, obj));
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
