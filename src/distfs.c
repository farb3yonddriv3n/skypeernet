#include <common.h>

static int transaction_locked(struct distfs_s *dfs, bool *locked)
{
    if (!dfs) return -1;
    ifr(pthread_mutex_lock(&dfs->mining.mutex));
    if (dfs->mining.state) *locked = true;
    else                   *locked = false;
    ifr(pthread_mutex_unlock(&dfs->mining.mutex));
    if (*locked) printf("Denied - block mining is ongoing.\n");
    return 0;
}

int dfs_transaction_add(struct distfs_s *dfs, char **argv, int argc)
{
    if (!dfs || !argv || argc < 2) return -1;
    if (strlen(argv[1]) > 128) return -1;
    int size;
    ifr(list.size(&dfs->transactions, &size));
    if (size > 0) {
        printf("Exactly one transaction allowed per block.\n");
        return 0;
    }
    bool locked;
    ifr(transaction_locked(dfs, &locked));
    if (locked) return 0;
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
    return list.add(&dfs->transactions, t, transaction.clean);
}

int dfs_transaction_share(struct distfs_s *dfs, char **argv, int argc)
{
    if (!dfs || !argv) return -1;
    int size;
    ifr(list.size(&dfs->transactions, &size));
    if (size > 0) {
        printf("Exactly one transaction allowed per block.\n");
        return 0;
    }
    bool locked;
    ifr(transaction_locked(dfs, &locked));
    if (locked) return 0;
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
    json_object_put(obj);
    return list.add(&dfs->transactions, t, NULL);
}

int dfs_transaction_list(struct distfs_s *dfs, char **argv, int argc)
{
    if (!dfs) return -1;
    bool locked;
    ifr(transaction_locked(dfs, &locked));
    if (locked) return 0;
    int cb(struct list_s *l, void *td, void *ud) {
        struct transaction_s *t = (struct transaction_s *)td;
        json_object *obj;
        ifr(transaction.dump(t, &obj));
        printf("%s\n", json_object_to_json_string(obj));
        json_object_put(obj);
        return 0;
    }
    return list.map(&dfs->transactions, cb, NULL);
}

int dfs_job_add(struct distfs_s *dfs, char **argv, int argc)
{
    if (!dfs || !argv || argc != 2) return -1;
    if (strlen(argv[1]) != SHA256HEX) return -1;
    unsigned char *h = (unsigned char *)argv[1];
    bool added, found, exists;
    ifr(job.add(&dfs->peer->cfg, &dfs->jobs, dfs->blocks.remote, h,
                strlen((const char *)h), &found,
                &added, &exists));
    if (exists) {
        printf("File %s exists locally, no need to download it\n", h);
        return 0;
    }
    if (!found) {
        printf("File %s not found in the list of remote files\n", h);
        return 0;
    }
    if (added) printf("Job %s added\n", h);
    else       printf("Job %s exists\n", h);
    return 0;
}
