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

int dfs_transaction_add(struct distfs_s *dfs, char **argv, int argc,
                        int *dfserr)
{
    if (!dfs || !argv || argc != 3 || !dfserr) return -1;
    if (strlen(argv[1]) > 128) return -1;
    int size;
    ifr(list.size(&dfs->transactions, &size));
    if (size > 0) {
        printf("Exactly one transaction allowed per block.\n");
        *dfserr = 1;
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
    param.action.add.tags = argv[2];
    char pathname[256];
    snprintf(pathname, sizeof(pathname), "%s/%s", p->cfg.dir.finalized,
                                                  argv[1]);
    param.action.add.pathname = pathname;
    ifr(transaction.init(&t, &param));
    return list.add(&dfs->transactions, t, transaction.clean);
}

int dfs_transaction_share(struct distfs_s *dfs, char **argv, int argc,
                          int *dfserr)
{
    if (!dfs || !argv || !dfserr || argc != 2) return -1;
    if (strlen(argv[1]) != SHA256HEX) return -1;
    int size;
    ifr(list.size(&dfs->transactions, &size));
    if (size > 0) {
        printf("Exactly one transaction allowed per block.\n");
        *dfserr = 1;
        return 0;
    }
    bool locked;
    ifr(transaction_locked(dfs, &locked));
    if (locked) {
        *dfserr = 2;
        return 0;
    }
    struct file_s *f = NULL;
    ifr(group.find.transaction(dfs->blocks.remote, (unsigned char *)argv[1],
                               (void **)&f, NULL, NULL));
    if (!f) {
        printf("Transaction's file hash %s not found\n", argv[1]);
        *dfserr = 3;
        return 0;
    }
    json_object *obj;
    ifr(transaction.data.save(f->parent, &obj));
    struct transaction_s *t;
    ifr(transaction.data.load(&t, obj));
    json_object_put(obj);
    return list.add(&dfs->transactions, t, NULL);
}

int dfs_transaction_list(struct distfs_s *dfs, char **argv, int argc,
                         int *dfserr)
{
    if (!dfs || !dfserr) return -1;
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

int dfs_job_add(struct distfs_s *dfs, char **argv, int argc,
                int *dfserr)
{
    if (!dfs || !argv || argc != 2 || !dfserr) return -1;
    if (strlen(argv[1]) != SHA256HEX) return -1;
    unsigned char *h = (unsigned char *)argv[1];
    bool added, found, exists;
    ifr(job.add(&dfs->peer->cfg, &dfs->jobs, dfs->blocks.remote, h,
                strlen((const char *)h), &found,
                &added, &exists));
    if (exists) {
        printf("File %s exists locally, no need to download it\n", h);
        *dfserr = 1;
        return 0;
    }
    if (!found) {
        printf("File %s not found in the list of remote files\n", h);
        *dfserr = 2;
        return 0;
    }
    if (added)
        printf("Job %s added\n", h);
    else {
        printf("Job %s exists\n", h);
        *dfserr = 3;
    }
    return 0;
}

int dfs_job_finalize(struct distfs_s *dfs, char **argv, int argc,
                     int *dfserr)
{
    if (!dfs || !argv || argc != 2 || !dfserr) return -1;
    if (strlen(argv[1]) != SHA256HEX) return -1;
    unsigned char *h = (unsigned char *)argv[1];
    bool finalized;
    if (job.finalize(dfs->peer, dfs->blocks.remote, h,
                     strlen((const char *)h), &finalized) != 0) {
        *dfserr = 2;
        return 0;
    };
    if (finalized) *dfserr = 0;
    else           *dfserr = 1;
    return 0;
}

static void *mine_thread_fail(struct distfs_s *dfs, const char *file,
                              const int line)
{
    syslog(LOG_ERR, "Error at %s:%d", file, line);
    pthread_mutex_lock(&dfs->mining.mutex);
    dfs->mining.state = false;
    pthread_mutex_unlock(&dfs->mining.mutex);
    return NULL;
}

static int mine_block_file(struct distfs_s *dfs)
{
    if (!dfs) return -1;
    struct peer_s *p = dfs->peer;
    char blockfile[256];
    if (strnlen((char *)dfs->blocks.file, sizeof(dfs->blocks.file)) > 0) {
        snprintf(blockfile, sizeof(blockfile), "%s/%.*s", p->cfg.dir.block,
                                                          (int )sizeof(dfs->blocks.file),
                                                          dfs->blocks.file);
        if (remove(blockfile) != 0) return -1;
    }
    memcpy(dfs->blocks.file, dfs->blocks.local->hash, sizeof(dfs->blocks.local->hash));
    snprintf(blockfile, sizeof(blockfile), "%s/%.*s", p->cfg.dir.block,
                                                      (int )sizeof(dfs->blocks.file),
                                                      dfs->blocks.file);
    return root.data.save.file(dfs->blocks.local, blockfile);
}

static void *mine(void *data)
{
    struct distfs_s *dfs = (struct distfs_s *)data;
    if (!dfs)
        return mine_thread_fail(dfs, __FILE__, __LINE__);
    int cb(struct list_s *l, void *ut, void *ud) {
        struct transaction_s *t = (struct transaction_s *)ut;
        struct block_s       *b = (struct block_s *)ud;
        if (!l || !ut || !ud) return -1;
        json_object *obj;
        ifr(transaction.data.save(t, &obj));
        struct transaction_s *tl;
        ifr(transaction.data.load(&tl, obj));
        ifr(block.transactions.add(b, tl));
        json_object_put(obj);
        return 0;
    }
    size_t size;
    if (root.blocks.size(dfs->blocks.local, &size) != 0)
        return mine_thread_fail(dfs, __FILE__, __LINE__);
    unsigned char *prev_block;
    if (size == 0) prev_block = (unsigned char *)DISTFS_BASE_ROOT_HASH;
    else           prev_block = dfs->blocks.local->hash;
    struct block_s *b;
    if (block.init(&b, prev_block) != 0)
        return mine_thread_fail(dfs, __FILE__, __LINE__);
    if (list.map(&dfs->transactions, cb, b) != 0)
        return mine_thread_fail(dfs, __FILE__, __LINE__);
    if (list.clean(&dfs->transactions) != 0)
        return mine_thread_fail(dfs, __FILE__, __LINE__);
    if (block.transactions.lock(b) != 0)
        return mine_thread_fail(dfs, __FILE__, __LINE__);
    double start;
    if (os.gettimems(&start) != 0)
        return mine_thread_fail(dfs, __FILE__, __LINE__);
    if (block.mine(b, dfs->peer->miningtarget.ptr,
                   dfs->peer->miningtarget.size) != 0)
        return mine_thread_fail(dfs, __FILE__, __LINE__);
    double end;
    if (os.gettimems(&end) != 0)
        return mine_thread_fail(dfs, __FILE__, __LINE__);
    if (root.blocks.add(dfs->blocks.local, b) != 0)
        return mine_thread_fail(dfs, __FILE__, __LINE__);
    if (mine_block_file(dfs) != 0)
        return mine_thread_fail(dfs, __FILE__, __LINE__);
    pthread_mutex_lock(&dfs->mining.mutex);
    dfs->mining.state    = false;
    dfs->mining.newblock = true;
    pthread_mutex_unlock(&dfs->mining.mutex);
    printf("Mining finished. It took %f seconds\n", end - start);
    return NULL;
}

int dfs_block_mine(struct distfs_s *dfs, char **argv, int argc,
                   int *dfserr)
{
    if (!dfs || !dfserr) return -1;
    ifr(pthread_mutex_lock(&dfs->mining.mutex));
    if (dfs->mining.state) {
        *dfserr = 1;
        ifr(pthread_mutex_unlock(&dfs->mining.mutex));
        return 0;
    }
    int size;
    ifr(list.size(&dfs->transactions, &size));
    if (size < 1) {
        *dfserr = 2;
        ifr(pthread_mutex_unlock(&dfs->mining.mutex));
        return 0;
    }
    pthread_t m;
    dfs->mining.state = true;
    ifr(pthread_mutex_unlock(&dfs->mining.mutex));
    if (pthread_create(&m, NULL, mine, dfs) != 0) return -1;
    return 0;
}

int dfs_block_send(struct peer_s *p, struct distfs_s *dfs,
                   int host, unsigned short port)
{
    if (!p || !dfs) return -1;
    char blockfile[256];
    if (strnlen((char *)dfs->blocks.file, sizeof(dfs->blocks.file)) < 1) return 0;
    snprintf(blockfile, sizeof(blockfile), "%s/%.*s", p->cfg.dir.block,
            (int )sizeof(dfs->blocks.file),
            dfs->blocks.file);
    bool exists;
    ifr(os.fileexists(blockfile, &exists));
    if (exists) {
        ifr(task.add(p, p->cfg.dir.block, dfs->blocks.file,
                     sizeof(dfs->blocks.file),
                     host, port, NULL, TASK_FILE_KEEP, 0));
    }
    return 0;
}

int dfs_hello(struct distfs_s *dfs, int host, unsigned short port)
{
    if (!dfs) return -1;
    struct peer_s *p = (struct peer_s *)dfs->peer;
    p->send_buffer.type = BUFFER_MESSAGE;
    p->send_buffer.u.message.str = MSG_HELLO;
    return payload.send(p, COMMAND_MESSAGE,
                        host, port, 0, 0,
                        NULL, NULL);
}

int dfs_block_xet(struct distfs_s *dfs, char **argv, int argc,
                         int *dfserr)
{
    if (!dfs || !argv || !dfserr) return -1;
    enum block_action_e { BLOCK_ACTION_NONE, BLOCK_ACTION_UPDATE,
                          BLOCK_ACTION_ADVERTISE };
    struct block_action_s { struct distfs_s *dfs; int counter; int action; };
    int cb(struct list_s *l, void *up, void *ud) {
        struct world_peer_s   *wp = (struct world_peer_s *)up;
        struct block_action_s *ba = (struct block_action_s *)ud;
        if (wp->unreachable != 0 || wp->type != WORLD_PEER_PEER) return 0;
        if (ba->action == BLOCK_ACTION_ADVERTISE) {
            ifr(dfs_block_send(ba->dfs->peer, ba->dfs,
                               wp->host, wp->port));
        } else if (ba->action == BLOCK_ACTION_UPDATE) {
            ifr(dfs_hello(ba->dfs,
                            wp->host, wp->port));
        } else return -1;
        ba->counter++;
        return 0;
    }
    struct block_action_s ba = { .dfs = dfs, .counter = 0 };
    if (strcmp(argv[1], "u")      == 0) ba.action = BLOCK_ACTION_UPDATE;
    else if (strcmp(argv[1], "a") == 0) ba.action = BLOCK_ACTION_ADVERTISE;
    else                                ba.action = BLOCK_ACTION_NONE;
    if (ba.action == BLOCK_ACTION_ADVERTISE) {
        pthread_mutex_lock(&dfs->mining.mutex);
        if (!dfs->mining.newblock) {
            *dfserr = 1;
            pthread_mutex_unlock(&dfs->mining.mutex);
            return 0;
        }
        dfs->mining.newblock = false;
        pthread_mutex_unlock(&dfs->mining.mutex);
    }
    ifr(list.map(&dfs->peer->peers, cb, &ba));
    printf("Block action %d to %d peers\n", ba.action, ba.counter);
    return 0;
}

int dfs_block_mining(struct distfs_s *dfs, bool *locked)
{
    if (!dfs || !locked) return -1;
    ifr(pthread_mutex_lock(&dfs->mining.mutex));
    if (dfs->mining.state) *locked = true;
    else                   *locked = false;
    ifr(pthread_mutex_unlock(&dfs->mining.mutex));
    return 0;
}
