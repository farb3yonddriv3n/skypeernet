#include <common.h>

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

static int block_send(struct peer_s *p, struct distfs_s *dfs,
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
                     host, port, TASK_FILE_KEEP));
    }
    return 0;
}

static int message(struct peer_s *p, int host,
                   unsigned short port,
                   char *msg, int len)
{
    if (!p || !msg) return -1;
    struct distfs_s *dfs = (struct distfs_s *)p->user.data;
    printf("Message %.*s from %x:%d\n", len, msg, host, port);
    rl_redraw_prompt_last_line();
    ifr(p->api.cb.message(p, host, port, msg, len));
    if (!(dmemcmp(MSG_HELLO, MSG_HELLO_SIZE, msg, len) &&
        strnlen((char *)dfs->blocks.file, sizeof(dfs->blocks.file)) > 0))
        return 0;
    syslog(LOG_INFO, "Peer %x:%d asked for blockfile", host, port);
    ifr(block_send(p, dfs, host, port));
    return 0;
}

static int dfs_auth(struct peer_s *p, int host,
                    unsigned short port,
                    char *data, int len)
{
    if (!p || !data) return -1;
    unsigned char *dec;
    int            ndec;
    ifr(rsa_decrypt(p->cfg.keys.local.rsa.private, (unsigned char *)data, len,
                    &dec, &ndec));
    p->send_buffer.type = BUFFER_AUTH_REPLY;
    sn_setr(p->send_buffer.u.auth.str, (char *)dec, ndec);
    ifr(payload.send(p, COMMAND_AUTH_REPLY,
                     host, port, 0, 0,
                     NULL));
    free(dec);
    return 0;
}

static int dfileask(struct peer_s *p, int host,
                    unsigned short port,
                    char *data, int len)
{
    if (!p || !data) return -1;
    unsigned char filename[SHA256HEX];
    unsigned char chunk[SHA256HEX];
    sn_initr(buffer, data, len);
    if (sn_read((void *)filename, sizeof(filename), &buffer) != 0) return -1;
    if (sn_read((void *)chunk,    sizeof(chunk),    &buffer) != 0) return -1;
    struct distfs_s *dfs = (struct distfs_s *)p->user.data;
    struct file_s *f = NULL;
    if (!dfs->blocks.local) return 0;
    ifr(root.find(dfs->blocks.local, filename, (void **)&f));
    if (!f) return 0;
    int i;
    for (i = 0; i < f->chunks.size; i++) {
        if (dmemcmp(f->chunks.array[i].hash.content,
                    sizeof(f->chunks.array[i].hash.content),
                    chunk, sizeof(chunk))) {
            char chunkfile[256];
            snprintf(chunkfile, sizeof(chunkfile), "%s/%.*s", p->cfg.dir.download,
                                                              (int )sizeof(chunk), chunk);
            bool exists;
            ifr(os.fileexists(chunkfile, &exists));
            if (!exists) return -1;
            return task.add(p, p->cfg.dir.download, chunk, sizeof(chunk),
                            host, port, TASK_FILE_KEEP);
        }
    }
    return 0;
}

static int dfile(struct peer_s *p, int host,
                 unsigned short port,
                 unsigned char *pubkeyhash,
                 char *fullpath, int nfullpath,
                 char *filename, int nfilename)
{
    struct root_s *src;
    struct distfs_s *dfs = (struct distfs_s *)p->user.data;
    size_t size;
    ifr(os.filesize(fullpath, &size));
    bool humanreadable;
    ifr(os.filereadable(fullpath, &humanreadable));
    if (humanreadable && root.data.load.file(&src, fullpath) == 0) {
        char fpubkeyhash[128];
        snprintf(fpubkeyhash, sizeof(fpubkeyhash), "%.*s", SHA256HEX, pubkeyhash);
        char blockname[256];
        ifr(os.blockname(&p->cfg, blockname, sizeof(blockname), fullpath, pubkeyhash));
        struct root_s *dst = NULL;
        ifr(group.find.root(dfs->blocks.remote, pubkeyhash, &dst));
        if (dst) {
            bool merged;
            ifr(root.merge(dst, src, &merged));
            ifr(root.clean(src));
            if (merged) {
                ifr(root.data.save.file(dst, blockname));
                syslog(LOG_INFO, "Remote block %s from %x:%d successfully merged",
                                 blockname, host, port);
            } else
                syslog(LOG_INFO, "Remote block %s from %x:%d cannot be merged",
                                 blockname, host, port);
        } else {
            ifr(group.roots.add(dfs->blocks.remote, src));
            ifr(os.filemove(fullpath, blockname));
            ifr(root.net.set(src, pubkeyhash));
            syslog(LOG_INFO, "Remote block %s from %x:%d imported",
                             blockname, host, port);
        }
    } else {
        ifr(job.update(p->cfg.dir.download, &dfs->jobs,
                       filename));
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
                        host, port, 0, 0,
                        NULL);
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
    if (p->api.cb.online && p->api.cb.online(p, wp) != 0) return -1;
    if (wp->type == WORLD_PEER_TRACKER) return 0;
    return distfs_command_send(p->user.data, DISTFS_HELLO, wp->host, wp->port);
}

static int offline(struct peer_s *p, struct world_peer_s *wp)
{
    if (p->api.cb.offline && p->api.cb.offline(p, wp) != 0) return -1;
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
    if (block.mine(b) != 0)
        return mine_thread_fail(dfs, __FILE__, __LINE__);
    double end;
    if (os.gettimems(&end) != 0)
        return mine_thread_fail(dfs, __FILE__, __LINE__);
    if (root.blocks.add(dfs->blocks.local, b) != 0)
        return mine_thread_fail(dfs, __FILE__, __LINE__);
    if (mine_block_file(dfs) != 0)
        return mine_thread_fail(dfs, __FILE__, __LINE__);
    pthread_mutex_lock(&dfs->mining.mutex);
    dfs->mining.state = false;
    pthread_mutex_unlock(&dfs->mining.mutex);
    printf("Mining finished. It took %f seconds\n", end - start);
    return NULL;
}

static int dfs_block_mine(struct distfs_s *dfs, char **argv, int argc)
{
    int size;
    ifr(pthread_mutex_lock(&dfs->mining.mutex));
    ifr(list.size(&dfs->transactions, &size));
    if (size < 1 || dfs->mining.state) {
        ifr(pthread_mutex_unlock(&dfs->mining.mutex));
        return 0;
    }
    pthread_t m;
    dfs->mining.state = true;
    ifr(pthread_mutex_unlock(&dfs->mining.mutex));
    if (pthread_create(&m, NULL, mine, dfs) != 0) return -1;
    return 0;
}

static int dfs_list_files(struct distfs_s *dfs, char **argv, int argc)
{
    if (!dfs || !argv) return -1;
    json_object *obj;
    if (strcmp(argv[1], "remote") == 0 ||
        strcmp(argv[1], "r") == 0) {
        ifr(group.dump(dfs->blocks.remote, &dfs->peer->cfg, &obj));
    } else if (strcmp(argv[1], "local") == 0 ||
               strcmp(argv[1], "l") == 0) {
        ifr(root.dump(dfs->blocks.local, &dfs->peer->cfg, &obj));
    } else return -1;
    printf("%s\n", json_object_to_json_string_ext(obj, JSON_C_TO_STRING_PRETTY));
    json_object_put(obj);
    return 0;
}

static int dfs_job_add(struct distfs_s *dfs, char **argv, int argc)
{
    if (!dfs || !argv) return -1;
    unsigned char *h = (unsigned char *)argv[1];
    bool added, found, exists;
    ifr(job.add(&dfs->peer->cfg, &dfs->jobs, dfs->blocks.remote, h,
                strlen((const char *)h), &found,
                &added, &exists));
    if (exists) {
        printf("File %s exists, no need to download it\n", h);
        return 0;
    }
    if (!found) {
        printf("File %s not found\n", h);
        return 0;
    }
    if (added) printf("Job %s added\n", h);
    else       printf("Job %s exists\n", h);
    return 0;
}

static int dfs_job_finalize(struct distfs_s *dfs, char **argv, int argc)
{
    if (!dfs || !argv) return -1;
    unsigned char *h = (unsigned char *)argv[1];
    bool finalized;
    ifr(job.finalize(&dfs->peer->cfg, dfs->blocks.remote, h,
                     strlen((const char *)h), &finalized));
    if (finalized) printf ("Job %s finalized\n", h);
    else           printf ("Unable to finalize job %s\n", h);
    return  0;
}

static int dfs_job_remove(struct distfs_s *dfs, char **argv, int argc)
{
    if (!dfs || !argv) return -1;
    unsigned char *h = (unsigned char *)argv[1];
    bool removed;
    ifr(job.remove(&dfs->jobs, h, strlen((const char *)h), &removed));
    if (removed) printf ("Job %s removed\n", h);
    else         printf ("Unable to remove job %s\n", h);
    return  0;
}


static int dfs_job_show(struct distfs_s *dfs, char **argv, int argc)
{
    if (!dfs) return -1;
    json_object *obj;
    ifr(job.dump(&dfs->peer->cfg, &dfs->jobs, &obj));
    printf("%s\n", json_object_to_json_string_ext(obj, JSON_C_TO_STRING_PRETTY));
    json_object_put(obj);
    return 0;
}

static int dfs_keysdump(struct distfs_s *dfs, char **argv, int argc)
{
    if (!dfs) return -1;
    return config_keysdump(&dfs->peer->cfg);
}

static int dfs_block_xet(struct distfs_s *dfs, char **argv, int argc)
{
    if (!dfs || !argv) return -1;
    enum block_action_e { BLOCK_ACTION_NONE, BLOCK_ACTION_UPDATE,
                          BLOCK_ACTION_ADVERTISE };
    struct block_action_s { struct distfs_s *dfs; int counter; int action; };
    int cb(struct list_s *l, void *up, void *ud) {
        struct world_peer_s   *wp = (struct world_peer_s *)up;
        struct block_action_s *ba = (struct block_action_s *)ud;
        if (wp->unreachable != 0 || wp->type != WORLD_PEER_PEER) return 0;
        if (ba->action == BLOCK_ACTION_ADVERTISE) {
            ifr(block_send(ba->dfs->peer, ba->dfs,
                           wp->host, wp->port));
        } else if (ba->action == BLOCK_ACTION_UPDATE) {
            ifr(hello_write(ba->dfs,
                            wp->host, wp->port));
        } else return -1;
        ba->counter++;
        return 0;
    }
    struct block_action_s ba = { .dfs = dfs, .counter = 0 };
    if (strcmp(argv[1], "u")      == 0) ba.action = BLOCK_ACTION_UPDATE;
    else if (strcmp(argv[1], "a") == 0) ba.action = BLOCK_ACTION_ADVERTISE;
    else                                ba.action = BLOCK_ACTION_NONE;
    ifr(list.map(&dfs->peer->peers, cb, &ba));
    printf("Block action %d to %d peers\n", ba.action, ba.counter);
    return 0;
}

static const struct { const char *alias[8];
                      int         nalias;
                      int         argc;
                      int         (*cb)(struct distfs_s *dfs, char **argv, int argc);
                    } cmds[] = {
    { { "ta", "tadd"  }, 2, 1, dfs_transaction_add  },
    { { "ts", "tshare"  }, 2, 1, dfs_transaction_share  },
    { { "tl", "tlist" }, 2, 0, dfs_transaction_list },
    { { "bm", "bmine" }, 2, 0, dfs_block_mine },
    { { "ba", "blocksaction" }, 2, 1, dfs_block_xet },
    { { "lf", "listfiles" }, 2, 1, dfs_list_files },
    { { "ja", "jobadd" }, 2, 1, dfs_job_add },
    { { "js", "jobshow" }, 2, 0, dfs_job_show },
    { { "jf", "jobfinalize" }, 2, 1, dfs_job_finalize },
    { { "jr", "jobremove" }, 2, 1, dfs_job_remove },
    { { "kd", "keysdump" }, 2, 0, dfs_keysdump },
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

static int clean(struct peer_s *p, void *data)
{
    struct distfs_s *dfs = (struct distfs_s *)data;
    ev_timer_stop(p->ev.loop, &dfs->ev.jobs);
    ifr(group.clean(dfs->blocks.remote));
    ifr(root.clean(dfs->blocks.local));
    ifr(list.clean(&dfs->transactions));
    ifr(job.data.save(dfs));
    ifr(list.clean(&dfs->jobs));
    ev_io_stop(p->ev.loop, &p->api.ev.read);
    return 0;
}

static int blocks_load(struct distfs_s *dfs)
{
    if (!dfs) return -1;
    int cb(void *udfs, const char *filename, const char *fullpath) {
        if (!udfs || !fullpath) return -1;
        struct distfs_s *dfs = (struct distfs_s *)udfs;
        struct root_s *r;
        unsigned char filehash[SHA256HEX];
        if (strlen(filename) != SHA256HEX) return -1;
        memcpy(filehash, filename, SHA256HEX);
        ifr(root.data.load.file(&r, fullpath));
        ifr(root.net.set(r, filehash));
        ifr(group.roots.add(dfs->blocks.remote, r));
        return 0;
    }
    ifr(os.blocksremote(&dfs->peer->cfg, (void *)dfs, cb));
    return 0;
}

static int init(struct peer_s *p, struct distfs_s *dfs)
{
    if (!p || !dfs) return -1;
    memset(dfs, 0, sizeof(*dfs));
    p->user.cb.clean   = clean;
    p->user.cb.message = message;
    p->user.cb.file    = dfile;
    p->user.cb.fileask = dfileask;
    p->user.cb.online  = online;
    p->user.cb.offline = offline;
    p->user.cb.cli     = dfs_cli;
    p->user.cb.auth    = dfs_auth;
    p->user.data       = dfs;
    p->api.cb.message  = api_message_write;
    p->api.cb.online   = api_peer_online;
    p->api.cb.offline  = api_peer_offline;
    dfs->peer          = p;
    ifr(group.init(&dfs->blocks.remote));
    ev_timer_init(&dfs->ev.jobs, job.resume, .0, 15.0);
    dfs->ev.jobs.data = (void *)dfs;
    ev_timer_again(p->ev.loop, &dfs->ev.jobs);
    bool exists;
    char blockpath[256];
    if (os.blockfile(&p->cfg, dfs->blocks.file, sizeof(dfs->blocks.file),
                     &exists, blockpath, sizeof(blockpath)) != 0) return -1;
    if (exists) {
        printf("Loading block file %s\n", blockpath);
        ifr(root.data.load.file(&dfs->blocks.local, blockpath));
    } else {
        ifr(root.init(&dfs->blocks.local));
    }
    ifr(blocks_load(dfs));
    ifr(job.data.load(dfs));
    ifr(pthread_mutex_init(&dfs->mining.mutex, NULL));
    ifr(os.pipes(dfs->peer));
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
                     p.tracker.port, 0, 0, NULL) != 0) return -1;
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
