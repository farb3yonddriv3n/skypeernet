#include <common.h>

enum distfs_cmd_e {
    DISTFS_HELLO,
};

static const struct { enum distfs_cmd_e cmd;
                      int (*write)(struct distfs_s *dfs, int host, unsigned short port);
                      int (*read)(struct distfs_s *dfs);
                    } dfs_cmds[] = {
    { DISTFS_HELLO, dfs_hello, NULL },
};

static int message(struct peer_s *p, int host,
                   unsigned short port,
                   char *msg, int len)
{
    if (!p || !msg) return -1;
    struct distfs_s *dfs = (struct distfs_s *)p->user.data;
    printf("Message %.*s from %x:%d\n", len, msg, host, port);
    ifr(p->api.cb.message(p, host, port, msg, len));
    if (!(dmemcmp(MSG_HELLO, MSG_HELLO_SIZE, msg, len) &&
        strnlen((char *)dfs->blocks.file, sizeof(dfs->blocks.file)) > 0))
        return 0;
    syslog(LOG_INFO, "Peer %x:%d asked for blockfile", host, port);
    ifr(dfs_block_send(p, dfs, host, port));
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
                     NULL, NULL));
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
                            host, port, filename, TASK_FILE_KEEP, 0);
        }
    }
    return 0;
}

static int dfile(struct peer_s *p, struct header_s *header,
                 int host, unsigned short port,
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
    if (header->tcp.reqtype != TCP_NONE) {
        char *dst;
        sn_initr(snfile, fullpath, nfullpath);
        int ndst = eioie_fread(&dst, snfile);
        if (ndst <= 0) return -1;
        if (header->tcp.reqtype == TCP_REQUEST) {
            ifr(endpoint.request(p, header, host, port, dst, ndst));
        } else if (header->tcp.reqtype == TCP_RESPONSE) {
            ifr(tunnel.response(p, header, dst, ndst));
        }
        if (dst) free(dst);
    } else if (humanreadable && root.data.load.file(&src, fullpath) == 0) {
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
        ifr(job.update(p, p->cfg.dir.download, &dfs->jobs,
                       filename, host, port));
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

static int dfs_list_files(struct distfs_s *dfs, char **argv, int argc,
                          int *dfserr)
{
    if (!dfs || !argv || !dfserr) return -1;
    json_object *obj;
    if (strcmp(argv[1], "remote") == 0 ||
        strcmp(argv[1], "r") == 0) {
        ifr(group.dump(dfs->peer, dfs->blocks.remote, &dfs->peer->cfg, &obj));
    } else if (strcmp(argv[1], "local") == 0 ||
               strcmp(argv[1], "l") == 0) {
        ifr(root.dump(dfs->peer, dfs->blocks.local, &dfs->peer->cfg, &obj));
    } else return -1;
    printf("%s\n", json_object_to_json_string_ext(obj, JSON_C_TO_STRING_PRETTY));
    json_object_put(obj);
    return 0;
}

static int dfs_job_remove(struct distfs_s *dfs, char **argv, int argc,
                          int *dfserr)
{
    if (!dfs || !argv || !dfserr) return -1;
    unsigned char *h = (unsigned char *)argv[1];
    bool removed;
    ifr(job.remove(&dfs->jobs, h, strlen((const char *)h), &removed));
    if (removed) printf ("Job %s removed\n", h);
    else         printf ("Unable to remove job %s\n", h);
    return 0;
}

static int dfs_job_clean(struct distfs_s *dfs, char **argv, int argc,
                         int *dfserr)
{
    if (!dfs || !dfserr) return -1;
    bool cleaned;
    ifr(job.clean(&dfs->jobs, &cleaned));
    if (cleaned) printf ("Jobs cleaned\n");
    else         printf ("No jobs to clean\n");
    return 0;
}

static int dfs_job_dump(struct distfs_s *dfs, char **argv, int argc,
                        int *dfserr)
{
    if (!dfs || !dfserr) return -1;
    json_object *obj;
    ifr(job.dump(&dfs->peer->cfg, &dfs->jobs, &obj));
    printf("%s\n", json_object_to_json_string_ext(obj, JSON_C_TO_STRING_PRETTY));
    json_object_put(obj);
    return 0;
}

static int dfs_rogue_dump(struct distfs_s *dfs, char **argv, int argc,
                          int *dfserr)
{
    if (!dfs || !dfserr) return -1;
    json_object *obj;
    ifr(rogue.dump(&dfs->peer->rogue, &obj));
    printf("%s\n", json_object_to_json_string_ext(obj, JSON_C_TO_STRING_PRETTY));
    json_object_put(obj);
    return 0;
}

static int dfs_task_dump(struct distfs_s *dfs, char **argv, int argc,
                         int *dfserr)
{
    if (!dfs || !dfserr) return -1;
    json_object *obj;
    ifr(task.dump(dfs->peer, &obj));
    printf("%s\n", json_object_to_json_string_ext(obj, JSON_C_TO_STRING_PRETTY));
    json_object_put(obj);
    return 0;
}

static int dfs_version_dump(struct distfs_s *dfs, char **argv, int argc,
                            int *dfserr)
{
    if (!dfs || !dfserr) return -1;
    json_object *obj;
    ifr(version.dump(&obj));
    printf("%s\n", json_object_to_json_string_ext(obj, JSON_C_TO_STRING_PRETTY));
    json_object_put(obj);
    return 0;
}

static int dfs_task_cancel(struct distfs_s *dfs, char **argv, int argc,
                           int *dfserr)
{
    if (!dfs || !argv || !dfserr) return -1;
    unsigned int idx = strtol(argv[1], NULL, 10);
    bool cancelled;
    ifr(task.cancel(dfs->peer, idx, &cancelled));
    if (cancelled) printf("Task %d cancelled\n", idx);
    else           printf("Task %d failed to cancel\n", idx);
    return  0;
}

static int dfs_keysdump(struct distfs_s *dfs, char **argv, int argc,
                        int *dfserr)
{
    if (!dfs || !dfserr) return -1;
    return config_keysdump(&dfs->peer->cfg);
}

static const struct { const char *alias[8];
                      int         nalias;
                      int         argc;
                      int         (*cb)(struct distfs_s *dfs, char **argv,
                                        int argc, int *dfserr);
                    } cmds[] = {
    { { "ta", "tadd"  },        2, 2, dfs_transaction_add   },
    { { "ts", "tshare"  },      2, 1, dfs_transaction_share },
    { { "tl", "tlist" },        2, 0, dfs_transaction_list  },
    { { "bm", "bmine" },        2, 0, dfs_block_mine        },
    { { "ba", "blocksaction" }, 2, 1, dfs_block_xet         },
    { { "lf", "listfiles" },    2, 1, dfs_list_files        },
    { { "ja", "jobadd" },       2, 1, dfs_job_add           },
    { { "jd", "jobdump" },      2, 0, dfs_job_dump          },
    { { "jf", "jobfinalize" },  2, 1, dfs_job_finalize      },
    { { "jr", "jobremove" },    2, 1, dfs_job_remove        },
    { { "jc", "jobclean" },     2, 0, dfs_job_clean         },
    { { "kd", "keysdump" },     2, 0, dfs_keysdump          },
    { { "rd", "rdump" },        2, 0, dfs_rogue_dump        },
    { { "ad", "taskdump" },     2, 0, dfs_task_dump         },
    { { "ac", "taskcancel" },   2, 1, dfs_task_cancel       },
    { { "v",  "versiondump" },  2, 0, dfs_version_dump      },
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
    int dfserr = 0;
    return cmds[idx].cb(dfs, argv, argc, &dfserr);
}

static int clean(struct peer_s *p, void *data)
{
    struct distfs_s *dfs = (struct distfs_s *)data;
    ev_timer_stop(p->ev.loop, &dfs->ev.jobs);
    ev_timer_stop(p->ev.loop, &dfs->ev.update);
    ifr(group.clean(dfs->blocks.remote));
    ifr(root.clean(dfs->blocks.local));
    ifr(list.clean(&dfs->transactions));
    ifr(job.data.save(dfs));
    ifr(list.clean(&dfs->jobs));
    ifr(list.clean(&p->rogue));
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
    p->api.cb.job.done = api_job_done;
    dfs->peer          = p;
    ifr(group.init(&dfs->blocks.remote));
    ev_timer_init(&dfs->ev.jobs, job.resume, .0, 15.0);
    dfs->ev.jobs.data = (void *)dfs;
    ev_timer_again(p->ev.loop, &dfs->ev.jobs);
    ev_timer_init(&dfs->ev.update, api_update, .0, 0.5);
    dfs->ev.update.data = (void *)p;
    ev_timer_again(p->ev.loop, &dfs->ev.update);
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
                     p.tracker.port,
                     0, 0, NULL, NULL) != 0) return -1;
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
