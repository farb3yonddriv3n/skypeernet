#include <common.h>

#define MAX_JOB_CHUNK_IDLE 10.0f

static int clean(void *uj)
{
    if (!uj) return -1;
    struct job_s *j = (struct job_s *)uj;
    free(j->chunks.array);
    free(j);
    return 0;
}

static int counter(struct job_s *j, struct job_chunk_s *jc,
                   enum job_counter_e action)
{
    if (!j || !jc) return -1;
#define COUNTER(m_type, m_dst)\
    case m_type:\
        if (action == CHUNKCOUNTER_ADD) m_dst++;\
        else                            m_dst--;\
        break;
    switch (jc->state) {
        COUNTER(JOBCHUNK_NONE,      j->counter.none)
        COUNTER(JOBCHUNK_NOTFOUND,  j->counter.notfound)
        COUNTER(JOBCHUNK_RECEIVING, j->counter.receiving)
        COUNTER(JOBCHUNK_DONE,      j->counter.done)
        default:
            return -1;
    }
    return 0;
}

static int chunk_state(struct job_s *j, struct job_chunk_s *jc,
                       enum job_chunk_state_e state)
{
    if (!j || !jc) return -1;
    ifr(counter(j, jc, CHUNKCOUNTER_REM));
    jc->state = state;
    ifr(counter(j, jc, CHUNKCOUNTER_ADD));
    return 0;
}

static int add(struct config_s *cfg, struct list_s *jobs, struct group_s *remote,
               unsigned char *file, int nfile, bool *found,
               bool *added, bool *exists)
{
    if (!jobs || !remote || !file || !found) return -1;
    if (nfile != SHA256HEX) return -1;
    char filename[256];
    snprintf(filename, sizeof(filename), "%s/%.*s", cfg->dir.finalized, nfile, file);
    if (os.fileexists(filename, exists));
    if (*exists) return 0;
    *found = *added = false;
    struct job_find_s { unsigned char *file; struct job_s *found; };
    int find_job(struct list_s *l, void *uj, void *ud) {
        struct job_s      *j  = (struct job_s *)uj;
        struct job_find_s *jf = (struct job_find_s *)ud;
        if (dmemcmp(j->file.name, sizeof(j->file.name),
                    jf->file, SHA256HEX)) {
            jf->found = j;
            return 1;
        }
        return 0;
    }
    struct job_find_s jf = { .file = file, .found = NULL };
    ifr(list.map(jobs, find_job, &jf));
    if (jf.found) {
        *added = true;
        *found = true;
        return 0;
    }
    struct file_s *f = NULL;
    ifr(group.find.transaction(remote, file, (void **)&f, NULL, NULL));
    if (!f) return 0;
    *found = true;

    struct job_s *j = malloc(sizeof(*j));
    if (!j) return -1;
    memset(j, 0, sizeof(*j));
    memcpy(j->file.name, file, nfile);
    memcpy(j->pubkeyhash, f->pubkeyhash, sizeof(j->pubkeyhash));
    j->file.size = f->meta.size;
    int i;
    for (i = 0; i < f->chunks.size; i++) {
        j->chunks.array = realloc(j->chunks.array, ++j->chunks.size * sizeof(*j));
        if (!j->chunks.array) return -1;
        int idx = j->chunks.size - 1;
        memcpy(j->chunks.array[idx].chunk, f->chunks.array[i].hash.content,
               sizeof(f->chunks.array[i].hash.content));
        j->chunks.array[idx].size     = f->chunks.array[i].size;
        j->chunks.array[idx].state    = JOBCHUNK_NONE;
        j->chunks.array[idx].updated  = .0f;
        j->chunks.array[idx].net.host = 0;
        j->chunks.array[idx].net.port = 0;
        j->counter.none++;
    }
    ifr(list.add(jobs, j, clean));
    *added = true;
    return 0;
}

static int show(struct config_s *cfg, struct list_s *jobs)
{
    if (!jobs) return -1;
    int cb(struct list_s *l, void *uj, void *ud) {
        struct job_s *j  = (struct job_s *)uj;
        struct config_key_s *found;
        ifr(config_keyexists(cfg, j->pubkeyhash, &found));
        printf("| %.*s | %10dkB | %5ld | %d/%d/%d/%d | %s |\n",
               (int)sizeof(j->file.name), j->file.name,
               (int)(j->file.size / 1024), j->chunks.size, j->counter.none,
               j->counter.receiving, j->counter.notfound,
               j->counter.done, found ? "Yes" : "No");
        return 0;
    }
    printf("| File | Size | Chunks | Counter| Decryptable |\n");
    return list.map(jobs, cb, NULL);
}

static int chunk_find(struct distfs_s *dfs, unsigned char *filename,
                      int *host, unsigned short *port)
{
    int cb(unsigned char *pubkeyhash, void *data) {
        struct distfs_s    *dfs = (struct distfs_s *)data;
        struct world_peer_s wp  = { .found = NULL };
        memcpy(wp.pubkeyhash, pubkeyhash, sizeof(wp.pubkeyhash));
        ifr(list.map(&dfs->peer->peers, world.peer.findpubkeyhash, &wp));
        if (wp.found) {
            *host = wp.found->host;
            *port = wp.found->port;
            return 1;
        }
        return 0;
    }
    struct file_s *f = NULL;
    return group.find.transaction(dfs->blocks.remote, filename,
                                  (void **)&f, dfs, cb);
}

static int chunk_start(struct distfs_s *dfs, struct job_s *j,
                       struct job_chunk_s *jc)
{
    if (!dfs || !j) return -1;
    struct peer_s *p = dfs->peer;
    ifr(chunk_find(dfs, j->file.name, &jc->net.host,
                   &jc->net.port));
    if (jc->net.host == 0 && jc->net.port == 0)
        return chunk_state(j, jc, JOBCHUNK_NOTFOUND);
    ifr(chunk_state(j, jc, JOBCHUNK_RECEIVING));
    ifr(os.gettimems(&jc->updated));
    p->send_buffer.type            = BUFFER_FILEASK;
    p->send_buffer.u.fileask.file  = j->file.name;
    p->send_buffer.u.fileask.chunk = jc->chunk;
    return payload.send(p, COMMAND_FILEASK,
                        jc->net.host, jc->net.port, 0, 0,
                        NULL);
}

static int chunk_restart(struct distfs_s *dfs, struct job_s *j,
                         struct job_chunk_s *jc)
{
    if (!dfs || !j || !jc) return -1;
    bool reachable;
    ifr(world.peer.isreachable(dfs->peer, jc->net.host, jc->net.port,
                               &reachable));
    if (!reachable) return chunk_start(dfs, j, jc);
    return 0;
}

static void resume(struct ev_loop *loop, struct ev_timer *timer, int revents)
{
    struct distfs_s *dfs = (struct distfs_s *)timer->data;
    if (!dfs) return;
    int cb(struct list_s *l, void *uj, void *ud) {
        struct job_s    *j   = (struct job_s *)uj;
        struct distfs_s *dfs = (struct distfs_s *)ud;
        double timems;
        ifr(os.gettimems(&timems));
        int i;
        for (i = 0; i < j->chunks.size; i++) {
            switch (j->chunks.array[i].state) {
                case JOBCHUNK_NONE: {
                    ifr(chunk_start(dfs, j, &j->chunks.array[i]));
                    } break;
                case JOBCHUNK_NOTFOUND:
                case JOBCHUNK_RECEIVING: {
                    if ((timems - MAX_JOB_CHUNK_IDLE) > j->chunks.array[i].updated) {
                        ifr(chunk_restart(dfs, j, &j->chunks.array[i]));
                    }
                    } break;
                default:
                    break;
            }
        }
        return 0;
    }
    assert(list.map(&dfs->jobs, cb, dfs) == 0);
}

static int finalize(struct config_s *cfg, struct group_s *remote, unsigned char *file,
                    int nfile, bool *finalized)
{
    if (!remote || !file || !finalized) return -1;
    if (nfile != SHA256HEX) return -1;
    *finalized = false;
    struct file_s *f    = NULL;
    ifr(group.find.transaction(remote, file, (void **)&f,
                               NULL, NULL));
    if (!f) return -1;

    char dst[256], chunkpath[256];
    snprintf(dst, sizeof(dst), "%s/%.*s", cfg->dir.finalized, SHA256HEX, file);
    remove(dst);
    int i;
    for (i = 0; i < f->chunks.size; i++) {
        snprintf(chunkpath, sizeof(chunkpath), "%s/%.*s", cfg->dir.download,
                                                          (int)sizeof(f->chunks.array[i].hash.content),
                                                          f->chunks.array[i].hash.content);
        char *buffer;
        sn_initz(cn, chunkpath);
        int n = eioie_fread(&buffer, cn);
        if (n <= 0) return -1;
        size_t ntag;
        unsigned char *tag = base64_decode((unsigned char *)f->chunks.array[i].tag,
                                           strlen(f->chunks.array[i].tag),
                                           &ntag);
        unsigned char *tagdec;
        int            ntagdec;
        struct config_key_s *key;
        ifr(config_keyexists(cfg, f->pubkeyhash, &key));
        if (!key) return 0;
        ifr(rsa_decrypt(key->rsa.private, tag, ntag,
                        &tagdec, &ntagdec));
        unsigned char decrypted[CHUNK_SIZE];
        int dc = aes_decrypt((unsigned char *)buffer, n, key->aes.key,
                             sizeof(key->aes.key), tagdec, key->aes.key,
                             key->aes.key, decrypted);
        if (dc < 1) return -1;
        ifr(eioie_fwrite(dst, "a", (char *)decrypted, dc));
        free(buffer);
    }
    *finalized = true;
    return 0;
}

static int update(const char *downloaddir, struct list_s *jobs,
                  const char *filename)
{
    if (!jobs || !filename) return -1;
    struct chunk_find_s { const char         *filename;
                          struct job_s       *foundj;
                          struct job_chunk_s *foundjc; };
    int cb(struct list_s *l, void *uj, void *ud) {
        struct job_s *j         = (struct job_s *)uj;
        struct chunk_find_s *cf = (struct chunk_find_s *)ud;
        int i;
        for (i = 0; i < j->chunks.size; i++) {
            if (dmemcmp(j->chunks.array[i].chunk, sizeof(j->chunks.array[i].chunk),
                        filename, SHA256HEX)) {
                cf->foundj  = j;
                cf->foundjc = &j->chunks.array[i];
                return 1;
            }
        }
        return 0;
    }
    struct chunk_find_s cf = { .filename = filename,
                               .foundj   = NULL,
                               .foundjc  = NULL };
    ifr(list.map(jobs, cb, &cf));
    if (!cf.foundjc) {
        printf("Unwanted file %s received\n", filename);
        return 0;
    }
    ifr(chunk_state(cf.foundj, cf.foundjc, JOBCHUNK_DONE));
    if (cf.foundj->counter.done == cf.foundj->chunks.size) {
        printf("Job done: %.*s\n", (int)sizeof(cf.foundj->file.name),
                                   cf.foundj->file.name);
        //ifr(list.del(jobs, cf.foundj));
    }
    return 0;
}

static int import()
{
    return 0;
}

static int export()
{
    return 0;
}

const struct module_job_s job = {
    .add      = add,
    .update   = update,
    .resume   = resume,
    .finalize = finalize,
    .show     = show,
};
