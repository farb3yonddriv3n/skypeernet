#include <common.h>

#define MAX_JOB_CHUNK_IDLE 10.0f
#define JOBS_TMP_FILE      ".jobs.tmp"

struct job_find_s {
    unsigned char *file;
    struct job_s  *found;
};

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

static int find_job(struct list_s *l, void *uj, void *ud)
{
    struct job_s      *j  = (struct job_s *)uj;
    struct job_find_s *jf = (struct job_find_s *)ud;
    if (dmemcmp(j->file.name, sizeof(j->file.name),
                jf->file, SHA256HEX)) {
        jf->found = j;
        return 1;
    }
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

static int dump(struct config_s *cfg, struct list_s *jobs, json_object **obj)
{
    if (!cfg || !jobs || !obj) return -1;
    int cb(struct list_s *l, void *uj, void *ud) {
        struct job_s *j   = (struct job_s *)uj;
        json_object *jobs = (json_object *)ud;
        struct config_key_s *found;
        ifr(config_keyexists(cfg, j->pubkeyhash, &found));
        json_object *jj = json_object_new_object();
        json_object *name = json_object_new_string_len((const char *)j->file.name,
                                                       sizeof(j->file.name));
        json_object *size = json_object_new_int64(j->file.size);
        json_object *chunks_size = json_object_new_int64(j->chunks.size);
        json_object *counter_none = json_object_new_int(j->counter.none);
        json_object *counter_receiving = json_object_new_int(j->counter.receiving);
        json_object *counter_notfound = json_object_new_int(j->counter.notfound);
        json_object *counter_done = json_object_new_int(j->counter.done);
        json_object_object_add(jj, "name", name);
        json_object_object_add(jj, "size", size);
        json_object_object_add(jj, "chunks_size", chunks_size);
        json_object_object_add(jj, "counter_none", counter_none);
        json_object_object_add(jj, "counter_receiving", counter_receiving);
        json_object_object_add(jj, "counter_notfound", counter_notfound);
        json_object_object_add(jj, "counter_done", counter_done);
        json_object_array_add(jobs, jj);
        return 0;
    }
    *obj = json_object_new_object();
    json_object *jjobs = json_object_new_array();
    json_object_object_add(*obj, "jobs", jjobs);
    return list.map(jobs, cb, jjobs);
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
    char chunkpath[256];
    snprintf(chunkpath, sizeof(chunkpath), "%s/%.*s", p->cfg.dir.download,
                                                      (int )sizeof(jc->chunk),
                                                      jc->chunk);
    bool exists;
    if (os.fileexists(chunkpath, &exists));
    if (exists) return chunk_state(j, jc, JOBCHUNK_DONE);
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
    if (list.map(&dfs->jobs, cb, dfs) != 0)
        syslog(LOG_ERR, "Resuming jobs failed");
}

static int job_remove(struct list_s *jobs, unsigned char *file,
                      int nfile, bool *removed)
{
    if (!jobs || !file) return -1;
    if (nfile != SHA256HEX) return -1;
    *removed = false;
    struct job_find_s jf = { .file = file, .found = NULL };
    ifr(list.map(jobs, find_job, &jf));
    if (jf.found) {
        ifr(list.del(jobs, jf.found));
        *removed = true;
    }
    return 0;
}

static int job_clean(struct list_s *jobs, bool *cleaned)
{
    if (!jobs || !cleaned) return -1;
    *cleaned = true;
    return list.clean(jobs);
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

    unsigned char *desc;
    int            ndesc;
    ifr(decode_desc(f, &desc, &ndesc));
    if (!desc) return 0;
    char dst[2048], chunkpath[256];
    snprintf(dst, sizeof(dst), "%s/%.*s", cfg->dir.finalized, ndesc, desc);
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
        struct config_key_s *key;
        ifr(config_keyexists(cfg, f->pubkeyhash, &key));
        if (!key) return 0;
        unsigned char *tagdec;
        int            ntagdec;
        ifr(decx(&tagdec, &ntagdec,
            (unsigned char *)f->chunks.array[i].tag,
            strlen(f->chunks.array[i].tag),
            key));
        unsigned char *decrypted = malloc(f->chunks.array[i].size);
        if (!decrypted) return -1;
        int dc = aes_decrypt((unsigned char *)buffer, n, key->aes.key,
                             sizeof(key->aes.key), tagdec, key->aes.key,
                             key->aes.key, decrypted);
        free(tagdec);
        if (dc < 1) return -1;
        ifr(eioie_fwrite(dst, "a", (char *)decrypted, dc));
        free(buffer);
        free(decrypted);
    }
    *finalized = true;
    return 0;
}

static int update(struct peer_s *p, const char *downloaddir,
                  struct list_s *jobs, const char *filename,
                  int host, unsigned short port)
{
    if (!p || !downloaddir || !jobs || !filename) return -1;
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
        char fullpath[512];
        snprintf(fullpath, sizeof(fullpath), "%s/%s", downloaddir, filename);
        ifr(remove(fullpath));
        printf("Unwanted file %s received\n", filename);
        ifr(rogue.add(&p->rogue, host, port, ROGUE_UNWANTED_FILE));
        return 0;
    }
    ifr(chunk_state(cf.foundj, cf.foundjc, JOBCHUNK_DONE));
    if (cf.foundj->counter.done == cf.foundj->chunks.size) {
        if (p->api.cb.job.done && p->api.cb.job.done(p, cf.foundj->file.name,
                                                     (int )sizeof(cf.foundj->file.name)) != 0)
            return -1;
        printf("Job done: %.*s\n", (int )sizeof(cf.foundj->file.name),
                                   cf.foundj->file.name);
    }
    return 0;
}

static int data_load(struct distfs_s *dfs)
{
    json_object *obj;
    bool exists;
    ifr(os.fileexists(JOBS_TMP_FILE, &exists));
    if (!exists) return 0;
    ifr(os.loadjsonfile(&obj, JOBS_TMP_FILE));
    json_object *jobs;
    json_object_object_get_ex(obj, "jobs", &jobs);
    ifr(list.init(&dfs->jobs));
    if (json_object_get_type(jobs) == json_type_array) {
        array_list *jobs_array = json_object_get_array(jobs);
        int i, k;
        for (i = 0; i < array_list_length(jobs_array); i++) {
            struct job_s *j = malloc(sizeof(*j));
            if (!j) return -1;
            memset(j, 0, sizeof(*j));
            json_object *job_item = array_list_get_idx(jobs_array, i);
            json_object *tmp;
            BIND_STR(j->file.name,   "filename",   tmp, job_item);
            BIND_INT64(j->file.size, "filesize",   tmp, job_item);
            BIND_STR(j->pubkeyhash,  "pubkeyhash", tmp, job_item);
            json_object *chunks;
            json_object_object_get_ex(job_item, "chunks", &chunks);
            if (json_object_get_type(chunks) == json_type_array) {
                array_list *chunks_array = json_object_get_array(chunks);
                for (k = 0; k < array_list_length(chunks_array); k++) {
                    int idx = j->chunks.size;
                    j->chunks.array = realloc(j->chunks.array, ++j->chunks.size *
                                              sizeof(*j->chunks.array));
                    if (!j->chunks.array) return -1;
                    memset(&j->chunks.array[idx], 0, sizeof(j->chunks.array[idx]));
                    json_object *chunk = array_list_get_idx(chunks_array, k);
                    json_object *tmp;
                    BIND_STR(j->chunks.array[idx].chunk,      "chunk",   tmp, chunk);
                    BIND_INT64(j->chunks.array[idx].size,     "size",    tmp, chunk);
                    BIND_INT(j->chunks.array[idx].state,      "state",   tmp, chunk);
                    BIND_DOUBLE(j->chunks.array[idx].updated, "updated", tmp, chunk);
                    BIND_INT(j->chunks.array[idx].net.host,   "host",    tmp, chunk);
                    BIND_INT(j->chunks.array[idx].net.port,   "port",    tmp, chunk);
                }
            }
            BIND_INT(j->counter.none,      "counter_none",      tmp, job_item);
            BIND_INT(j->counter.receiving, "counter_receiving", tmp, job_item);
            BIND_INT(j->counter.notfound,  "counter_notfound",  tmp, job_item);
            BIND_INT(j->counter.done,      "counter_done",      tmp, job_item);
            ifr(list.add(&dfs->jobs, j, clean));
        }
    }
    json_object_put(obj);
    return 0;
}

static int data_save(struct distfs_s *dfs)
{
    if (!dfs) return -1;
    int cb(struct list_s *l, void *je, void *ud) {
        struct job_s       *j    = (struct job_s *)je;
        struct json_object *jobs = (struct json_object *)ud;

        json_object *jobj = json_object_new_object();
        json_object *filename = json_object_new_string_len((const char *)j->file.name,
                                                           sizeof(j->file.name));
        json_object_object_add(jobj, "filename", filename);
        json_object *filesize = json_object_new_int64(j->file.size);
        json_object_object_add(jobj, "filesize", filesize);
        json_object *pubkeyhash = json_object_new_string_len((const char *)j->pubkeyhash,
                                                             sizeof(j->pubkeyhash));
        json_object_object_add(jobj, "pubkeyhash", pubkeyhash);
        json_object *chunks = json_object_new_array();
        json_object_object_add(jobj, "chunks", chunks);
        int i;
        for (i = 0; i < j->chunks.size; i++) {
            json_object *chunk = json_object_new_object();
            json_object_array_add(chunks, chunk);
            json_object *chunkname = json_object_new_string_len((const char *)j->chunks.array[i].chunk,
                                                                 sizeof(j->chunks.array[i].chunk));
            json_object_object_add(chunk, "chunk", chunkname);
            json_object *chunksize = json_object_new_int64(j->chunks.array[i].size);
            json_object_object_add(chunk, "size", chunksize);
            json_object *chunkstate = json_object_new_int(j->chunks.array[i].state);
            json_object_object_add(chunk, "state", chunkstate);
            json_object *chunkupdated = json_object_new_double(j->chunks.array[i].updated);
            json_object_object_add(chunk, "updated", chunkupdated);
            json_object *chunkhost = json_object_new_int(j->chunks.array[i].net.host);
            json_object_object_add(chunk, "host", chunkhost);
            json_object *chunkport = json_object_new_int(j->chunks.array[i].net.port);
            json_object_object_add(chunk, "port", chunkport);
        }
        json_object *counter_none = json_object_new_int(j->counter.none);
        json_object_object_add(jobj, "counter_none", counter_none);
        json_object *counter_receiving = json_object_new_int(j->counter.receiving);
        json_object_object_add(jobj, "counter_receiving", counter_receiving);
        json_object *counter_notfound = json_object_new_int(j->counter.notfound);
        json_object_object_add(jobj, "counter_notfound", counter_notfound);
        json_object *counter_done = json_object_new_int(j->counter.done);
        json_object_object_add(jobj, "counter_done", counter_done);
        json_object_array_add(jobs, jobj);
        return 0;
    }
    json_object *r    = json_object_new_object();
    json_object *jobs = json_object_new_array();
    json_object_object_add(r, "jobs", jobs);
    ifr(list.map(&dfs->jobs, cb, jobs));
    const char *json = json_object_to_json_string(r);
    ifr(eioie_fwrite(JOBS_TMP_FILE, "w", (char *)json, strlen(json)));
    json_object_put(r);
    return 0;
}

const struct module_job_s job = {
    .add       = add,
    .update    = update,
    .resume    = resume,
    .finalize  = finalize,
    .dump      = dump,
    .remove    = job_remove,
    .clean     = job_clean,
    .data.save = data_save,
    .data.load = data_load,
};
