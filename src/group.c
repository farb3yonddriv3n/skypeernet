#include <common.h>
#include <dirent.h>

static int mallocz(struct group_s **g)
{
    *g = malloc(sizeof(**g));
    if (!(*g)) return -1;
    memset(*g, 0, sizeof(**g));
    return 0;
}

static int init(struct group_s **g)
{
    if (mallocz(g) != 0) return -1;
    memset(*g, 0, sizeof(**g));
    return 0;
}

static int find_root(struct group_s *g,
                     unsigned char *h,
                     struct root_s **found)
{
    int i;
    for (i = 0; i < g->roots.size; i++) {
        if (dmemcmp(h, SHA256HEX,
                    g->roots.array[i]->pubkeyhash,
                    sizeof(g->roots.array[i]->pubkeyhash))) {
            *found = g->roots.array[i];
            break;
        }
    }
    return 0;
}

static int find_transaction(struct group_s *g,
                            unsigned char *h,
                            void **found,
                            void *data,
                            int (*cb)(unsigned char *pubkeyhash,
                                      void *data))
{
    if (!g || !h || !found) return -1;
    int i;
    for (i = 0; i < g->roots.size; i++) {
        *found = NULL;
        if (root.find(g->roots.array[i], h, found) != 0) return -1;
        if (!(*found)) continue;
        if (cb) {
            int ret = cb(g->roots.array[i]->pubkeyhash, data);
            if (ret == -1) return ret;
            if (ret ==  1) break;
        } else break;
    }
    return 0;
}

static int clean(struct group_s *g)
{
    if (!g) return -1;
    int i;
    for (i = 0; i < g->roots.size; i++) {
        if (root.clean(g->roots.array[i]) != 0) return -1;
    }
    free(g->roots.array);
    free(g);
    return 0;
}

static int dump(struct peer_s *p, struct group_s *g, struct config_s *cfg,
                json_object **obj)
{
    if (!g || !cfg) return -1;
    int i;
    *obj = json_object_new_object();
    json_object *roots = json_object_new_array();
    json_object_object_add(*obj, "roots", roots);
    for (i = 0; i < g->roots.size; i++) {
        json_object *robj;
        if (root.dump(p, g->roots.array[i], cfg, &robj) != 0) return -1;
        json_object_array_add(roots, robj);
    }
    return 0;
}

static void filename(char *buffer, size_t nbuffer, char *fname, int nfname)
{
    snprintf(buffer, nbuffer, "%s/%.*s", psig->cfg.dir.block, nfname, fname);
}

static int compare(struct group_s *local, struct group_s *remote,
                   bool *equal)
{
    if (!local || !remote) return -1;
    *equal = true;
    if (local->roots.size != remote->roots.size) {
        *equal = false;
        return 0;
    }
    int i, j;
    bool found;
    struct root_diff_s diff;
    for (i = 0; i < local->roots.size; i++) {
        found = false;
        for (j = 0; j < remote->roots.size; j++) {
            if (memcmp(local->roots.array[i]->hash,
                       remote->roots.array[j]->hash,
                       sizeof(local->roots.array[i]->hash)) == 0) {
                found = true;
                if (root.compare(local->roots.array[i],
                                 remote->roots.array[j],
                                 &diff) != 0) return -1;
                if (diff.equal == false) {
                    *equal = false;
                    return 0;
                }
            }
        }
        if (!found) {
            *equal = false;
            return 0;
        }
    }
    return 0;
}

static int validate(struct group_s *g, bool *valid)
{
    if (!g || !valid) return -1;
    int i;
    for (i = 0; i < g->roots.size; i++) {
        if (root.validate(g->roots.array[i], valid) != 0) return -1;
        if (*valid == false) break;
    }
    return 0;
}

static int db_load(struct group_s *g)
{
    if (!g) return -1;
    DIR *dir;
    struct dirent *ent;
    if ((dir = opendir(psig->cfg.dir.block)) == NULL) return -1;
    while ((ent = readdir(dir)) != NULL) {
        char fname[1024];
        filename(fname, sizeof(fname), ent->d_name, strlen(ent->d_name));
        if (strlen(ent->d_name) != SHA256HEX) continue;
        struct root_s *r;
        if (root.data.load.file(&r, fname) != 0) return -1;
        if (group.roots.add(g, r) != 0) return -1;
    }
    closedir(dir);
    return 0;
}

static int export(struct group_s *g, void *ud,
                  int (*cb)(json_object *obj, void *ud))
{
    if (!g || !cb) return -1;
    int i;
    for (i = 0; i < g->roots.size; i++) {
        struct root_s *r = g->roots.array[i];
        json_object *dst;
        if (root.data.save.object(r, &dst) != 0) return -1;
        ifr(cb(dst, ud));
    }
    return 0;
}

static int db_save(struct group_s *g)
{
    if (!g) return -1;
    int i;
    for (i = 0; i < g->roots.size; i++) {
        struct root_s *r = g->roots.array[i];
        json_object *dst;
        if (root.data.save.object(r, &dst) != 0) return -1;
        const char *root_json = json_object_to_json_string(dst);
        char fname[1024];
        filename(fname, sizeof(fname), (char *)r->hash, sizeof(r->hash));
        if (eioie_fwrite(fname, "w", (char *)root_json, strlen(root_json)) != 0)
            return -1;
        json_object_put(dst);
    }
    return 0;
}

static int roots_add(struct group_s *g, struct root_s *r)
{
    if (!g || !r) return -1;
    g->roots.size++;
    g->roots.array = realloc(g->roots.array,
                             sizeof(void *) * g->roots.size);
    if (!g->roots.array) return -1;
    g->roots.array[g->roots.size - 1] = r;
    return 0;
}

const struct module_group_s group = {
    .init             = init,
    .compare          = compare,
    .validate         = validate,
    .dump             = dump,
    .export           = export,
    .find.transaction = find_transaction,
    .find.root        = find_root,
    .clean            = clean,
    .roots.add        = roots_add,
    .db.save          = db_save,
    .db.load          = db_load,
};
