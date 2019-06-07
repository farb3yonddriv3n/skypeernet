#include <common.h>
#include <dirent.h>
#include <time.h>

static const char *dirs[] = { "parts", "blocks" };

static int load_json(json_object **obj, char *content, int ncontent)
{
    if (!obj || !content || ncontent < 1) return -1;
    struct json_tokener *tok = json_tokener_new();
    *obj = json_tokener_parse_ex(tok, content, ncontent);
    json_tokener_free(tok);
    if (!*obj) return -1;
    return 0;
}

static int load_json_file(json_object **obj, const char *filename)
{
    if (!obj || !filename) return -1;
    sn_initz(fn, (char *)filename);
    char *content;
    int n = eioie_fread(&content, fn);
    if (n <= 1) return -1;
    if (load_json(obj, content, n) != 0) return -1;
    if (content) free(content);
    return 0;
}

static int fileexists(const char *filename, bool *exists)
{
    FILE *pfile;
    pfile = fopen(filename, "r");
    *exists = false;
    if (pfile) *exists = true;
    return 0;
}

static int filepart(const char *filename, size_t offset, size_t maxread,
                    char **dst, uint64_t *ndst)
{
    if (!filename) return -1;
    FILE *pfile;
    pfile = fopen(filename, "rb");
    if (!pfile) return -1;
    fseek(pfile, 0, SEEK_END);
    size_t size = ftell(pfile);
    if (offset >= size) return -1;
    fseek(pfile, offset, SEEK_SET);
    *ndst = (maxread > size - offset) ?
            size - offset :
            maxread;
    *dst = malloc(*ndst);
    if (*dst == NULL) {
        fclose(pfile);
        return -1;
    }
    size_t result = fread(*dst, sizeof(char), *ndst, pfile);
    if (result != *ndst) {
        fclose(pfile);
        free(*dst);
        return -1;
    }
    fclose(pfile);
    return 0;
}

static int fileparts(const char *filename, size_t maxread,
                     unsigned int *parts)
{
    size_t size;
    if (os.filesize(filename, &size) != 0) return -1;
    *parts = size / maxread;
    if (size % maxread != 0) (*parts)++;
    return 0;
}

static int filesize(const char *filename, size_t *size)
{
    if (!filename || !size) return -1;
    FILE *pfile;
    pfile = fopen(filename, "rb");
    if (!pfile) return -1;
    fseek(pfile, 0, SEEK_END);
    *size = ftell(pfile);
    fclose(pfile);
    return 0;
}

static int filewrite(const char *fname, const char *mode,
                     char *content, int ncontent)
{
    FILE *pfile;
    int result;
    pfile = fopen(fname, mode);
    if (pfile == NULL) return -1;
    result = fwrite(content, sizeof(char), ncontent, pfile);
    fclose(pfile);
    return (result == ncontent) ? 0 : -1;
}

static int gettimems(double *result)
{
    long   ms;
    time_t s;
    struct timespec spec;
    clock_gettime(CLOCK_REALTIME, &spec);
    s  = spec.tv_sec;
    ms = round(spec.tv_nsec / 1.0e6);
    *result = s + ((double)ms / 1000);
    return 0;
}

static int gettime(char *buffer, size_t nbuffer)
{
    double result;
    if (gettimems(&result) != 0) return -1;
    snprintf(buffer, nbuffer, "%lf", result);
    return 0;
}

static int finalize(struct config_s *cfg, struct list_s *l,
                    char *dstname, int ndstname)
{
    void **files = NULL;
    int nfiles, i;
    ifr(list.toarray_sort(l, &files, &nfiles, LIST_ARRAY_SORT_STR));
    char timems[128];
    ifr(gettime(timems, sizeof(timems)));
    snprintf(dstname, ndstname, "%s/%s", cfg->download_dir, timems);
    for (i = 0; i < nfiles; i++) {
        char *fname = ((char **)files)[i];
        char *buffer;
        char fbuffer[1024];
        snprintf(fbuffer, sizeof(fbuffer), "%s/%s/%s",
                                           cfg->download_dir,
                                           dirs[0], fname);
        sn_initz(fn, fbuffer);
        int n = eioie_fread(&buffer, fn);
        if (n <= 0) return -1;
        if (eioie_fwrite(dstname, "a", buffer, n) != 0) return -1;
        free(buffer);
        if (remove(fbuffer) != 0) return -1;
    }
    if (files) free(files);
    return 0;
}

static int filejoin_clean(void *fname)
{
    if (!fname) return -1;
    free(fname);
    return 0;
}

static int filejoin(struct config_s *cfg, const char *fname, char *received,
                    int nreceived, bool *finalized)
{
    struct list_s files;
    ifr(list.init(&files));
    int host, port, tidx, gidx, pidx, parts;
    int found = sscanf(fname, "%x_%d_%d_%d_%d_%d.part",
                               &host, &port, &tidx,
                               &gidx, &pidx, &parts);
    if (found != 6) return -1;
    DIR *dir;
    struct dirent *ent;
    char partsdir[256];
    snprintf(partsdir, sizeof(partsdir), "%s/%s/", cfg->download_dir, dirs[0]);
    if ((dir = opendir(partsdir)) == NULL) return -1;
    while ((ent = readdir(dir)) != NULL) {
        int shost, sport, stidx;
        found = sscanf(ent->d_name, "%x_%d_%d", &shost, &sport, &stidx);
        if (found != 3) continue;
        if (host != shost || port != sport || tidx != stidx) continue;
        int len = strlen(ent->d_name) + 1;
        char *name = malloc(len);
        if (!name) return -1;
        memcpy(name, ent->d_name, len - 1);
        name[len - 1] = 0;
        ifr(list.add(&files, (void *)name, filejoin_clean));
    }
    closedir(dir);
    *finalized = false;
    int size;
    ifr(list.size(&files, &size));
    if (size == parts) {
        ifr(finalize(cfg, &files, received, nreceived));
        *finalized = true;
    }
    ifr(list.clean(&files));
    return 0;
}

static int dldir(struct config_s *cfg)
{
    if (!cfg) return -1;
    struct stat st = {0};
    char partsdir[256];
    if (stat(cfg->download_dir, &st) == -1) {
        if (mkdir(cfg->download_dir, 0700) != 0) return -1;
    }
    int i;
    for (i = 0; i < COUNTOF(dirs); i++) {
        snprintf(partsdir, sizeof(partsdir), "%s/%s/", cfg->download_dir, dirs[i]);
        if (stat(partsdir, &st) == -1) {
            if (mkdir(partsdir, 0700) != 0) return -1;
        }
    }
    return 0;
}

static const char *getpartsdir()
{
    return dirs[0];
}

static int filemove(const char *dst, const char *src)
{
    if (!dst || !src) return -1;
    if (rename(dst, src) != 0) return -1;
    return 0;
}

static int blockname(struct config_s *cfg, char *blockname, int nblockname,
                     const char *received, unsigned char *keyhash)
{
    if (!blockname || !received || !keyhash) return -1;
    snprintf(blockname, nblockname, "%s/%s/%.*s",
                                    cfg->download_dir,
                                    dirs[1],
                                    SHA256HEX, keyhash);
    return 0;
}

static int partexists(struct config_s *cfg, const char *startswith,
                      bool *exists)
{
    if (!startswith || !exists) return -1;
    *exists = false;
    DIR *dir;
    struct dirent *ent;
    char partsdir[256];
    snprintf(partsdir, sizeof(partsdir), "%s/%s/", cfg->download_dir, dirs[0]);
    if ((dir = opendir(partsdir)) == NULL) return -1;
    while ((ent = readdir(dir)) != NULL) {
        if (strlen(ent->d_name) < strlen(startswith)) continue;
        if (dmemcmp(startswith, strlen(startswith),
                    ent->d_name, strlen(startswith))) {
            *exists = true;
            break;
        }
    }
    closedir(dir);
    return 0;
}

const struct module_os_s os = {
    .filepart      = filepart,
    .fileparts     = fileparts,
    .filesize      = filesize,
    .filewrite     = filewrite,
    .filejoin      = filejoin,
    .fileexists    = fileexists,
    .filemove      = filemove,
    .partexists    = partexists,
    .blockname     = blockname,
    .dldir         = dldir,
    .loadjsonfile  = load_json_file,
    .loadjson      = load_json,
    .gettimems     = gettimems,
    .getpartsdir   = getpartsdir,
};
