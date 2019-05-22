#include <common.h>
#include <dirent.h>

#define TMP_DIR "./tmp/"

static int filepart(const char *filename, size_t offset, size_t maxread,
                    char **dst, size_t *ndst)
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

static int append(struct list_s *l)
{
    void **files = NULL;
    int nfiles, i;
    ifr(list.toarray_sort(l, &files, &nfiles, LIST_ARRAY_SORT_STR));
    for (i = 0; i < nfiles; i++) {
        char *fname = ((char **)files)[i];
        char *buffer;
        char fbuffer[1024];
        snprintf(fbuffer, sizeof(fbuffer), "%s%s", TMP_DIR, fname);
        sn_initz(fn, fbuffer);
        int n = eioie_fread(&buffer, fn);
        if (n <= 0) return -1;
        if (eioie_fwrite("test.bin", "a", buffer, n) != 0) return -1;
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

static int filejoin(const char *fname, char *received)
{
    struct list_s files;
    ifr(list.init(&files));
    int host, port, tidx, gidx, pidx, parts;
    int found = sscanf(fname, "tmp/%d_%d_%d_%d_%d_%d.part",
                               &host, &port, &tidx,
                               &gidx, &pidx, &parts);
    if (found != 6) return -1;
    DIR *dir;
    struct dirent *ent;
    if ((dir = opendir(TMP_DIR)) == NULL) return -1;
    while ((ent = readdir(dir)) != NULL) {
        int shost, sport, stidx;
        found = sscanf(ent->d_name, "%d_%d_%d", &shost, &sport, &stidx);
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
    int size;
    ifr(list.size(&files, &size));
    if (size == parts) {
        ifr(append(&files));
    }
    ifr(list.clean(&files));
    return 0;
}

const struct module_os_s os = {
    .filepart  = filepart,
    .fileparts = fileparts,
    .filesize  = filesize,
    .filewrite = filewrite,
    .filejoin  = filejoin,
};
