#include <common.h>

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

const struct module_os_s os = {
    .filepart  = filepart,
    .filesize  = filesize,
    .filewrite = filewrite,
};
