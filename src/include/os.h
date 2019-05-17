#ifndef OS_H_
#define OS_H_

struct module_os_s {
    int (*filepart)(const char *filename, size_t offset, size_t maxread,
                    char **dst, size_t *ndst);
    int (*filesize)(const char *filename, size_t *size);
    int (*filewrite)(const char *fname, const char *mode,
                     char *content, int ncontent);
};

const struct module_os_s os;

#endif
