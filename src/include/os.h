#ifndef OS_H_
#define OS_H_

#define PARTS_DIR "parts"

struct module_os_s {
    int (*filepart)(const char *filename, size_t offset, size_t maxread,
                    char **dst, size_t *ndst);
    int (*fileparts)(const char *filename, size_t maxread,
                     unsigned int *parts);
    int (*filesize)(const char *filename, size_t *size);
    int (*filewrite)(const char *fname, const char *mode,
                     char *content, int ncontent);
    int (*filejoin)(struct config_s *cfg, const char *fname, char *received,
                    int nreceived, bool *finalized);
    int (*dldir)(struct config_s *cfg);
    int (*loadjson)(json_object **obj, const char *filename);
    int (*gettimems)(double *result);
};

const struct module_os_s os;

#endif
