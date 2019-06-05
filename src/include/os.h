#ifndef OS_H_
#define OS_H_

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
    int (*fileexists)(const char *filename, bool *exists);
    int (*filemove)(const char *dst, const char *src);
    int (*partexists)(struct config_s *cfg, const char *startswith,
                      bool *exists);
    int (*blockname)(struct config_s *cfg, char *blockname, int nblockname,
                     const char *received, unsigned char *keyhash);
    int (*dldir)(struct config_s *cfg);
    int (*loadjson)(json_object **obj, char *content, int ncontent);
    int (*loadjsonfile)(json_object **obj, const char *filename);
    int (*gettimems)(double *result);
    const char *(*getpartsdir)();
};

const struct module_os_s os;

#endif
