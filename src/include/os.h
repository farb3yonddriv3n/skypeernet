#ifndef OS_H_
#define OS_H_

struct module_os_s {
    int (*filepart)(const char *filename, size_t offset, size_t maxread,
                    char **dst, uint64_t *ndst);
    int (*fileparts)(const char *filename, size_t maxread,
                     unsigned int *parts);
    int (*filesize)(const char *filename, uint64_t *size);
    int (*filewrite)(const char *fname, const char *mode,
                     char *content, int ncontent);
    int (*filejoin)(struct config_s *cfg, const char *fname,
                    char *fullpath, int nfullpath,
                    char *filename, int nfilename,
                    bool *finalized);
    int (*fileexists)(const char *filename, bool *exists);
    int (*filemove)(const char *src, const char *dst);
    int (*filereadable)(const char *fname, bool *hr);
    int (*partexists)(struct config_s *cfg, const char *startswith,
                      bool *exists);
    int (*blockname)(struct config_s *cfg, char *blockname, int nblockname,
                     const char *received, unsigned char *keyhash);
    int (*blockfile)(struct config_s *cfg, unsigned char *bfile,
                     int nbfile, bool *found, char *blockpath, int nblockpath);
    int (*blocksremote)(struct config_s *cfg, void *dfs,
                        int (*cb)(void *dfs, const char *filename,
                                  const char *fullpath));
    int (*makedirs)(struct config_s *cfg);
    int (*loadjson)(json_object **obj, char *content, int ncontent);
    int (*loadjsonfile)(json_object **obj, const char *filename);
    int (*gettimems)(double *result);
    const char *(*getpartsdir)();
    int (*readkeys)(struct config_s *cfg,
                    int (*cb)(struct config_s *cfg, const char *fullpath,
                              const char *filename));
    int (*pipes)(struct peer_s *p);
    int (*pipe_open)(struct peer_s *p, const char *fullpath, unsigned int flags,
                     int *fd, ev_io *ev, unsigned int evflags,
                     void (*cb)(EV_P_ ev_io *w, int revents));
};

const struct module_os_s os;

#endif
