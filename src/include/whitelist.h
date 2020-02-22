#ifndef WHITELIST_H_
#define WHITELIST_H_

enum wl_e {
    WL_ADD,
    WL_REM,
};

struct module_whitelist_s {
    int (*addrem)(struct peer_s *p, const char *pubhash,
                  enum wl_e action);
    int (*list)(struct peer_s *p);
    int (*exists)(struct peer_s *p, const char *pubhash, bool *exists);
    int (*load)(struct peer_s *p);
};

extern const struct module_whitelist_s whitelist;

#endif
