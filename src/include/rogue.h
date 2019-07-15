#ifndef ROGUE_H_
#define ROGUE_H_

enum rogue_e {
    ROGUE_UNWANTED_FILE,
    ROGUE_BLOCKCHAIN_TOOBIG,
};

struct rogue_s {
    int            host;
    unsigned short port;
    enum rogue_e   reason;
    int            count;
};

struct module_rogue_s {
    int (*add)(struct list_s *l, int host, unsigned short port,
               enum rogue_e reason);
    int (*dump)(struct list_s *l, json_object **obj);
};

const struct module_rogue_s rogue;

#endif
