#ifndef LOCALPORT_H_
#define LOCALPORT_H_

struct module_localport_s {
    int (*load)(struct config_s *cfg, unsigned short *port);
    int (*save)(struct config_s *cfg, unsigned short port);
};

const struct module_localport_s localport;

#endif
