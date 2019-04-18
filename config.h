#ifndef CONFIG_H_
#define CONFIG_H_

struct config_s {
    struct {
        RSA *public;
        RSA *private;
    } rsakey;
    struct {
        sn public;
        sn private;
    } key;
};

int config_init(struct config_s **cfg);
void config_free(struct config_s *cfg);

#endif
