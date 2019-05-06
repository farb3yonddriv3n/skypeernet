#include <common.h>

int config_init(struct config_s **cfg)
{
    *cfg = malloc(sizeof(**cfg));
    if (!(*cfg)) return -1;

    if (rsa_load(*cfg) == 0)
        return 0;

    if (rsa_generate() != 0)
        return -1;

    if (rsa_load(*cfg) != 0)
        return -1;

    return 0;
}

void config_free(struct config_s *cfg)
{
    RSA_free(cfg->rsakey.public);
    RSA_free(cfg->rsakey.private);
    sn_free(cfg->key.public);
    sn_free(cfg->key.private);
    free(cfg);
}
