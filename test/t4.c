#include <common.h>
#include <cu.h>

void t4_rsa_encrypt_decrypt()
{
    struct config_s cfg;
    A(config_init(&cfg), 0);

    unsigned char *encrypt;
    int enclen;

    int ret = rsa_encrypt(cfg.keys.local.rsa.public, (const unsigned char *)"asdf", 4,
                          &encrypt, &enclen);
    A(ret, 0);

    unsigned char *decrypted;
    int ndecrypted;
    ret = rsa_decrypt(cfg.keys.local.rsa.private, encrypt, enclen,
                      &decrypted, &ndecrypted);
    A(ret, 0);

    printf("[%.*s] %.*s\n", ndecrypted, decrypted, sn_p(cfg.keys.local.str.private));

    free(encrypt);
    free(decrypted);
    config_free(&cfg);
}
