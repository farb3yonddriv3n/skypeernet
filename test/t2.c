#include <common.h>
#include <cu.h>

#define STR    "aaaabbbbccccddddeeee"
#define STRLEN (sizeof(STR) - 1)

void t4_rsa_encrypt_decrypt()
{
    struct config_s cfg;
    A(config_init(&cfg), 0);

    unsigned char *encrypt;
    int enclen;

    int ret = rsa_encrypt(cfg.keys.local.rsa.public, (const unsigned char *)STR, STRLEN,
                          &encrypt, &enclen);
    A(ret, 0);

    unsigned char *decrypted;
    int ndecrypted;
    ret = rsa_decrypt(cfg.keys.local.rsa.private, encrypt, enclen,
                      &decrypted, &ndecrypted);
    A(ret, 0);

    A(!(ndecrypted == STRLEN), 0);
    A(memcmp(decrypted, STR, STRLEN), 0);

    free(encrypt);
    free(decrypted);
    config_free(&cfg);
}
