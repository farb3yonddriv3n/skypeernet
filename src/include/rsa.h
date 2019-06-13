#ifndef RSA_H_
#define RSA_H_

int rsa_loadkey(RSA *(*func)(FILE *pfile, RSA **x, pem_password_cb *cb, void *u),
                const char *key, RSA **ctx,
                sn *str, unsigned char *hash);
int rsa_load(struct config_s *cfg);
int rsa_generate();
int rsa_decrypt(RSA *privkey, const unsigned char *src, const int srclen,
                unsigned char **dst, int *dstlen);
int rsa_encrypt(RSA *keypair, const unsigned char *src, const int srclen,
                unsigned char **dst, int *dstlen);

#endif
