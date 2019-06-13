#ifndef RSA_H_
#define RSA_H_

int rsa_load(struct config_s *cfg);
int rsa_generate();
int rsa_find(struct config_s *cfg, unsigned char *keyhash,
             bool *found);
int rsa_decrypt(RSA *privkey, const unsigned char *src, const int srclen,
                unsigned char **dst, int *dstlen);
int rsa_encrypt(RSA *keypair, const unsigned char *src, const int srclen,
                unsigned char **dst, int *dstlen);

#endif
