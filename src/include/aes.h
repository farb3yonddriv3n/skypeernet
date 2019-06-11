#ifndef AES_H_
#define AES_H_

const unsigned char aes_key[32];
const unsigned char aes_iv[16];
const unsigned char aes_aad[13];
unsigned char aes_tag[16];

int aes_encrypt(unsigned char *plaintext, int plaintext_len, unsigned char *aad,
                int aad_len, unsigned char *key, unsigned char *iv,
                unsigned char *ciphertext, unsigned char *tag);
int aes_decrypt(unsigned char *ciphertext, int ciphertext_len, unsigned char *aad,
                int aad_len, unsigned char *tag, unsigned char *key, unsigned char *iv,
                unsigned char *plaintext);

#endif
