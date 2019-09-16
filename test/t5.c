#include <common.h>
#include <cu.h>

void t5_aes()
{
    const char *text = "Lazy fox jumped over the dog.";
    unsigned char encoded[128];
    const char *key = "aaaabbbbccccaaaaaaaabbbbccccaaaa";
    unsigned char tag[128];
    const char *iv  = "aaaabbbbccccdddd";
    int n = aes_encrypt((unsigned char *)text, strlen(text),
                        NULL, 0,
                        (unsigned char *)key,
                        (unsigned char *)iv,
                        encoded, tag);
    unsigned char decrypted[128];
    int nd = aes_decrypt(encoded, n, NULL, 0, tag,
                         (unsigned char *)key, (unsigned char *)iv, decrypted);
    A(nd, strlen(text));
    printf("Decrypted [%.*s]\n", nd, decrypted);
}
