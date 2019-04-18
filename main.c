#include <common.h>

int main()
{
    struct config_s *cfg;
    int ret = config_init(&cfg);
    if (ret != 0) return ret;

    unsigned char *encrypt;
    int enclen;

    ret = rsa_encrypt(cfg->rsakey.public, (const unsigned char *)"asdf", 4,
                      &encrypt, &enclen);
    if (ret != 0) return ret;

    unsigned char *decrypted;
    int ndecrypted;
    ret = rsa_decrypt(cfg->rsakey.private, encrypt, enclen,
                      &decrypted, &ndecrypted);
    if (ret != 0) return ret;

    printf("[%.*s] %.*s\n", ndecrypted, decrypted, sn_p(cfg->key.private));

    free(encrypt);
    free(decrypted);
    config_free(cfg);

    return 0;
}

/*
int main()
{
    char *file;
    int len = eioie_fread(&file, "1012019.pdf");
    if (len <= 0) return 1;

    printf("%d bytes read\n", len);
    unsigned char md[SHA256_DIGEST_LENGTH];
    SHA256((const unsigned char *)file, len, md);

    int nchunks;
    struct file_chunk_s *chunks;
    int ret = file_chunks(file, len, &chunks, &nchunks);
    if (ret != 0) return -1;

    int i;
    printf("nchunks %d\n", nchunks);
    for (i = 0; i < nchunks; i++) {
        printf("%d %p\n", chunks[i].n, chunks[i].ptr);
        int n;
        for (n = 0; n < SHA256_DIGEST_LENGTH; n++)
            printf("%02x", (unsigned char)chunks[i].hash[n]);
        putchar('\n');
    }
    int n;
    for (n = 0; n < SHA256_DIGEST_LENGTH; n++)
        printf("%02x", (unsigned char)md[n]);
    putchar('\n');

    return 0;
}
*/
