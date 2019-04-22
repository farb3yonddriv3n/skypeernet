#include <common.h>

static int t_file_add(const char *filename, struct block_s *b)
{
    struct transaction_param_s param;
    param.type = TFILE_ADD;
    sn_setz(param.action.add.name, (char *)filename);

    struct transaction_s *t;
    int ret = transaction.init(&t, &param);
    if (ret != 0) return ret;

    bool valid;
    ret = transaction.validate(t, &valid);
    if (ret != 0) return ret;
    printf("Transaction file add valid: %d\n", valid);

    block.transaction.add(b, t);

    return 0;
}

int main()
{
    struct config_s *cfg;
    int ret = config_init(&cfg);
    if (ret != 0) return ret;


    const char *files[] = { "1012019.pdf", "1012019.pdf" };
    struct root_s r;

    root.init(&r);

    unsigned char prev_block[SHA256HEX];
    memset(prev_block, 97, sizeof(prev_block));

    int i;
    for (i = 0; i < 3; i++) {
        uint64_t nounce;
        unsigned char newblock[SHA256HEX];
        block.mine(prev_block, newblock, &nounce);

        bool valid;
        block.validate(prev_block, newblock, nounce, &valid);
        printf("Block valid %d\n", valid);

        struct block_s *b;
        block.init(&b, prev_block, newblock, nounce, i);

        int t;
        for (t = 0; t < COUNOF(files); t++) {
            t_file_add(files[t], b);
        }

        block.transaction.hash(b);

        root.add(&r, b);

        memcpy(prev_block, newblock, sizeof(newblock));
    }

    root.data.export(&r);

    return 0;
}

/*
int main()
{
    struct config_s *cfg;
    int ret = config_init(&cfg);
    if (ret != 0) return ret;

    struct transaction_s t;
    struct transaction_param_s param;

    param.type = TFILE_ADD;
    sn_setz(param.action.add.name, "1012019.pdf");

    ret = transaction.init(&t, &param);
    if (ret != 0) return ret;

    ret = transaction.dump(&t);
    if (ret != 0) return ret;

    bool valid;
    ret = transaction.validate(&t, &valid);
    if (ret != 0) return ret;
    printf("valid: %d\n", valid);

    uint64_t nounce;
    unsigned char newblock[SHA256HEX];
    block.mine(transaction.hash(&t), newblock, &nounce);

    block.validate(transaction.hash(&t), newblock, nounce, &valid);
    printf("valid %d\n", valid);

    struct block_s b;
    block.init(&b, transaction.hash(&t), newblock, nounce);

    block.transaction.add(&b, &t);
    block.transaction.hash(&b);

    block.data.export(&b);

    return 0;
}
*/

/*
int main()
{
    struct config_s *cfg;
    int ret = config_init(&cfg);
    if (ret != 0) return ret;

    struct transaction_s t;
    struct transaction_param_s param;

    param.type = TFILE_ADD;
    sn_setz(param.action.add.name, "1012019.pdf");

    ret = transaction.init(&t, &param);
    if (ret != 0) return ret;

    ret = transaction.dump(&t);
    if (ret != 0) return ret;

    bool valid;
    ret = transaction.validate(&t, &valid);
    if (ret != 0) return ret;
    printf("valid: %d\n", valid);

    return 0;
}
*/

/*
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
*/

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
