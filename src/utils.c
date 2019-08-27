#include <common.h>

int encx(unsigned char **dst, size_t *ndst,
         unsigned char *src, int nsrc,
         int *k)
{
    if (!dst || !ndst || !src) return -1;
    unsigned char *encrypted;
    int            nencrypted;
    ifr(rsa_encrypt(psig->cfg.keys.local.rsa.public, src,
                    nsrc, &encrypted, &nencrypted));
    *dst = base64_encode(encrypted, nencrypted, ndst);
    if (!(*dst)) return -1;
    if (encrypted) free(encrypted);
    return 0;
}

int decx(unsigned char **dst, int *ndst,
         unsigned char *src, int nsrc,
         struct config_key_s *key)
{
    if (!dst || !ndst || !src || !key) return -1;
    size_t         ndecoded;
    unsigned char *decoded = base64_decode(src, nsrc, &ndecoded);
    if (!decoded) return -1;
    ifr(rsa_decrypt(key->rsa.private, decoded, ndecoded,
                    dst, ndst));
    if (decoded) free(decoded);
    return 0;
}

int eioie_fwrite(const char *fname, const char *mode, char *content, int ncontent)
{
    FILE *pfile;
    int result;

    pfile = fopen(fname, mode);
    if (pfile == NULL) {
        return -1;
    }

    result = fwrite(content, sizeof(char), ncontent, pfile);

    fclose(pfile);
    return !(result == ncontent);
}

int eioie_fread(char **dst, sn fname)
{
    FILE *pfile;
    int   lsize;
    char *buffer;
    int   result;

    char fnbuffer[1024];
    snprintf(fnbuffer, sizeof(fnbuffer), "%.*s", sn_p(fname));
    pfile = fopen(fnbuffer, "rb");
    if (pfile == NULL) {
        return -1;
    }

    fseek(pfile, 0, SEEK_END);
    lsize = ftell(pfile);
    rewind(pfile);

    if (lsize > MAX_FILE_SIZE || lsize == -1) {
        fclose(pfile);
        return -1;
    }

    buffer = malloc(sizeof(char) * lsize);
    if (buffer == NULL) {
        fclose(pfile);
        return -1;
    }

    result = fread(buffer, sizeof(char), lsize, pfile);
    if (result != lsize) {
        fclose(pfile);
        free(buffer);
        return -1;
    }

    *dst = buffer;

    fclose(pfile);
    return result;
}

void bin2hexstr(char *dst, size_t dstlen,
                char *src, size_t srclen)
{
    char tmp[8];
    char *step = src;
    int i, n;
    for (i = 0; i < srclen; i++) {
        snprintf(tmp, sizeof(tmp), "%02x", (unsigned char)(*step));
        n = strlen(tmp);
        if ((step - src + n) > dstlen) break;
        memcpy(dst, tmp, n);
        dst += n;
        step++;
    }
}

int decode_desc(struct file_s *f, unsigned char **desc, int *ndesc)
{
    if (!f || !desc || !ndesc) return -1;
    struct config_key_s *key;
    *desc  = NULL;
    *ndesc = 0;
    ifr(config_keyexists(&psig->cfg, f->pubkeyhash,
                         &key));
    if (key) {
        ifr(decx(desc, ndesc, (unsigned char *)f->meta.description.enc,
                 strlen(f->meta.description.enc), key));
    }
    return 0;
}

void swap_memory(char *dst, int ndst)
{
    int i, j;
    for (i = 0, j = ndst - 1; (i < (ndst / 2) && j > 0); i++, j--) {
        dst[j] ^= dst[i];
        dst[i] ^= dst[j];
        dst[j] ^= dst[i];
    }
}
