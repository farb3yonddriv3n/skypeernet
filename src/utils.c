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

    if (!dst) return -1;
    *dst = NULL;

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
    if (f->enc.scheme == FILE_SCHEME_PLAIN) {
        int n = strnlen(f->meta.description.enc,
                        sizeof(f->meta.description.enc));
        *desc = malloc(n);
        if (!*desc) return -1;
        memcpy(*desc, f->meta.description.enc, n);
        *ndesc = n;
        return 0;
    }
    if (f->enc.scheme == FILE_SCHEME_CEK) {
        unsigned char *cek;
        ifr(file_cek_resolve(f, &cek));
        if (!cek) return 0;
        size_t         ndecoded;
        unsigned char *decoded;
        decoded = base64_decode((unsigned char *)f->meta.description.enc,
                                strnlen(f->meta.description.enc,
                                        sizeof(f->meta.description.enc)),
                                &ndecoded);
        if (!decoded) return -1;
        if (ndecoded <= AES_IV_SIZE + AES_TAG_SIZE) {
            free(decoded);
            return -1;
        }
        int nct = ndecoded - AES_IV_SIZE - AES_TAG_SIZE;
        unsigned char *out = malloc(nct);
        if (!out) {
            free(decoded);
            return -1;
        }
        int n = aes_decrypt(decoded + AES_IV_SIZE + AES_TAG_SIZE, nct,
                            NULL, 0,
                            decoded + AES_IV_SIZE, cek,
                            decoded, out);
        free(decoded);
        if (n < 1) {
            free(out);
            return -1;
        }
        *desc  = out;
        *ndesc = n;
        return 0;
    }
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

int unique_together(char *output, int noutput, int *offset, int params, ...)
{
    va_list args;
    va_start(args, params);
    *offset = 0;
    while (params-- > 0) {
        char *buf = va_arg(args, char*);
        int n = va_arg(args, int);
        if (*offset + n > noutput) return -1;
        memcpy(output + *offset, buf, n);
        *offset += n;
    }
    va_end(args);
    return 0;
}

int bindump(char *buffer, int nbuffer)
{
    if (!buffer || nbuffer < 1) return -1;
    int i;
    printf("Dumping %d bytes\n", nbuffer);
    for (i = 0; i < nbuffer; i++) {
        printf("%x ", buffer[i]);
    }
    printf("\nDump ended\n");
    return 0;
}
