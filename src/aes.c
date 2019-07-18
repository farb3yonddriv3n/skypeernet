#include <common.h>

static void handleErrors(void)
{
    unsigned long errCode;
    while ((errCode = ERR_get_error())) {
        char *err = ERR_error_string(errCode, NULL);
        syslog(LOG_ERR, "AES error: %s", err);
    }
}

int aes_encrypt(unsigned char *plaintext, int plaintext_len, unsigned char *aad,
                int aad_len, unsigned char *key, unsigned char *iv,
                unsigned char *ciphertext, unsigned char *tag)
{
    EVP_CIPHER_CTX *ctx = NULL;
    int len = 0, ciphertext_len = 0;
    if (!(ctx = EVP_CIPHER_CTX_new())) {
        handleErrors();
        return -1;
    }
    if (1 != EVP_EncryptInit_ex(ctx, EVP_aes_256_gcm(), NULL, NULL, NULL)) {
        handleErrors();
        return -1;
    }
    if (1 != EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_GCM_SET_IVLEN, 16, NULL)) {
        handleErrors();
        return -1;
    }
    if (1 != EVP_EncryptInit_ex(ctx, NULL, NULL, key, iv)) handleErrors();
    if (aad && aad_len > 0) {
        if (1 != EVP_EncryptUpdate(ctx, NULL, &len, aad, aad_len)) {
            handleErrors();
            return -1;
        }
    }
    if (plaintext) {
        if (1 != EVP_EncryptUpdate(ctx, ciphertext, &len, plaintext, plaintext_len)) {
            handleErrors();
            return -1;
        }
        ciphertext_len = len;
    }
    if (1 != EVP_EncryptFinal_ex(ctx, ciphertext + len, &len)) {
        handleErrors();
        return -1;
    }
    ciphertext_len += len;
    if (1 != EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_GCM_GET_TAG, 16, tag)) {
        handleErrors();
        return -1;
    }
    EVP_CIPHER_CTX_free(ctx);
    return ciphertext_len;
}

int aes_decrypt(unsigned char *ciphertext, int ciphertext_len, unsigned char *aad,
                int aad_len, unsigned char *tag, unsigned char *key, unsigned char *iv,
                unsigned char *plaintext)
{
    EVP_CIPHER_CTX *ctx = NULL;
    int len = 0, plaintext_len = 0, ret;
    if (!(ctx = EVP_CIPHER_CTX_new())) {
        handleErrors();
        return -1;
    }
    if (!EVP_DecryptInit_ex(ctx, EVP_aes_256_gcm(), NULL, NULL, NULL)) {
        handleErrors();
        return -1;
    }
    if (!EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_GCM_SET_IVLEN, 16, NULL)) {
        handleErrors();
        return -1;
    }
    if (!EVP_DecryptInit_ex(ctx, NULL, NULL, key, iv)) {
        handleErrors();
        return -1;
    }
    if (aad && aad_len > 0) {
        if (!EVP_DecryptUpdate(ctx, NULL, &len, aad, aad_len)) {
            handleErrors();
            return -1;
        }
    }
    if (ciphertext) {
        if (!EVP_DecryptUpdate(ctx, plaintext, &len, ciphertext, ciphertext_len)) {
            handleErrors();
            return -1;
        }
        plaintext_len = len;
    }
    if (!EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_GCM_SET_TAG, 16, tag)) {
        handleErrors();
        return -1;
    }
    ret = EVP_DecryptFinal_ex(ctx, plaintext + len, &len);
    EVP_CIPHER_CTX_free(ctx);
    if (ret > 0) {
        plaintext_len += len;
        return plaintext_len;
    } else {
        handleErrors();
        return -1;
    }
}
