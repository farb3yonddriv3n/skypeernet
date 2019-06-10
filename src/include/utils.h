#ifndef UTILS_H_
#define UTILS_H_

#define FLAG(m_n)     (1 << m_n)

#define MAX_FILE_SIZE (1024 * 1024 * 1024) // 1GB

#define SHA256HEX     (SHA256_DIGEST_LENGTH * 2)

#define COUNTOF(m_a)  (sizeof(m_a) / sizeof(m_a[0]))

#define ALIGN16(m_src) char align16[m_src]

#define ifr(m_src) if (m_src != 0) {\
        backtrace.add(__FILE__, __LINE__);\
        return -1;\
    }

#define dmemcmp(m_src, m_nsrc, m_dst, m_ndst)\
    (m_nsrc == m_ndst && memcmp(m_src, m_dst, m_ndst) == 0)

int eioie_fwrite(const char *fname, const char *mode, char *content, int ncontent);
int eioie_fread(char **dst, sn fname);
void bin2hexstr(char *dst, size_t dstlen,
                char *src, size_t srclen);

inline static void sha256hex(const unsigned char *src, const int srclen,
                             unsigned char *hex)
{
    unsigned char md[SHA256_DIGEST_LENGTH];
    SHA256(src, srclen, md);
    bin2hexstr((char *)hex, SHA256HEX, (char *)md, sizeof(md));
}

#endif
