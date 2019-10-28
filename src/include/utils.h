#ifndef UTILS_H_
#define UTILS_H_

#define FLAG(m_n)     (1 << m_n)
#define CMPFLAG(m_src, m_f) ((m_src & m_f) == m_f)

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

#define BIND_STR(m_dst, m_name, m_src, m_obj)\
        json_object_object_get_ex(m_obj, m_name, &m_src);\
        if (json_object_get_string_len(m_src) == sizeof(m_dst))\
            memcpy(m_dst, json_object_get_string(m_src),\
                   json_object_get_string_len(m_src));

#define BIND_STRLEN(m_dst, m_name, m_src, m_obj)\
        json_object_object_get_ex(m_obj, m_name, &m_src);\
        if (json_object_get_string_len(m_src) < sizeof(m_dst))\
            snprintf(m_dst, sizeof(m_dst), "%.*s",\
                     json_object_get_string_len(m_src),\
                     json_object_get_string(m_src));

#define BIND_INT(m_dst, m_name, m_src, m_obj)\
        json_object_object_get_ex(m_obj, m_name, &m_src);\
        m_dst = json_object_get_int64(m_src);

#define BIND_INT64(m_dst, m_name, m_src, m_obj)\
        json_object_object_get_ex(m_obj, m_name, &m_src);\
        m_dst = json_object_get_int64(m_src);

#define BIND_DOUBLE(m_dst, m_name, m_src, m_obj)\
        json_object_object_get_ex(m_obj, m_name, &m_src);\
        m_dst = json_object_get_double(m_src);

#define LMAP(m_func, m_struct)\
    int m_func(struct list_s *m_l, void *m_ex, void *m_ud) {\
        struct m_struct *dst = (struct m_struct *)m_ex;\
        struct m_struct *src = (struct m_struct *)m_ud;

#define LMAPE\
        return 0;\
    }

#define LCOND(m_cond)\
    if (m_cond) {\
        src->found = dst;\
        return 1;\
    }

#define SEND_MSG(m_peer, m_host, m_port, m_msg)\
    m_peer->send_buffer.type = BUFFER_MESSAGE;\
    m_peer->send_buffer.u.message.str = m_msg;\
    ifr(payload.send(m_peer, COMMAND_MESSAGE,\
                     m_host, m_port, 0, 0,\
                     NULL, NULL));

int eioie_fwrite(const char *fname, const char *mode, char *content, int ncontent);
int eioie_fread(char **dst, sn fname);
void bin2hexstr(char *dst, size_t dstlen,
                char *src, size_t srclen);
int encx(unsigned char **dst, size_t *ndst,
         unsigned char *src, int nsrc, int *k);
struct config_key_s;
int decx(unsigned char **dst, int *ndst,
         unsigned char *src, int nsrc,
         struct config_key_s *key);

inline static void sha256hex(const unsigned char *src, const int srclen,
                             unsigned char *hex)
{
    unsigned char md[SHA256_DIGEST_LENGTH];
    SHA256(src, srclen, md);
    bin2hexstr((char *)hex, SHA256HEX, (char *)md, sizeof(md));
}
struct file_s;
int decode_desc(struct file_s *f, unsigned char **desc, int *ndesc);
void swap_memory(char *dst, int ndst);

#endif
