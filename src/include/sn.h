#ifndef SN_H_
#define SN_H_

/*
 * Initialize m_dst with m_src
 */
#define sn_init(m_dst, m_src)\
    sn m_dst = { .s = m_src.s, .n = m_src.n, .offset = m_src.offset }

/*
 * Initialize SN with an external raw buffer and length
 */
#define sn_initr(m_dst, m_src_s, m_src_n)\
    sn m_dst = { .s = m_src_s, .n = m_src_n, .offset = 0 }

/*
 * Initialize SN with an external zero terminated buffer
 */
#define sn_initz(m_dst, m_src)\
    sn m_dst = { .s = m_src, .n = strlen(m_src), .offset = 0 }

/*
 * Set existing string m_var with string m_src
 */
#define sn_set(m_var, m_src)\
    m_var.s      = m_src.s;\
    m_var.n      = m_src.n;\
    m_var.offset = m_src.offset;

/*
 * Set existing string m_var with an external raw buffer and length
 */
#define sn_setr(m_var, m_src, m_src_n)\
    m_var.s      = m_src;\
    m_var.n      = m_src_n;\
    m_var.offset = 0;

/*
 * Set existing string m_var with an external zero terminated buffer
 */
#define sn_setz(m_var, m_src)\
    m_var.s      = m_src;\
    m_var.n      = strlen(m_src);\
    m_var.offset = 0;

/*
 * String length
 */
#define sn_len(m_var) m_var.n

/*
 * Format to printf()
 */
#define sn_p(m_var) m_var.n, m_var.s

/*
 * Append to m_dst sn
 *
 */
#define sn_append(m_dst, m_src, m_nsrc)\
    m_dst.s = realloc(m_dst.s, m_dst.n + m_nsrc);\
    memcpy(m_dst.s + m_dst.n, m_src, m_nsrc);\
    m_dst.n += m_nsrc;

/*
 * Allocate m_size bytes from heap
 *
 * See sn_bytes_append(), sn_bytes_delete()
 */
#define sn_bytes_new(m_var, m_size)\
    sn m_var;\
    sn_bytes_init_new(m_var, m_size);
/*
 * Allocate m_size bytes from heap
 *
 * See sn_bytes_append(), sn_bytes_delete()
 */
#define sn_bytes_init_new(m_var, m_size)\
    m_var.s = malloc(m_size);\
    if (!m_var.s) return -1;\
    m_var.n = m_size;\
    m_var.offset = 0;

/*
 * Append m_src string to m_var string
 * Warning: this is not aligned
 *
 * See sn_bytes_new(), sn_bytes_delete()
 */
/*
#define sn_bytes_append(m_var, m_src)\
    assert((m_var.offset + m_src.n) <= m_var.n);\
    memcpy(m_var.s + m_var.offset, m_src.s, m_src.n);\
    m_var.offset += m_src.n;
*/

/*
 * Free allocated m_var allocated
 *
 * See sn_bytes_new(), sn_bytes_append()
 */
#define sn_bytes_delete(m_var)\
    if (m_var.s) free(m_var.s);\
    m_var.s = NULL;\
    m_var.n = m_var.offset = 0;

/*
 * Copy string m_src to buffer of chars m_var with size m_size
 */
#define sn_to_char(m_var, m_src, m_size)\
    char buf##m_var[m_size];\
    snprintf(buf##m_var, sizeof(buf##m_var), "%.*s", m_src.n, m_src.s);\
    char *m_var = buf##m_var;

/*
 * Convert string m_src to int m_var
 * while m_size is conversion buffer size
 */
#define sn_atoi(m_var, m_src, m_size)\
    char buf##m_var[m_size];\
    snprintf(buf##m_var, sizeof(buf##m_var), "%.*s", m_src.n, m_src.s);\
    int m_var = atoi(buf##m_var);

/*
 * Convert int m_src to string m_var
 * while m_size is conversion buffer size
 */
#define sn_itoa(m_var, m_src, m_size)\
    char buf##m_var[m_size];\
    snprintf(buf##m_var, sizeof(buf##m_var), "%d", m_src);\
    sn_initz(m_var, buf##m_var);

/*
 * Zero initialize m_dst
 */
#define snb_zero(m_dst)\
    snb m_dst = { .n = 0, .offset = 0 }

/*
 * Copy m_src string to m_dst
 * Only if destination space is sufficient
 *
 * See struct snb_s
 */
#define snb_cpy_ds(m_dst, m_src)\
    m_dst.n = 0;\
    if (sizeof(m_dst.s) >= m_src.n) {\
        m_dst.n = m_src.n;\
        memcpy(m_dst.s, m_src.s, m_src.n);\
    } else { abort(); }

/*
 * Copy m_src string to m_dst
 * Only if destination space is sufficient
 *
 * See struct snb_s
 */
#define snb_cpy_d(m_dst, m_src)\
    m_dst->n = 0;\
    if (sizeof(m_dst->s) >= m_src.n) {\
        m_dst->n = m_src.n;\
        memcpy(m_dst->s, m_src.s, m_src.n);\
    } else { abort(); }

/*
 * Compare m_dst and m_src strings
 */
#define sn_memcmp(m_dst, m_dst_n, m_src, m_src_n)\
    (m_dst_n == m_src_n && memcmp(m_dst, m_src, m_dst_n) == 0)
#define sn_cmps(m_dst, m_src)\
    sn_memcmp(m_dst.s, m_dst.n, m_src.s, m_src.n)

/*
 * Set SN number
 */
#define sn_num_set(n_num, n_src)\
    ((n_num.n == sizeof(int)) ? *(int *)(n_num.s) = n_src :\
    ((n_num.n == sizeof(short)) ? *(short *)(n_num.s) = n_src :\
    ((n_num.n == sizeof(char)) ? *(char *)(n_num.s) = n_src : 0 )))

/*
 * Get SN number
 */
#define sn_num(n_num)\
    ((n_num.n == sizeof(int)) ? *(int *)(n_num.s) :\
    ((n_num.n == sizeof(short)) ? *(short *)(n_num.s) :\
    ((n_num.n == sizeof(char)) ? *(char *)(n_num.s) : 0 )))

/*
 * Free SN
 */
#define sn_free(dst)\
    if (dst.n > 0) {\
        if (dst.s) free(dst.s);\
        dst.s = NULL;\
        dst.n = dst.offset = 0;\
    }

/**
 * @brief Memory region S of N characters.
 *
 * Defines memory region for later manipulation.
 */
typedef struct sn_s {
    char *s;     /**< Pointer to a memory region */
    int  n;      /**< Size of memory region */
    int  offset; /**< Offset from start of region */
} sn;

/**
 * @brief Preallocated memory region S of sizeof(S) characters.
 *
 * Defines memory region for later manipulation
 */
typedef struct snb_s {
    char s[1024]; /**< Pointer to a memory region */
    int  n;       /**< Size of memory region snb#s */
    int  offset;  /**< Offset from start of region */
} snb;

inline static int sn_bytes_append_raw(char *buffer, int *offset, int size,
                                  char *src, int nsrc)
{
    if (nsrc + *offset > size) return -1;
    memcpy(buffer + *offset, src, nsrc);
    *offset += nsrc;
    return 0;
}

inline static int sn_bytes_append(sn *dst, char *src, int nsrc)
{
    return sn_bytes_append_raw(dst->s, &dst->offset, dst->n, src, nsrc);
}

inline static int snb_bytes_append(snb *dst, char *src, int nsrc)
{
    return sn_bytes_append_raw(dst->s, &dst->offset, dst->n, src, nsrc);
}

inline static int sn_read(char *dst, int ndst, sn *src)
{
    if (src->offset + ndst > src->n) return -1;
    memcpy(dst, src->s + src->offset, ndst);
    src->offset += ndst;
    return 0;
}

#endif
