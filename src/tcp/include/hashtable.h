#ifndef HASHTABLE_H_
#define HASHTABLE_H_

#define HT_SIZE    (1<<12)
#define HT_NOALLOC 0x0
#define HT_ALLOC   0x1

#define HT_ADD(mht, mkey, mnkey, mvalue, mnvalue)\
    ht_add(mht, mkey, mnkey, mvalue, mnvalue, HT_ALLOC)

#define HT_ADDL(mht, mkey, mvalue, mnvalue)\
    ht_add(mht, mkey, strlen(mkey), mvalue, mnvalue, HT_ALLOC)

#define HT_ADD_WA(mht, mkey, mnkey, mvalue, mnvalue)\
    ht_add(mht, mkey, mnkey, mvalue, mnvalue, HT_NOALLOC)

#define HT_GETL(mht, mkey)\
    ht_get(mht, mkey, strlen(mkey))

struct ht_s {
    char*        k;
    int          nk;
    char*        v;
    int          nv;
    unsigned int flag;
    struct ht_s* next;
};

struct ht_s** ht_init();
void ht_free(struct ht_s** ht);
int ht_add(struct ht_s** ht, const char* key, const int nkey,
           const void* value, const int nvalue, const int alloc);
int ht_rem(struct ht_s** ht, const char* key, const int nkey);
struct ht_s *ht_get(struct ht_s** ht, const char* key, const int nkey);
void ht_dump_index(struct ht_s** ht, const char* key, const int nkey);
void ht_map(struct ht_s** ht, void (*callback)(void*, const int, void*),
            void* userdata);
#endif
