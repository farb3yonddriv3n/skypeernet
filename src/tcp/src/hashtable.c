#include <stdio.h>
#include <memory.h>
#include <malloc.h>
#include <assert.h>
#include "hashtable.h"

struct ht_s *ht_init()
{
    struct ht_s *ht = malloc(sizeof(*ht));
    if (!ht) return NULL;
    memset(ht, 0, sizeof(*ht));
    ht->meta.size = sizeof(void *) * HT_SIZE;
    ht->ht = malloc(ht->meta.size);
    if(!ht->ht) return NULL;
    memset(ht->ht, 0, ht->meta.size);
    return ht;
}

void ht_free(struct ht_s *ht)
{
    struct ht_item_s *h;
    struct ht_item_s *del;
    int                  i;
    for(i = 0; i < HT_SIZE; i++) {
        for(h = ht->ht[i]; h != NULL; ) {
            if(h->flag == HT_ALLOC) free(h->v);
            free(h->k);
            del = h;
            h = h->next;
            free(del);
        }
    }
    free(ht->ht);
    free(ht);
}

inline static void ht_key(int* dst, const char* key, const int nkey)
{
    int i;
    *dst = 0;
    for(i = 0; i < nkey; i++)
        *dst += key[i];
    *dst %= HT_SIZE;
}

int ht_add(struct ht_s *ht, const char *key, const int nkey,
           const void *value, const int nvalue, const int alloc)
{
    struct ht_item_s *h;
    int                   idx = 0;
    if (!ht || !key || !value) return -1;
    ht_key(&idx, key, nkey);
    ht->meta.items++;
    for(h = ht->ht[idx]; h != NULL; h = h->next) {
        if(nkey == h->nk && memcmp(key, h->k, nkey) == 0) {
            /** first, free existing value */
            if(h->flag == HT_ALLOC) free(h->v);
            /** then copy new value */
            if(alloc == HT_ALLOC) {
                h->v = malloc(nvalue);
                if(!h->v) return -1;
                memcpy(h->v, value, nvalue);
            } else
                h->v = (void *)value;
            h->flag = alloc;
            h->nv   = nvalue;
            return 0;
        }
    }
    h = malloc(sizeof(*h));
    if(!h) return -1;
    h->nk = nkey;
    h->k  = malloc(nkey);
    if(!h->k) return -1;
    h->nv = nvalue;
    memcpy(h->k, key, nkey);
    if(alloc == HT_ALLOC) {
        h->flag = HT_ALLOC;
        h->v    = malloc(nvalue);
        if(!h->v) return -1;
        memcpy(h->v, value, nvalue);
    } else {
        h->flag = HT_NOALLOC;
        h->v    = (void*)value;
    }
    h->next = ht->ht[idx];
    ht->ht[idx] = h;
    return 0;
}

int ht_rem(struct ht_s *ht, const char *key, const int nkey)
{
    struct ht_item_s *h;
    struct ht_item_s *prev = NULL;
    int                  idx = 0;
    if (!ht || !key) return -1;
    ht_key(&idx, key, nkey);
    for(h = ht->ht[idx], prev = NULL; h != NULL; prev = h, h = h->next) {
        if(nkey == h->nk && memcmp(h->k, key, nkey) == 0) {
            if(!prev) ht->ht[idx] = h->next;
            else      prev->next  = h->next;
            if(h->flag == HT_ALLOC) free(h->v);
            free(h->k);
            free(h);
            ht->meta.items--;
            return 0;
        }
    }
    return -1;
}

struct ht_item_s *ht_get(struct ht_s *ht, const char *key, const int nkey)
{
    struct ht_item_s *h;
    int                  idx = 0;
    if (!ht || !key) return NULL;
    ht_key(&idx, key, nkey);
    if(ht->ht[idx]) {
       for(h = ht->ht[idx]; h != NULL; h = h->next) {
            if(h->nk == nkey && memcmp(h->k, key, nkey) == 0) return h;
        }
        return NULL;
    } else return NULL;
}

void ht_dump_idx(struct ht_s *ht, const char *key, const int nkey)
{
    struct ht_item_s *h;
    int                  idx;
    if (!ht || !key) return;
    ht_key(&idx, key, nkey);
    for(h = ht->ht[idx]; h != NULL; h = h->next)
        printf("idx [%d] with key [%.*s], value [%.*s]\n", idx, h->nk, h->k, h->nv, h->v);
}

void ht_map(struct ht_s *ht, void (*callback)(void*, const int, void*),
            void *userdata)
{
    struct ht_item_s *h;
    int                  i;
    for(i = 0; i < HT_SIZE; i++) {
        for(h = ht->ht[i]; h != NULL; ) {
            struct ht_item_s *item = h;
            h = h->next;
            if(callback) callback(item->v, item->nv, userdata);
        }
    }
}


int ht_items(struct ht_s *ht, int *items)
{
    if (!ht || !items) return -1;
    *items = ht->meta.items;
    return 0;
}
