#include <common.h>

#define QUEUE_SIZE 100

struct list_internal_s {
    struct list_internal_s *prev;
    struct list_internal_s *next;
    struct {
        void *ptr;
        int (*clean)(void *ptr);
    } data;
};

static int filter_find(struct list_s *l, const char *column,
                       struct ht_s **found);

static int init(struct list_s *l)
{
    if (!l) return -1;
    memset(l, 0, sizeof(*l));
    return 0;
}

static int clean_items(struct list_s *l)
{
    if (!l) return -1;
    struct list_internal_s *li, *d;
    for (li = l->head; li != NULL; ) {
        if (li->data.clean && li->data.clean(li->data.ptr) != 0) return -1;
        d = li;
        li = li->next;
        free(d);
    }
    l->head = l->tail = NULL;
    l->size = 0;
    return 0;
}

static int reset(struct list_s *l)
{
    if (!l) return -1;
    ifr(clean_items(l));
    int i;
    for (i = 0; i < l->filter.ncolumns; i++) {
        ht_free(l->filter.columns[i].ht);
        l->filter.columns[i].ht = ht_init();
    }
    return 0;
}

static int column_init(struct list_s *l, struct list_param_s *params,
                       int nparams)
{
    if (!l) return -1;
    memset(l, 0, sizeof(*l));
    l->filter.ncolumns = nparams;
    l->filter.columns = malloc(nparams * sizeof(*(l->filter.columns)));
    if (!l->filter.columns) return -1;
    int i;
    for (i = 0; i < nparams; i++) {
        snprintf(l->filter.columns[i].name, sizeof(l->filter.columns[i].name),
                 "%s", params[i].name);
        l->filter.columns[i].ht = ht_init();
        if (!l->filter.columns[i].ht) return -1;
    }
    return 0;
}

static int init_internal(struct list_internal_s **li)
{
    *li = malloc(sizeof(**li));
    if (!*li) return -1;
    memset(*li, 0, sizeof(**li));
    return 0;
}

static int add_internal(struct list_s *l, void *userdata,
                        int (*clean)(void *ptr),
                        struct list_internal_s **li)
{
    if (!l || !userdata) return -1;
    if (init_internal(li) != 0) return -1;
    (*li)->data.ptr   = userdata;
    (*li)->data.clean = clean;
    (*li)->next       = NULL;
    (*li)->prev       = NULL;
    (*li)->prev       = l->tail;
    if (l->tail) {
        l->tail->next = *li;
        l->tail       = *li;
    } else l->head = l->tail = *li;
    l->size++;
    return 0;
}

static int column_add(struct list_s *l, void *userdata,
                      int (*columns)(struct list_s *l, void *li, void *userdata),
                      int (*clean)(void *ptr))
{
    if (!l || !userdata || !columns) return -1;
    struct list_internal_s *li;
    ifr(add_internal(l, userdata, clean, &li));
    if (columns) return columns(l, li, userdata);
    return 0;
}

static int add(struct list_s *l, void *userdata,
               int (*clean)(void *ptr))
{
    if (!l || !userdata) return -1;
    struct list_internal_s *li;
    return add_internal(l, userdata, clean, &li);
}

static int add_head(struct list_s *l, void *userdata, int (*clean)(void *ptr))
{
    if (!l || !userdata) return -1;
    struct list_internal_s *li;
    if (init_internal(&li) != 0) return -1;
    li->data.ptr   = userdata;
    li->data.clean = clean;
    li->next       = l->head;
    li->prev       = NULL;
    if (l->head) {
        l->head->prev = li;
        l->head       = li;
    }
    else l->head = l->tail = li;
    l->size++;
    return 0;
}

static int del_item(struct list_s *l, struct list_internal_s *li)
{
    if (!l || !li) return -1;
    if (li->prev) li->prev->next = li->next;
    else l->head = li->next;
    if (li->next) li->next->prev = li->prev;
    else l->tail = li->prev;
    l->size--;
    if (li->data.clean && li->data.clean(li->data.ptr) != 0) return -1;
    free(li);
    return 0;
}

static int column_del(struct list_s *l, const char *column,
                      void *key, int nkey)
{
    if (!l || !column) return -1;
    struct ht_s *found;
    ifr(filter_find(l, column, &found));
    if (!found) return -1;
    struct ht_item_s *item = ht_get(found, key, nkey);
    if (!item) return 0;
    int i;
    for (i = 0; i < l->filter.ncolumns; i++) {
        struct ht_item_s *r = ht_get(l->filter.columns[i].ht, item->k, item->nk);
        if (r == item) continue;
        ht_rem(l->filter.columns[i].ht, item->k, item->nk);
    }
    ifr(del_item(l, (struct list_internal_s *)item->v));
    ht_rem(found, item->k, item->nk);
    printf("removing \n");
    return 0;
}

static int del(struct list_s *l, void *userdata)
{
    if (!l || !userdata) return -1;
    struct list_internal_s *li;
    for (li = l->head; li != NULL; li = li->next) {
        if (li->data.ptr == userdata) {
            return del_item(l, li);
        }
    }
    return -1;
}

static int del_tail(struct list_s *l)
{
    if (!l) return -1;
    struct list_internal_s *li = l->tail;
    if (!li) return -1;
    return del_item(l, li);
}

static int map(struct list_s *l, int (*cb)(struct list_s*, void*, void*),
               void *userdata)
{
    if (!l || !cb) return -1;
    struct list_internal_s *li, *op;
    for (li = l->head; li != NULL; ) {
        op = li;
        li = li->next;
        int ret = cb(l, op->data.ptr, userdata);
        if (ret == -1)     return ret;
        else if (ret == 1) return 0;
    }
    return 0;
}

static int clean(struct list_s *l)
{
    if (!l) return -1;
    ifr(clean_items(l));
    int i;
    for (i = 0; i < l->filter.ncolumns; i++)
        ht_free(l->filter.columns[i].ht);
    if (l->filter.columns) free(l->filter.columns);
    return 0;
}

static int size(struct list_s *l, int *sz)
{
    if (!l || !sz) return -1;
    *sz = l->size;
    return 0;
}

static int toarray(struct list_s *l, void ***dst, int *ndst)
{
    struct toarray_s { void **dst; int i; };
    int cb(struct list_s *l, void *uv, void *ud) {
        if (!uv || !ud) return -1;
        struct toarray_s *ta = (struct toarray_s *)ud;
        char **d = (char **)ta->dst;
        d[ta->i++] = (char *)uv;
        return 0;
    }
    *dst = malloc(sizeof(void *) * l->size);
    if (!dst) return -1;
    struct toarray_s ta = { .dst = *dst, .i = 0 };
    if (list.map(l, cb, &ta) != 0) return -1;
    *ndst = l->size;
    return 0;
}

static int sort_str(const void *a, const void *b)
{
    char **sa = (char **)a;
    char **sb = (char **)b;
    return strcmp(*sa, *sb);
}

static int toarray_sort(struct list_s *l, void ***dst, int *ndst,
                        enum list_array_sort_e las)
{
    if (!l || !dst || !ndst) return -1;
    if (toarray(l, dst, ndst) != 0) return -1;
    switch (las) {
        case LIST_ARRAY_SORT_STR:
            qsort((char **)(*dst), *ndst, sizeof(char *), sort_str);
            break;
        default:
            return -1;
    }
    return 0;
}

static int queue_add(struct list_s *l, void *userdata, int (*clean)(void *ptr))
{
    if (!l || !userdata) return -1;
    int sz;
    if (size(l, &sz) != 0) return -1;
    if (l->tail && sz >= QUEUE_SIZE) {
        if (del_tail(l) != 0) return -1;
    }
    return add_head(l, userdata, clean);
}


static int filter_find(struct list_s *l, const char *column,
                       struct ht_s **found)
{
    if (!l || !column || !found) return -1;
    *found = NULL;
    int i;
    for (i = 0; i < l->filter.ncolumns; i++) {
        if(dmemcmp(l->filter.columns[i].name, strlen(l->filter.columns[i].name),
           column, strlen(column))) {
            *found = l->filter.columns[i].ht;
            return 0;
        }
    }
    return 0;
}

static int column_map(struct list_s *l, const char *column,
                      const void *key, const int nkey,
                      const void *value, const int nvalue)
{
    if (!l || !column || !key || !value) return -1;
    struct ht_s *found;
    ifr(filter_find(l, column, &found));
    if (!found) return -1;
    HT_ADD_WA(found, key, nkey, value, nvalue);
    return 0;
}

static int column_find(struct list_s *l, const char *column,
                       void *key, const int nkey,
                       int (*cb)(struct list_s *l, void *value,
                                 const int nvalue, void **userdata),
                       void **userdata)
{
    if (!l || !column || !cb) return -1;
    struct ht_s *found;
    ifr(filter_find(l, column, &found));
    if (!found) return -1;
    struct ht_item_s *item = ht_get(found, key, nkey);
    if (!item) return 0;
    struct list_internal_s *li = (struct list_internal_s *)item->v;
    return cb(l, li->data.ptr, sizeof(li->data.ptr), userdata);
}

const struct module_list_s list = {
    .init         = init,
    .add          = add,
    .add_head     = add_head,
    .del          = del,
    .map          = map,
    .size         = size,
    .toarray      = toarray,
    .toarray_sort = toarray_sort,
    .reset        = reset,
    .clean        = clean,
    .queue_add    = queue_add,
    .column.init  = column_init,
    .column.add   = column_add,
    .column.map   = column_map,
    .column.find  = column_find,
    .column.del   = column_del,
};
