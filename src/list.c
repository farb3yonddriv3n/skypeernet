#include <common.h>

struct list_internal_s {
    struct list_internal_s *prev;
    struct list_internal_s *next;
    struct {
        void *ptr;
        int (*clean)(void *ptr);
    } data;
};

static int init(struct list_s *l)
{
    if (!l) return -1;
    memset(l, 0, sizeof(*l));
    return 0;
}

static int init_internal(struct list_internal_s **li)
{
    *li = malloc(sizeof(**li));
    if (!*li) return -1;
    memset(*li, 0, sizeof(**li));
    return 0;
}

static int add(struct list_s *l, void *userdata, int (*clean)(void *ptr))
{
    if (!l || !userdata) return -1;
    struct list_internal_s *li;
    if (init_internal(&li) != 0) return -1;
    li->data.ptr   = userdata;
    li->data.clean = clean;
    li->next       = NULL;
    li->prev       = l->tail;
    if (l->tail) {
        l->tail->next = li;
        l->tail       = li;
    } else l->head = l->tail = li;
    l->size++;
    return 0;
}

static int del(struct list_s *l, void *userdata)
{
    if (!l || !userdata) return -1;
    struct list_internal_s *li;
    for (li = l->head; li != NULL; li = li->next) {
        if (li->data.ptr == userdata) {
            if (li->prev) li->prev->next = li->next;
            else l->head = li->next;
            if (li->next) li->next->prev = li->prev;
            else l->tail = li->prev;
            l->size--;
            if (li->data.clean && li->data.clean(li->data.ptr) != 0) return -1;
            free(li);
            return 0;
        }
    }
    return -1;
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
    struct list_internal_s *li, *d;
    for (li = l->head; li != NULL; ) {
        if (li->data.clean(li->data.ptr) != 0) return -1;
        d = li;
        li = li->next;
        free(d);
    }
    if (init(l) != 0) return -1;
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
    ifr(list.map(l, cb, &ta));
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
    ifr(toarray(l, dst, ndst));
    switch (las) {
        case LIST_ARRAY_SORT_STR:
            qsort((char **)(*dst), *ndst, sizeof(char *), sort_str);
            break;
        default:
            return -1;
    }
    return 0;
}

const struct module_list_s list = {
    .init         = init,
    .add          = add,
    .del          = del,
    .map          = map,
    .size         = size,
    .toarray      = toarray,
    .toarray_sort = toarray_sort,
    .clean        = clean,
};
