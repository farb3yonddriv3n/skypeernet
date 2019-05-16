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
            if (li->data.clean && li->data.clean(li->data.ptr) != 0) return -1;
            free(li);
            l->size--;
            return 0;
        }
    }
    return -1;
}

static int map(struct list_s *l, int (*cb)(struct list_s*, void*, void*),
               void *userdata)
{
    if (!l) return -1;
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

const struct module_list_s list = {
    .init  = init,
    .add   = add,
    .del   = del,
    .map   = map,
    .clean = clean,
};
