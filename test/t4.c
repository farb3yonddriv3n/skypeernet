#include <common.h>
#include <cu.h>

struct t4_s {
    int x;
    int y;
};

static int clean(void *data)
{
    if (!data) return -1;
    free(data);
    return 0;
}

static int map_columns(struct list_s *l, void *li, void *userdata)
{
    if (!l || !userdata) return -1;
    struct t4_s *item = (struct t4_s *)userdata;
    ifr(LISTADD(l, "x", &item->x, sizeof(item->x)));
    ifr(LISTADD(l, "y", &item->y, sizeof(item->y)));
    return 0;
}

static int findcb(struct list_s *l, void *value,
                  const int nvalue, void **userdata)
{
    struct t4_s **found = (struct t4_s **)userdata;
    if (nvalue != sizeof(void *)) return -1;
    *found = (struct t4_s *)value;
    return 0;
}

#define ITEMS 10

void t4_list()
{
    struct list_s l;
    struct list_param_s params[] = { { .name = "x" },
                                     { .name = "y" } };
    A(list.column.init(&l, params, COUNTOF(params)), 0);
    struct t4_s *t4[ITEMS];

    int i;
    for (i = 0; i < ITEMS; i++) {
        struct t4_s *t = malloc(sizeof(*t));
        t->x = i;
        t->y = i + 1;
        t4[i] = t;
        A(list.column.add(&l, t, map_columns, clean), 0);
    }

    for (i = 0; i < ITEMS; i++) {
        struct t4_s *found = NULL;
        A(list.column.find(&l, "x", &i, sizeof(i),
                           findcb, (void **)&found), 0);
        CU_ASSERT(t4[i] == found);
        found = NULL;
        int find = i + 1;
        A(list.column.find(&l, "y", &find, sizeof(find),
                           findcb, (void **)&found), 0);
        CU_ASSERT(t4[i] == found);
    }

    for (i = 0; i < COUNTOF(t4); i++) {
        A(list.column.del(&l, "x", &t4[i]->x, sizeof(t4[i]->x)), 0);
    }

    int size;
    void **vt;
    A(list.toarray(&l, &vt, &size), 0);
    for (i = 0; i < size; i++) {
        printf("%d\n", ((struct t4_s *)(((char **)vt)[i]))->x);
    }
    free(vt);

    A(list.reset(&l), 0);
    for (i = 0; i < 200; i++) {
        int *n = malloc(sizeof(*n));
        *n = i;
        A(list.queue_add(&l, n, clean), 0);
    }
    A(list.size(&l, &size), 0);
    A(size, 100);
    A(list.clean(&l), 0);
}
