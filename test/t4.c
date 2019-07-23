#include <common.h>
#include <cu.h>

struct t7_s {
    int x;
};

static int clean(void *data)
{
    if (!data) return -1;
    free(data);
    return 0;
}

static int cb(struct list_s *l, void *data, void *userdata)
{
    assert(data != NULL);
    struct t7_s *t = (struct t7_s *)data;
    printf("%d\n", t->x);
    A(list.del(l, data), 0);
    return 0;
}

#define ITEMS 10

void t7_list()
{
    struct list_s l;
    A(list.init(&l), 0);
    struct t7_s *t7[ITEMS];

    int i;
    for (i = 0; i < ITEMS; i++) {
        struct t7_s *t = malloc(sizeof(*t));
        t->x = i;
        t7[i] = t;
        A(list.add(&l, t, clean), 0);
    }

    for (i = 0; i < COUNTOF(t7); i++) {
        if (i == 2 || i == 3) A(list.del(&l, t7[i]), 0);
    }

    void **vt;
    int size;
    A(list.toarray(&l, &vt, &size), 0);
    printf("size: %d\n", size);
    for (i = 0; i < size; i++) {
        printf("%d\n", ((struct t7_s *)(((char **)vt)[i]))->x);
    }
    free(vt);
    A(list.map(&l, cb, NULL), 0);
    A(list.clean(&l), 0);
}
