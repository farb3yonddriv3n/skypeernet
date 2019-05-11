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

static int cb(struct list_s *l, void *data)
{
    assert(data != NULL);
    struct t7_s *t = (struct t7_s *)data;
    printf("%d\n", t->x);
    return 0;
}

void t7_list()
{
    struct list_s *l;
    A(list.init(&l), 0);
    struct t7_s *t7[3];

    int i;
    for (i = 0; i < 10; i++) {
        struct t7_s *t = malloc(sizeof(*t));
        t->x = i;
        if (i == 0) t7[0] = t;
        if (i == 4) t7[1] = t;
        if (i == 9) t7[2] = t;
        A(list.add(l, t, clean), 0);
    }

    for (i = 0; i < COUNTOF(t7); i++) {
        A(list.del(l, t7[i]), 0);
    }
    A(list.map(l, cb), 0);
    A(list.clean(&l), 0);
}
