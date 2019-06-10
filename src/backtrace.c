#include <common.h>

struct backtrace_s {
    char filename[256];
    int  line;
};

static struct list_s backtrace_list;

static int init()
{
    return list.init(&backtrace_list);
}

static int clean()
{
    return list.clean(&backtrace_list);
}

static int clean_item(void *ub)
{
    if (!ub) return -1;
    free(ub);
    return 0;
}

static void add(const char *filename, int line)
{
    struct backtrace_s *b;
    b = malloc(sizeof(*b));
    if (!b) abort();
    snprintf(b->filename, sizeof(b->filename), "%s", filename);
    b->line = line;
    if (list.add(&backtrace_list, b, clean_item) != 0) abort();
}

static void show()
{
    int cb(struct list_s *l, void *ub, void *ud) {
        struct backtrace_s *b = (struct backtrace_s *)ub;
        int *frame            = (int *)ud;
        printf("#%d %s:%d\n", (*frame)++, b->filename, b->line);
        return 0;
    }
    int frame = 0;
    printf("Backtrace:\n");
    if (list.map(&backtrace_list, cb, &frame) != 0) abort();
}

const struct module_backtrace_s backtrace = {
    .init  = init,
    .add   = add,
    .show  = show,
    .clean = clean,
};
