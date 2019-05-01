#include <common.h>
#include <cu.h>

void t2_group_root_load_save()
{
    struct config_s *cfg;
    A(config_init(&cfg), 0);

    struct group_s *g;
    A(group.init(&g), 0);
    A(group.db.load(g), 0);
    CU_ASSERT(g->roots.size > 0);

    struct root_s *r[3];
    json_object *obj;
    r[0] = g->roots.array[0];
    A(root.data.save(r[0], &obj), 0);
    A(root.data.load.object(&r[1], obj), 0);

    struct root_diff_s diff;
    A(root.compare(r[0], r[1], &diff), 0);
    A(diff.verdict, true);

    A(root.data.load.file(&r[2], F64BIN), -1);

    A(root.clean(r[1]), 0);
    A(group.clean(g), 0);

    config_free(cfg);
}
