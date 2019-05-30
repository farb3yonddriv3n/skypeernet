#include <common.h>
#include <cu.h>

void t5_root_compare()
{
    struct config_s cfg;
    A(config_init(&cfg), 0);
    struct root_s *r[2];

    struct group_s *g;
    A(group.init(&g), 0);
    A(group.db.load(g), 0);
    CU_ASSERT(g->roots.size > 1);
    r[0] = g->roots.array[0];
    r[1] = g->roots.array[1];

    struct root_diff_s diff;
    A(root.compare(r[0], r[1], &diff), 0);
    A(diff.equal, false);
    //A(diff.winner, ROOT_DST);

    bool merged;
    A(root.merge(r[0], r[1], &merged), 0);
    A(merged, true);

    A(group.clean(g), 0);

    config_free(&cfg);
}
