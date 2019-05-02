#include <common.h>
#include <cu.h>

void t5_root_compare()
{
    struct config_s *cfg;
    A(config_init(&cfg), 0);
    struct root_s *r[2];

    struct group_s *g;
    A(group.init(&g), 0);
    A(group.db.load(g), 0);
    CU_ASSERT(g->roots.size > 1);
    r[0] = g->roots.array[0];
    r[1] = g->roots.array[1];

    size_t rsize;
    A(root.blocks.size(r[0], &rsize), 0);
    A(rsize, 4);
    A(root.blocks.size(r[1], &rsize), 0);
    A(rsize, 1);

    struct root_diff_s diff;
    A(root.compare(r[0], r[1], &diff), 0);
    A(diff.equal, false);
    A(diff.winner, ROOT_DST);

    bool merged;
    A(root.merge(r[0], r[1], &merged), 0);
    A(merged, false);
    A(root.merge(r[1], r[0], &merged), 0);
    A(merged, true);
    A(root.compare(r[0], r[1], &diff), 0);
    A(diff.equal, true);

    A(group.clean(g), 0);

    config_free(cfg);
}
