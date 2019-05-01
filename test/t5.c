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
    A(rsize, 3);
    A(root.blocks.size(r[1], &rsize), 0);
    A(rsize, 4);

    struct root_diff_s diff;
    A(root.compare(r[0], r[1], &diff), 0);
    A(diff.verdict, false);
    A(diff.winner, ROOT_REMOTE);

    json_object *b;
    A(root.blocks.export(r[1], diff.blockidx, &b), 0);

    printf("block: %s\n", json_object_to_json_string(b));

    A(root.compare(r[0], r[1], &diff), 0);
    A(diff.verdict, false);
    A(diff.winner, ROOT_REMOTE);

    A(root.compare(r[1], r[0], &diff), 0);
    A(diff.verdict, false);
    A(diff.winner, ROOT_LOCAL);

    A(root.blocks.append(r[0], b), 0);
    json_object_put(b);

    A(root.compare(r[0], r[1], &diff), 0);
    A(diff.verdict, true);

    A(root.clean(r[0]), 0);
    A(root.clean(r[1]), 0);

    config_free(cfg);
}
