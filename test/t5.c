#include <common.h>
#include <cu.h>

void t5_root_compare()
{
    struct config_s *cfg;
    A(config_init(&cfg), 0);
    struct root_s r[2];

    A(root.data.load.file(&r[0], FROOT_VALID),  0);
    A(root.data.load.file(&r[1], FROOT_VALID2), 0);

    struct root_diff_s diff;
    A(root.compare(&r[0], &r[1], &diff), 0);
    A(diff.verdict, false);
    A(diff.winner, ROOT_REMOTE);

    json_object *b;
    A(root.blocks.export(&r[1], diff.blockidx, &b), 0);

    printf("block: %s\n", json_object_to_json_string(b));

    A(root.compare(&r[0], &r[1], &diff), 0);
    A(diff.verdict, false);
    A(diff.winner, ROOT_REMOTE);

    A(root.compare(&r[1], &r[0], &diff), 0);
    A(diff.verdict, false);
    A(diff.winner, ROOT_LOCAL);

    A(root.blocks.append(&r[0], b), 0);

    A(root.compare(&r[0], &r[1], &diff), 0);
    A(diff.verdict, true);
}
