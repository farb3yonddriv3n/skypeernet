#include <common.h>
#include <cu.h>

void t5_root_compare()
{
    struct config_s *cfg;
    A(config_init(&cfg), 0);
    struct root_s r[2];

    A(root.data.import(&r[0], FROOT_VALID), 0);
    A(root.data.import(&r[1], FROOT_VALID), 0);

    struct block_equal_s equal;
    A(root.compare(&r[0], &r[1], &equal), 0);

    A(equal.verdict, true);
}
