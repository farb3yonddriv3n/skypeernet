#include <common.h>
#include <cu.h>

void t1_root_import_export()
{
    struct config_s *cfg;
    A(config_init(&cfg), 0);
    struct root_s r;
    A(root.data.import(&r, FROOT_VALID), 0);
    A(root.data.export(&r), 0);
    A(root.data.import(&r, FROOT_INVALID), -1);
}
