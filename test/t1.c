#include <common.h>
#include <cu.h>

void t1_root_load_save()
{
    struct config_s *cfg;
    A(config_init(&cfg), 0);
    struct root_s r[3];
    A(root.data.load.file(&r[0], FROOT_VALID), 0);
    json_object *obj;
    A(root.data.save(&r[0], &obj), 0);
    A(root.data.load.object(&r[1], obj), 0);

    struct root_diff_s diff;
    A(root.compare(&r[0], &r[1], &diff), 0);
    A(diff.verdict, true);

    A(root.data.load.file(&r[2], FROOT_INVALID), -1);
}
