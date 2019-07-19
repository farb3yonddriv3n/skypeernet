#include <common.h>

static int dump(json_object **obj)
{
    if (!obj) return -1;
    *obj = json_object_new_object();
    json_object *version = json_object_new_double(SPN_VERSION);
    json_object_object_add(*obj, "version", version);
    return 0;
}

const struct module_version_s version = {
    .dump = dump,
};
