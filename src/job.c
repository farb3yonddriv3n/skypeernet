#include <common.h>

static int add(struct group_s *remote,
               unsigned char *file, void **found)
{
    if (!remote || !file || !found) return -1;
    ifr(group.find(remote, file, found));
    return 0;
}

const struct module_job_s job = {
    .add = add,
};
