#include <common.h>

static int add(struct list_s *jobs, struct group_s *remote,
               unsigned char *file, int nfile, bool *found,
               bool *added)
{
    if (!jobs || !remote || !file || !found) return -1;
    if (nfile != SHA256HEX) return -1;
    *found = *added = false;
    struct job_find_s { unsigned char *file; struct job_s *found; };
    int find_job(struct list_s *l, void *uj, void *ud) {
        struct job_s      *j  = (struct job_s *)uj;
        struct job_find_s *jf = (struct job_find_s *)ud;
        if (memcmp(j->file, jf->file, sizeof(j->file)) == 0) {
            jf->found = j;
            return 1;
        }
        return 0;
    }
    struct job_find_s jf = { .file = file, .found = NULL };
    ifr(list.map(jobs, find_job, &jf));
    if (jf.found) {
        *added = true;
        return 0;
    }
    struct file_s *f = NULL;
    ifr(group.find(remote, file, (void **)&f));
    if (!f) return 0;

    struct job_s *j = malloc(sizeof(*j));
    if (!j) return -1;
    memset(j, 0, sizeof(*j));
    int i;
    for (i = 0; i < f->chunks.size; i++) {
        j->chunks.array = realloc(j->chunks.array, ++j->chunks.size * sizeof(*j));
        if (!j->chunks.array) return -1;
        memcpy(j->chunks.array[j->chunks.size - 1].chunk, f->chunks.array[i].hash.content,
               sizeof(f->chunks.array[i].hash.content));
        j->chunks.array[j->chunks.size - 1].size  = f->chunks.array[i].size;
        j->chunks.array[j->chunks.size - 1].state = JOBCHUNK_NONE;
    }
    *added = true;
    return 0;
}

static int export()
{
    return 0;
}

const struct module_job_s job = {
    .add = add,
};
