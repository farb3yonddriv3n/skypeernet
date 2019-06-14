#include <common.h>

static int mallocz(struct root_s **r)
{
    *r = malloc(sizeof(**r));
    if (!(*r)) return -1;
    memset(*r, 0, sizeof(**r));
    return 0;
}

static int init(struct root_s **r)
{
    if (mallocz(r) != 0) return -1;
    memcpy((*r)->hash, DISTFS_BASE_ROOT_HASH, sizeof((*r)->hash));
    return 0;
}

static int blocks_add(struct root_s *r, struct block_s *b)
{
    if (!r || !b) return -1;
    r->blocks.size++;
    r->blocks.array = realloc(r->blocks.array,
                              sizeof(void *) * r->blocks.size);
    if (!r->blocks.array) return -1;
    r->blocks.array[r->blocks.size - 1] = b;
    memcpy(r->hash, b->hash.pow, sizeof(b->hash.pow));
    return 0;
}

static int blocks_append(struct root_s *r, const json_object *blockobj)
{
    struct block_s *b;
    if (block.data.load(&b, blockobj) != 0) return -1;
    return root.blocks.add(r, b);
}

static int load_object(struct root_s **r, const json_object *obj)
{
    if (!r || !obj) return -1;

    if (root.init(r) != 0) return -1;
    json_object *blocks;
    json_object_object_get_ex(obj, "blocks", &blocks);
    if (json_object_get_type(blocks) == json_type_array) {
        array_list *blocks_array = json_object_get_array(blocks);
        int i;
        for (i = 0; i < array_list_length(blocks_array); i++) {
            json_object *block_item = array_list_get_idx(blocks_array, i);
            if (root.blocks.append(*r, block_item) != 0) return -1;
        }
    }
    json_object_put((json_object *)obj);
    bool valid = true;
    if (root.validate(*r, &valid) != 0) return -1;
    if (!valid) {
        if (root.clean(*r) != 0) return -1;
        return -1;
    }
    return 0;
}

static int load_json_file(struct root_s **r, const char *filename)
{
    json_object *obj;
    if (os.loadjsonfile(&obj, filename) != 0) return -1;
    return root.data.load.object(r, obj);
}

static int load_json(struct root_s **r, char *content, int ncontent)
{
    json_object *obj;
    if (os.loadjson(&obj, content, ncontent) != 0) return -1;
    return root.data.load.object(r, obj);
}

static int blocks_size(struct root_s *r, size_t *size)
{
    if (!r || !size) return -1;
    *size = r->blocks.size;
    return 0;
}

static int compare(struct root_s *local, struct root_s *remote,
                   struct root_diff_s *diff)
{
    if (!local || !remote || !diff) return -1;

    struct { size_t local; size_t remote; } size; 
    if (root.blocks.size(local,  &size.local)  != 0) return -1;
    if (root.blocks.size(remote, &size.remote) != 0) return -1;

    bool valid;
    if (root.validate(local, &valid) != 0) return -1;
    if (valid == false) return -1;
    if (root.validate(remote, &valid) != 0) return -1;
    if (valid == false) return -1;

    diff->winner = ROOT_NONE;
    for (diff->blockidx = 0, diff->equal = true;
        ((diff->blockidx < size.local && diff->blockidx < size.remote) && diff->equal == true);
        diff->blockidx++) {
        if (block.compare(local->blocks.array[diff->blockidx],
                          remote->blocks.array[diff->blockidx],
                          diff) != 0) return -1;
    }

    if (diff->equal == true && size.local != size.remote) {
        diff->equal = false;
        diff->winner  = (size.local > size.remote) ?
                        ROOT_DST : ROOT_SRC;
    }

    return 0;
}

static int canmerge(struct root_s *dst, struct root_s *src,
                    struct root_diff_s *diff)
{
    if (!dst || !src || !diff) return -1;
    if (root.compare(dst, src, diff) != 0) return -1;
    diff->canmerge = MERGE_NOT_NEEDED;
    if (diff->equal == true) return 0;
    size_t sdst, ssrc;
    if (root.blocks.size(src, &ssrc) != 0) return -1;
    if (root.blocks.size(dst, &sdst) != 0) return -1;
    if (diff->winner == ROOT_SRC && diff->blockidx == sdst) {
        diff->src.ptr  = src;
        diff->src.size = ssrc;
        diff->dst.ptr  = dst;
        diff->dst.size = sdst;
        diff->canmerge = MERGE_POSSIBLE;
    } else if (diff->winner == ROOT_DST && diff->blockidx == ssrc) {
        diff->src.ptr  = dst;
        diff->src.size = sdst;
        diff->dst.ptr  = src;
        diff->dst.size = ssrc;
        diff->canmerge = MERGE_POSSIBLE;
    } else diff->canmerge = MERGE_IMPOSSIBLE;
    return 0;
}

static int merge(struct root_s *dst, struct root_s *src,
                 bool *merged)
{
    *merged = false;
    struct root_diff_s diff;
    if (root.canmerge(dst, src, &diff) != 0) return -1;
    if (diff.canmerge == MERGE_NOT_NEEDED) {
        *merged = true;
    } else if (diff.canmerge == MERGE_IMPOSSIBLE) {
        return 0;
    } else {
        int i;
        for (i = diff.blockidx; i < diff.src.size; i++) {
            json_object *b;
            if (root.blocks.export(diff.src.ptr, i, &b) != 0) return -1;
            if (root.blocks.append(diff.dst.ptr, b) != 0) return -1;
            json_object_put(b);
        }
    }
    if (root.compare(dst, src, &diff) != 0) return -1;
    *merged = diff.equal;
    if (*merged != true) return -1;
    return 0;
}

static int save_object(const struct root_s *r, json_object **robj)
{
    if (!r || !robj) return -1;
    *robj = json_object_new_object();
    json_object *blocks = json_object_new_array();
    json_object_object_add(*robj, "blocks", blocks);
    int i;
    for (i = 0; i < r->blocks.size; i++) {
        json_object *bobj;
        int ret = block.data.save(r->blocks.array[i], &bobj);
        if (ret != 0) return ret;
        json_object_array_add(blocks, bobj);
    }
    return 0;
}

static int save_json_file(const struct root_s *r, const char *filename)
{
    json_object *obj;
    if (root.data.save.object(r, &obj) != 0) return -1;
    const char *json = json_object_to_json_string(obj);
    if (!json) return -1;
    if (eioie_fwrite(filename, "w", (char *)json, strlen(json)) != 0)
        return -1;
    json_object_put(obj);
    return 0;
}

static int blocks_export(const struct root_s *r, const uint64_t blockidx,
                         json_object **blockobj)
{
    if (!r) return -1;
    if (blockidx > r->blocks.size) return -1;
    if (block.data.save(r->blocks.array[blockidx], blockobj) != 0) return -1;
    return 0;
}

static int copy(struct root_s **dst, const struct root_s *src)
{
    if (!src) return -1;
    json_object *obj;
    if (root.data.save.object(src, &obj) != 0) return -1;
    if (root.data.load.object(dst, obj) != 0) return -1;
    return 0;
}

static int validate(const struct root_s *r, bool *valid)
{
    if (!r || !valid) return -1;
    unsigned char *block_prev = (unsigned char *)DISTFS_BASE_ROOT_HASH;
    *valid = true;
    int i;
    for (i = 0; i < r->blocks.size; i++) {
        struct block_s *b = r->blocks.array[i];
        if (memcmp(block_prev, b->hash.prev, sizeof(b->hash.prev)) != 0) {
            *valid = false;
            return 0;
        }
        if (block.validate(b, valid) != 0) return -1;
        if (*valid != true) return 0;
        block_prev = b->hash.pow;
    }
    return 0;
}

static int dump(struct root_s *r, struct config_s *cfg)
{
    if (!r || !cfg) return -1;
    printf("Peer: %x:%d\n", r->net.host, r->net.port);
    int i;
    for (i = 0; i < r->blocks.size; i++) {
        struct block_s *b = r->blocks.array[i];
        if (block.dump(b) != 0) return -1;
    }
    return 0;
}

static int find(struct root_s *r, unsigned char *h, void **found,
                int *host, unsigned short *port)
{
    if (!r || !h || !found) return -1;
    int i;
    for (i = 0; i < r->blocks.size; i++) {
        if (block.find(r->blocks.array[i], h, found) != 0) return -1;
        if (*found && r->net.host != *host && r->net.port != *port) {
            *host       = r->net.host;
            *port       = r->net.port;
            break;
        } else *found = NULL;
    }
    return 0;
}

static int clean(struct root_s *r)
{
    if (!r) return -1;
    int i;
    for (i = 0; i < r->blocks.size; i++) {
        struct block_s *b = r->blocks.array[i];
        if (block.clean(b) != 0) return -1;
    }
    free(r->blocks.array);
    free(r);
    return 0;
}

static int net_set(struct root_s *r, int host, unsigned short port)
{
    if (!r) return -1;
    r->net.host = host;
    r->net.port = port;
    return 0;
}

const struct module_root_s root = {
    .init             = init,
    .compare          = compare,
    .copy             = copy,
    .validate         = validate,
    .canmerge         = canmerge,
    .merge            = merge,
    .find             = find,
    .dump             = dump,
    .clean            = clean,
    .blocks.add       = blocks_add,
    .blocks.append    = blocks_append,
    .blocks.size      = blocks_size,
    .blocks.export    = blocks_export,
    //.blocks.import    = blocks_import,
    .data.load.json   = load_json,
    .data.load.file   = load_json_file,
    .data.load.object = load_object,
    .data.save.file   = save_json_file,
    .data.save.object = save_object,
    .net.set          = net_set,
};
