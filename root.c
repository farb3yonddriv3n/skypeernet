#include <common.h>

static const char *default_hash = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";

static int init(struct root_s *r)
{
    if (!r) return -1;
    memset(r, 0, sizeof(*r));
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
    return 0;
}

static int blocks_append(struct root_s *r, const json_object *blockobj)
{
    struct block_s *b;
    if (block.data.load(&b, blockobj) != 0) return -1;
    return root.blocks.add(r, b);
}

static int load_object(struct root_s *r, const json_object *obj)
{
    if (!r || !obj) return -1;

    json_object *blocks;
    json_object_object_get_ex(obj, "blocks", &blocks);
    if (json_object_get_type(blocks) == json_type_array) {
        root.init(r);
        array_list *blocks_array = json_object_get_array(blocks);
        int i;
        for (i = 0; i < array_list_length(blocks_array); i++) {
            json_object *block_item = array_list_get_idx(blocks_array, i);
            if (root.blocks.append(r, block_item) != 0) return -1;
        }
    }
    return 0;
}

static int load_file(struct root_s *r, const char *filename)
{
    if (!r || !filename) return -1;
    sn_initz(fn, (char *)filename);
    char *content;
    int n = eioie_fread(&content, fn);
    if (n <= 1) return -1;

    struct json_tokener *tok = json_tokener_new();
    struct json_object *obj = json_tokener_parse_ex(tok, content, n);
    json_tokener_free(tok);
    if (!obj) return -1;

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
    if (!local || !remote) return -1;

    struct { size_t local; size_t remote; } size; 
    if (root.blocks.size(local,  &size.local)  != 0) return -1;
    if (root.blocks.size(remote, &size.remote) != 0) return -1;

    bool valid;
    if (root.validate(local, &valid) != 0) return -1;
    if (valid == false) return -1;
    if (root.validate(remote, &valid) != 0) return -1;
    if (valid == false) return -1;

    for (diff->blockidx = 0, diff->verdict = true;
        ((diff->blockidx < size.local && diff->blockidx < size.remote) && diff->verdict == true);
        diff->blockidx++) {
        if (block.compare(local->blocks.array[diff->blockidx],
                          remote->blocks.array[diff->blockidx],
                          diff) != 0) return -1;
    }

    printf("sizes: %ld %ld %ld\n", size.local, size.remote, diff->blockidx);
    if (diff->verdict == true && size.local != size.remote) {
        diff->verdict = false;
        diff->winner  = (size.local > size.remote) ?
                        ROOT_LOCAL : ROOT_REMOTE;

    }

    return 0;
}

static int save(const struct root_s *r, json_object **robj)
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

    printf("json: %s\n", json_object_to_json_string(*robj));

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

static int copy(struct root_s *dst, const struct root_s *src)
{
    json_object *obj;
    if (root.data.save(src, &obj) != 0) return -1;
    if (root.data.load.object(dst, obj) != 0) return -1;
    return 0;
}

static int validate(const struct root_s *r, bool *valid)
{
    unsigned char *block_prev = (unsigned char *)default_hash;
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
        block_prev = b->hash.current;
    }

    return 0;
}

const struct module_root_s root = {
    .init             = init,
    .compare          = compare,
    //.cut              = cut,
    .copy             = copy,
    .validate         = validate,
    .blocks.add       = blocks_add,
    .blocks.append    = blocks_append,
    .blocks.size      = blocks_size,
    .blocks.export    = blocks_export,
    //.blocks.import    = blocks_import,
    .data.load.file   = load_file,
    .data.load.object = load_object,
    .data.save        = save,
};
