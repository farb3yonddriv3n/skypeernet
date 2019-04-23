#include <common.h>

static int init(struct root_s *r)
{
    memset(r, 0, sizeof(*r));
    return 0;
}

static int add(struct root_s *r, struct block_s *b)
{
    r->blocks.size++;
    r->blocks.array = realloc(r->blocks.array,
                              sizeof(void *) * r->blocks.size);
    if (!r->blocks.array) return -1;
    r->blocks.array[r->blocks.size - 1] = b;
    return 0;
}

static int import(struct root_s *r, const char *filename)
{
    sn_initz(fn, (char *)filename);
    char *content;
    int n = eioie_fread(&content, fn);
    if (n <= 1) return -1;

    struct json_tokener *tok = json_tokener_new();
    struct json_object *obj = json_tokener_parse_ex(tok, content, n);
    json_tokener_free(tok);

    json_object *blocks;
    json_object_object_get_ex(obj, "blocks", &blocks);
    if (json_object_get_type(blocks) == json_type_array) {

        root.init(r);

        array_list *blocks_array = json_object_get_array(blocks);
        int i;
        for (i = 0; i < array_list_length(blocks_array); i++) {
            json_object *block_item = array_list_get_idx(blocks_array, i);
            struct block_s *b;
            block.data.import(&b, block_item);
            root.add(r, b);
        }
    }

    return 0;
}

static int export(struct root_s *r)
{
    json_object *rt = json_object_new_object();
    json_object *blocks = json_object_new_array();
    json_object_object_add(rt, "blocks", blocks);

    int i;
    for (i = 0; i < r->blocks.size; i++) {
        json_object *bobj;
        int ret = block.data.export(r->blocks.array[i], &bobj);
        if (ret != 0) return ret;
        json_object_array_add(blocks, bobj);
    }

    printf("json: %s\n", json_object_to_json_string(rt));

    return 0;
}

const struct module_root_s root = {
    .init        = init,
    .add         = add,
    .data.import = import,
    .data.export = export,
};
