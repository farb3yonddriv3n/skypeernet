#include <common.h>

#define WHITELIST_FILE ".whitelist"

struct wl_item_s {
    char *pubhash;
    char *found;
};

static int data_save(struct peer_s *p);

static int clean(void *data)
{
    if (data) free(data);
    return 0;
}

static int add(struct peer_s *p, char *ph)
{
    if (!p || !ph) return -1;
    char *k = malloc(SHA256HEX);
    if (!k) return -1;
    memcpy(k, ph, SHA256HEX);
    ifr(list.add(&p->whitelist, k, clean));
    return data_save(p);
}

static int rem(struct peer_s *p, char *ph)
{
    if (!p || !ph) return -1;
    ifr(list.del(&p->whitelist, ph));
    return data_save(p);
}

static int data_load(struct peer_s *p)
{
    json_object *obj;
    bool exists;
    ifr(os.fileexists(WHITELIST_FILE, &exists));
    if (!exists) return 0;
    ifr(os.loadjsonfile(&obj, WHITELIST_FILE));
    json_object *keys;
    json_object_object_get_ex(obj, "keys", &keys);
    ifr(list.init(&p->whitelist));
    if (json_object_get_type(keys) == json_type_array) {
        array_list *keys_array = json_object_get_array(keys);
        int i;
        for (i = 0; i < array_list_length(keys_array); i++) {
            json_object *kitem = array_list_get_idx(keys_array, i);
            int len = json_object_get_string_len(kitem);
            if (len != SHA256HEX) return -1;
            ifr(add(p, (char *)json_object_get_string(kitem)));
       }
    }
    json_object_put(obj);
    return 0;
}

static int export(struct peer_s *p, json_object **obj)
{
    if (!p || !obj) return -1;
    int cb(struct list_s *l, void *ke, void *ud) {
        char *k                  = (char *)ke;
        struct json_object *keys = (struct json_object *)ud;

        json_object *key = json_object_new_string_len(k, SHA256HEX);
        json_object_array_add(keys, key);
        return 0;
    }
    *obj = json_object_new_object();
    json_object *keys = json_object_new_array();
    json_object_object_add(*obj, "keys", keys);
    return list.map(&p->whitelist, cb, keys);
}

static int data_save(struct peer_s *p)
{
    if (!p) return -1;
    json_object *r;
    ifr(export(p, &r));
    const char *json = json_object_to_json_string(r);
    ifr(eioie_fwrite(WHITELIST_FILE, "w", (char *)json, strlen(json)));
    json_object_put(r);
    return 0;
}

static int find(struct list_s *l, void *ex, void *ud)
{
    if (!l || !ex) return -1;
    struct wl_item_s *wlf = (struct wl_item_s *)ud;
    if (memcmp(ex, wlf->pubhash, SHA256HEX) == 0) {
        wlf->found = ex;
        return 1;
    }
    return 0;
}

static int wl_addrem(struct peer_s *p, const char *pubhash,
                     enum wl_e action)
{
    if (!p || !pubhash) return -1;
    if (strlen(pubhash) != SHA256HEX) return -1;
    struct wl_item_s wlf = {
        .pubhash = (char *)pubhash,
        .found = NULL,
    };
    ifr(list.map(&p->whitelist, find, &wlf));
    if (!wlf.found && action == WL_ADD)
        return add(p, (char *)pubhash);
    else if (wlf.found && action == WL_REM)
        return rem(p, wlf.found);
    return -1;
}

static int wl_exists(struct peer_s *p, const char *pubhash, bool *exists)
{
    if (!p || !pubhash) return -1;
    if (strlen(pubhash) != SHA256HEX) return -1;
    struct wl_item_s wlf = {
        .pubhash = (char *)pubhash,
        .found = NULL,
    };
    ifr(list.map(&p->whitelist, find, &wlf));
    *exists = wlf.found ? true : false;
    return 0;
}

static int wl_list(struct peer_s *p)
{
    if (!p) return -1;
    json_object *r;
    ifr(export(p, &r));
    const char *json = json_object_to_json_string_ext(r, JSON_C_TO_STRING_PRETTY);
    printf("%s\n", json);
    json_object_put(r);
    return 0;
}

const struct module_whitelist_s whitelist = {
    .addrem = wl_addrem,
    .list   = wl_list,
    .exists = wl_exists,
    .load   = data_load,
};
