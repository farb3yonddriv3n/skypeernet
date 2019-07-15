#include <common.h>

struct ip_find_s {
    int             host;
    unsigned short  port;
    enum rogue_e    reason;
    struct rogue_s *found;
};

static int clean(void *ur)
{
    if (!ur) return -1;
    free(ur);
    return 0;
}

static int find(struct list_s *l, void *ur, void *ud)
{
    struct rogue_s   *r  = (struct rogue_s *)ur;
    struct ip_find_s *rf = (struct ip_find_s *)ud;
    if (r->host == rf->host && r->port == rf->port &&
        r->reason == rf->reason) {
        rf->found = r;
        return 1;
    }
    return 0;
}

static int add(struct list_s *l, int host, unsigned short port,
               enum rogue_e reason)
{
    if (!l) return -1;
    struct ip_find_s ipf = { .host = host, .port = port,
                             .reason = reason, .found = NULL };
    ifr(list.map(l, find, &ipf));
    if (ipf.found) {
        ipf.found->count++;
        return 0;
    }
    struct rogue_s *rip = malloc(sizeof(*rip));
    if (!rip) return -1;
    rip->host   = host;
    rip->port   = port;
    rip->reason = reason;
    rip->count  = 1;
    return list.add(l, rip, clean);
}

static int dump(struct list_s *l, json_object **obj)
{
    if (!l || !obj) return -1;
    int cb(struct list_s *l, void *ur, void *ud) {
        struct rogue_s *r    = (struct rogue_s *)ur;
        json_object *rogues = (json_object *)ud;
        json_object *jr     = json_object_new_object();
        char hexhost[32];
        snprintf(hexhost, sizeof(hexhost), "%x", r->host);
        json_object *host   = json_object_new_string((const char *)hexhost);
        json_object *port   = json_object_new_int(r->port);
        json_object *reason = json_object_new_int(r->reason);
        json_object *count  = json_object_new_int(r->count);
        json_object_object_add(jr, "host", host);
        json_object_object_add(jr, "port", port);
        json_object_object_add(jr, "reason", reason);
        json_object_object_add(jr, "count", count);
        json_object_array_add(rogues, jr);
        return 0;
    }
    *obj = json_object_new_object();
    json_object *jrogues = json_object_new_array();
    json_object_object_add(*obj, "rogues", jrogues);
    return list.map(l, cb, jrogues);
}

const struct module_rogue_s rogue = {
    .add  = add,
    .dump = dump,
};
