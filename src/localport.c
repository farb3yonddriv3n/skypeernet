#include <common.h>

#define LOCAL_PORT_FILE ".local_port"

static int data_load(struct config_s *cfg)
{
    json_object *obj;
    bool exists;
    ifr(os.fileexists(LOCAL_PORT_FILE, &exists));
    if (!exists) return 0;
    ifr(os.loadjsonfile(&obj, LOCAL_PORT_FILE));
    json_object *tmp;
    BIND_INT(cfg->net.local_port, "local_port", tmp, obj);
    json_object_put(obj);
    return 0;
}

static int data_save(struct config_s *cfg)
{
    if (!cfg) return -1;
    json_object *r = json_object_new_object();
    json_object *localport = json_object_new_int(cfg->net.local_port);
    json_object_object_add(r, "local_port", localport);
    const char *json = json_object_to_json_string(r);
    ifr(eioie_fwrite(LOCAL_PORT_FILE, "w", (char *)json, strlen(json)));
    json_object_put(r);
    return 0;
}

static int load(struct config_s *cfg, unsigned short *port)
{
    if (!cfg || !port) return -1;
    ifr(data_load(cfg));
    *port = cfg->net.local_port;
    return 0;
}

static int save(struct config_s *cfg, unsigned short port)
{
    if (!cfg || !port) return -1;
    cfg->net.local_port = port;
    return data_save(cfg);
}

const struct module_localport_s localport = {
    .load = load,
    .save = save,
};
