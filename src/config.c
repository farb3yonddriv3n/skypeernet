#include <common.h>

int config_keyexists(struct config_s *cfg, unsigned char *sharedname,
                     struct config_key_s **exists)
{
    if (!cfg || !sharedname || !exists) return -1;
    *exists = NULL;
    int i;
    for (i = 0; i < cfg->keys.shared.size; i++) {
        if (memcmp(cfg->keys.shared.array[i].sharedname, sharedname, SHA256HEX) == 0) {
            *exists = &cfg->keys.shared.array[i];
            return 0;
        }
    }
    if (dmemcmp(sharedname, SHA256HEX,
                cfg->keys.local.hash.public,
                sizeof(cfg->keys.local.hash.public)))
        *exists = &cfg->keys.local;
    return 0;
}

int config_keysdump(struct config_s *cfg)
{
    int i;
    printf(" | My key |\n");
    printf(" | %.*s |\n", SHA256HEX, cfg->keys.local.hash.public);
    printf(" | Shared keys |\n");
    for (i = 0; i < cfg->keys.shared.size; i++) {
        printf(" | %.*s |\n", SHA256HEX, cfg->keys.shared.array[i].sharedname);
    }
    return 0;
}

static int port_clean(void *ptr)
{
    if (!ptr) return -1;
    free(ptr);
    return 0;
}

static int send_table(struct config_s *cfg)
{
    if (!cfg) return -1;
    if (cfg->net.max.send_retry < 2 || cfg->net.sendfactor < 2) return -1;
    cfg->net.sendtable = malloc(sizeof(bool) * cfg->net.max.send_retry);
    if (!cfg->net.sendtable) return -1;
    int i, step;
    cfg->net.sendtable[0] = true;
    for (i = 1, step = cfg->net.sendfactor; i < cfg->net.max.send_retry; i++) {
        if (i == step) {
            cfg->net.sendtable[i] = true;
            step *= cfg->net.sendfactor;
        } else
            cfg->net.sendtable[i] = false;
    }
    return 0;
}

int config_init(struct config_s *cfg)
{
    if (!cfg) return -1;
    memset(cfg, 0, sizeof(*cfg));
    json_object *obj;
    if (os.loadjsonfile(&obj, "config/settings.cfg") != 0) return -1;
    json_object *tmp;
    json_object_object_get_ex(obj, "tracker_ip", &tmp);
    snprintf(cfg->net.tracker.ip, sizeof(cfg->net.tracker.ip), "%.*s",
             json_object_get_string_len(tmp),
             json_object_get_string(tmp));
    json_object_object_get_ex(obj, "tracker_port", &tmp);
    cfg->net.tracker.port = json_object_get_int(tmp);
    json_object_object_get_ex(obj, "proxy", &tmp);
    cfg->net.proxy = json_object_get_int(tmp);
    json_object_object_get_ex(obj, "interval_retry", &tmp);
    cfg->net.interval.retry = json_object_get_double(tmp);
    json_object_object_get_ex(obj, "interval_peers_reachable", &tmp);
    cfg->net.interval.peers_reachable = json_object_get_double(tmp);
    json_object_object_get_ex(obj, "mining_target", &tmp);
    cfg->miningtarget = json_object_get_int(tmp);
    json_object_object_get_ex(obj, "send_factor", &tmp);
    cfg->net.sendfactor = json_object_get_int(tmp);
    json_object_object_get_ex(obj, "max_chunk_size", &tmp);
    cfg->net.max.chunk_size = json_object_get_int(tmp);
    json_object_object_get_ex(obj, "max_task_buffer", &tmp);
    cfg->net.max.task_buffer = json_object_get_int(tmp);
    json_object_object_get_ex(obj, "max_send_retry", &tmp);
    cfg->net.max.send_retry = json_object_get_int(tmp);
    json_object_object_get_ex(obj, "max_upload_kbytes", &tmp);
    cfg->net.max.upload = json_object_get_int(tmp);
    json_object_object_get_ex(obj, "max_download_kbytes", &tmp);
    cfg->net.max.download = json_object_get_int(tmp);
    json_object_object_get_ex(obj, "max_send_queue", &tmp);
    cfg->net.max.send_queue = json_object_get_int(tmp);
    json_object_object_get_ex(obj, "max_peer_unreachable", &tmp);
    cfg->net.max.peer_unreachable = json_object_get_int(tmp);
    json_object_object_get_ex(obj, "download_directory", &tmp);
    snprintf(cfg->dir.download, sizeof(cfg->dir.download), "%.*s",
             json_object_get_string_len(tmp),
             json_object_get_string(tmp));
    json_object_object_get_ex(obj, "block_directory", &tmp);
    snprintf(cfg->dir.block, sizeof(cfg->dir.block), "%.*s",
             json_object_get_string_len(tmp),
             json_object_get_string(tmp));
    json_object_object_get_ex(obj, "keys_directory", &tmp);
    snprintf(cfg->dir.keys, sizeof(cfg->dir.keys), "%.*s",
             json_object_get_string_len(tmp),
             json_object_get_string(tmp));
    json_object_object_get_ex(obj, "finalized_directory", &tmp);
    snprintf(cfg->dir.finalized, sizeof(cfg->dir.finalized), "%.*s",
             json_object_get_string_len(tmp),
             json_object_get_string(tmp));
    json_object_object_get_ex(obj, "tcp_directory", &tmp);
    snprintf(cfg->dir.tcp, sizeof(cfg->dir.tcp), "%.*s",
             json_object_get_string_len(tmp),
             json_object_get_string(tmp));
    json_object_object_get_ex(obj, "pipe_read", &tmp);
    snprintf(cfg->api.pipes.read, sizeof(cfg->api.pipes.read), "%.*s",
             json_object_get_string_len(tmp),
             json_object_get_string(tmp));
    json_object_object_get_ex(obj, "pipe_write", &tmp);
    snprintf(cfg->api.pipes.write, sizeof(cfg->api.pipes.write), "%.*s",
             json_object_get_string_len(tmp),
             json_object_get_string(tmp));
    json_object_object_get_ex(obj, "tcp_ports_description", &tmp);
    snprintf(cfg->tcp.description, sizeof(cfg->tcp.description), "%.*s",
             json_object_get_string_len(tmp),
             json_object_get_string(tmp));
    json_object *allowed_ports;
    json_object_object_get_ex((json_object *)obj, "tcp_allowed_ports", &allowed_ports);
    if (json_object_get_type(allowed_ports) == json_type_array) {
       array_list *ap_array = json_object_get_array(allowed_ports);
       int i;
       for (i = 0; i < array_list_length(ap_array); i++) {
            json_object *ap_item = array_list_get_idx(ap_array, i);
            int *port = malloc(sizeof(*port));
            if (!port) return -1;
            *port = json_object_get_int(ap_item);
            ifr(list.add(&cfg->tcp.ports, port, port_clean));
       }
    }
    json_object_put(obj);
    if (rsa_load(cfg) != 0) {
        if (rsa_generate() != 0) return -1;
        if (rsa_load(cfg) != 0) return -1;
    }
    if (os.makedirs(cfg) != 0) return -1;
    int importkey(struct config_s *cfg, const char *fullpath,
                  const char *filename) {
        if (!cfg || !fullpath || !filename) return -1;
        printf("Importing shared key %s\n", fullpath);
        struct config_key_s *exists;
        ifr(config_keyexists(cfg, (unsigned char *)filename, &exists));
        if (exists) return 0;
        cfg->keys.shared.array = realloc(cfg->keys.shared.array,
                                         ++cfg->keys.shared.size * sizeof(struct config_key_s));
        if (!cfg->keys.shared.array) return -1;
        struct config_key_s *cfgk = &cfg->keys.shared.array[cfg->keys.shared.size - 1];
        memset(cfgk, 0, sizeof(*cfgk));
        memcpy(cfgk->sharedname, filename, SHA256HEX);
        ifr(rsa_loadkey(PEM_read_RSAPrivateKey, fullpath,
                        &cfgk->rsa.private,
                        &cfgk->str.private,
                        cfgk->hash.private));
        SHA256((unsigned char *)cfgk->str.private.s,
               cfgk->str.private.n,
               cfgk->aes.key);
        return 0;
    }
    ifr(os.readkeys(cfg, importkey));
    ifr(send_table(cfg));
    return 0;
}

void config_free(struct config_s *cfg)
{
    if (!cfg) return;
    if (cfg->keys.shared.array) {
        int i;
        for (i = 0; i < cfg->keys.shared.size; i++) {
            RSA_free(cfg->keys.shared.array[i].rsa.private);
            sn_free(cfg->keys.shared.array[i].str.private);
        }
        free(cfg->keys.shared.array);
    }
    list.clean(&cfg->tcp.ports);
    RSA_free(cfg->keys.local.rsa.public);
    RSA_free(cfg->keys.local.rsa.private);
    sn_free(cfg->keys.local.str.public);
    sn_free(cfg->keys.local.str.private);
    free(cfg->net.sendtable);
}
