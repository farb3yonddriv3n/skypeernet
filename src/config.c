#include <common.h>

int config_init(struct config_s *cfg)
{
    if (!cfg) return -1;
    if (rsa_load(cfg) != 0) {
        if (rsa_generate() != 0) return -1;
        if (rsa_load(cfg) != 0) return -1;
    }

    json_object *obj;
    if (os.loadjsonfile(&obj, "config/settings.cfg") != 0) return -1;
    json_object *tmp;
    json_object_object_get_ex(obj, "tracker_ip", &tmp);
    snprintf(cfg->net.tracker.ip, sizeof(cfg->net.tracker.ip), "%.*s",
             json_object_get_string_len(tmp),
             json_object_get_string(tmp));
    json_object_object_get_ex(obj, "tracker_port", &tmp);
    cfg->net.tracker.port = json_object_get_int(tmp);
    json_object_object_get_ex(obj, "interval_retry", &tmp);
    cfg->net.interval.retry = json_object_get_double(tmp);
    json_object_object_get_ex(obj, "interval_peers_reachable", &tmp);
    cfg->net.interval.peers_reachable = json_object_get_double(tmp);
    json_object_object_get_ex(obj, "interval_resend", &tmp);
    cfg->net.interval.resend = json_object_get_int(tmp);
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
    snprintf(cfg->download_dir, sizeof(cfg->download_dir), "%.*s",
             json_object_get_string_len(tmp),
             json_object_get_string(tmp));
    json_object_put(obj);
    return 0;
}

void config_free(struct config_s *cfg)
{
    RSA_free(cfg->rsakey.public);
    RSA_free(cfg->rsakey.private);
    sn_free(cfg->key.public);
    sn_free(cfg->key.private);
}
