/*
 *
 * GrizzlyCloud library - simplified VPN alternative for IoT
 * Copyright (C) 2017 - 2018 Filip Pancik
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 */
#include <gc.h>

static void ev_send(struct hm_pool_s *pool,
                    struct gc_ringbuffer_s *rb,
                    struct ev_loop *loop,
                    struct ev_io *write,
                    char *buf, int len)
{
    gc_ringbuffer_send_append(pool, rb, buf, len);
    ev_io_start(loop, write);
}

static void memory_append(gcsn *dst, const char *src, const int nsrc)
{
    assert(dst->n >= nsrc + dst->offset);
    memcpy(dst->s + dst->offset, src, nsrc);
    dst->offset += nsrc;
}

static int net_send(struct gc_s *gc, const char *buffer, const int nbuffer)
{
    int len = nbuffer;
    gcsn m = {
        .s      = hm_palloc(gc->pool, nbuffer + sizeof(int)),
        .n      = nbuffer + sizeof(int),
        .offset = 0};
    int tmplen = len;

    gc_swap_memory((void *)&len, sizeof(len));

    memory_append(&m, (void *)&len, sizeof(len));
    memory_append(&m, buffer, tmplen);

    struct gc_gen_client_ssl_s *c = &gc->client;
    gc_ssl_ev_send(c, m.s, m.n);

    hm_pfree(gc->pool, m.s);

    return GC_OK;
}

void gc_swap_memory(char *dst, int ndst)
{
    int i, j;

    for (i = 0, j = ndst - 1; (i < (ndst / 2) && j > 0); i++, j--) {
        dst[j] ^= dst[i];
        dst[i] ^= dst[j];
        dst[j] ^= dst[i];
    }
}

void gc_ssl_ev_send(struct gc_gen_client_ssl_s *client, char *buf, const int len)
{
    ev_send(client->base.pool, &client->base.rb,
            client->base.loop, &client->base.write, buf, len);
}

void gc_gen_ev_send(struct gc_gen_client_s *client, char *buf, const int len)
{
    ev_send(client->base.pool, &client->base.rb, client->base.loop,
            &client->base.write, buf, len);
}

int gc_packet_send(struct gc_s *gc, struct proto_s *pr)
{
    /*
    sn dst;
    if (gc_serialize(gc->pool, &dst, pr) != GC_OK) {
        hm_log(GCLOG_DEBUG, &gc->log, "Packet serialization failed");
        return GC_ERROR;
    }

    if (net_send(gc, dst.s, dst.n) != GC_OK) {
        hm_log(GCLOG_DEBUG, &gc->log, "Packet of size %d couldn't be sent", dst.n);
        return GC_ERROR;
    }

    gcsn_free(gc->pool, dst);
    */
    return GC_OK;
}

int gc_parse_delimiter(struct hm_pool_s *pool, gcsn input, char ***argv,
                       int *argc, char delimiter)
{
    char *start, *tmp;

#define ASET\
    *argv = hm_prealloc(pool, *argv, (size_t)((++(*argc)) * sizeof(void *)));\
    if (!argv) return GC_ERROR;\
    (*argv)[*argc - 1] = start;

    for (start = tmp = input.s, *argc = 0, *argv = NULL;
        tmp < (input.s + input.n);
        tmp++) {
        if (*tmp == delimiter) {
            ASET
            start = tmp + 1;
            *tmp = '\0'; // replace / with zero to terminate str
        }
    }

    ASET

    return GC_OK;
}

int gc_backend_parse(struct hm_pool_s *pool, struct gc_config_s *cfg, const char *path)
{
    char *content;
    int n;

    assert(cfg);

    n = gc_fread(pool, &content, path);
    if (n <= 1) {
        return GC_ERROR;
    }

    // Trim long paths up to sizeof(cfg->file) bytes
    snprintf(cfg->backends.file, sizeof(cfg->backends.file), "%s", path);

    struct json_tokener *tok = json_tokener_new();
    struct json_object *jobj = json_tokener_parse_ex(tok, content, n);
    json_tokener_free(tok);

    if (jobj == NULL) {
        return GC_ERROR;
    }

    struct json_object *backends;
    json_object_object_get_ex(jobj, "backends", &backends);
    if (json_object_get_type(backends) == json_type_array) {
        array_list *backends_array = json_object_get_array(backends);

        int i;
        for (i = 0; i < array_list_length(backends_array); i++) {
            struct json_object *backend = array_list_get_idx(backends_array, i);
            struct json_object *b_ip, *b_host;

#define BND(m_dst, m_name, m_src)\
        json_object_object_get_ex(backend, m_name, &m_src);\
        gcsn_setr(m_dst,\
                (char *)json_object_get_string(m_src),\
                json_object_get_string_len(m_src));

            BND(cfg->backends.item[i].ip,       "ip",       b_ip)
            BND(cfg->backends.item[i].hostname, "hostname", b_host)

            cfg->backends.n++;
        }
    }

#define BND_INT(m_dst, m_name, m_src)\
        json_object_object_get_ex(jobj, m_name, &m_src);\
        m_dst = json_object_get_int(m_src);

    struct json_object *b_compare;
    BND_INT(cfg->backends.compare,       "compare",      b_compare)

    cfg->backends.jobj = jobj;
    cfg->backends.content = content;

    return GC_OK;
}

int gc_config_parse(struct hm_pool_s *pool, struct gc_config_s *cfg, const char *path)
{
    char *content;
    int n;

    assert(cfg);

    n = gc_fread(pool, &content, path);
    if (n <= 1) {
        return GC_ERROR;
    }

    // Trim long paths up to sizeof(cfg->file) bytes
    snprintf(cfg->file, sizeof(cfg->file), "%s", path);

    struct json_tokener *tok = json_tokener_new();
    struct json_object *jobj = json_tokener_parse_ex(tok, content, n);
    json_tokener_free(tok);

    if (jobj == NULL) {
        return GC_ERROR;
    }

#define PRS(m_v, m_type)\
    struct json_object *m_v;\
    json_object_object_get_ex(jobj, #m_v, &m_v);\
    enum json_type type##m_v;\
    type##m_v = json_object_get_type(m_v);\
    if (type##m_v != m_type) {\
        return GC_ERROR;\
    }

#define VAL_STR(m_dst, m_src)\
    PRS(m_src, json_type_string);\
    gcsn_setr(m_dst,\
            (char *)json_object_get_string(m_src),\
            json_object_get_string_len(m_src));

    VAL_STR(cfg->username, user)
    VAL_STR(cfg->password, password)

    struct json_object *device;
    json_object_object_get_ex(jobj, "device", &device);
    if (json_object_get_type(device) == json_type_string) {
        gcsn_setr(cfg->device,
            (char *)json_object_get_string(device),
            json_object_get_string_len(device));
    }

    struct json_object *action;
    json_object_object_get_ex(jobj, "action", &action);
    if (json_object_get_type(action) == json_type_string) {
        gcsn_setr(cfg->action,
            (char *)json_object_get_string(action),
            json_object_get_string_len(action));
    }

    struct json_object *allow;
    json_object_object_get_ex(jobj, "allow", &allow);
    if (json_object_get_type(allow) == json_type_array) {
        array_list *allow_array = json_object_get_array(allow);
        int i;
        for (i = 0; i < array_list_length(allow_array); i++) {
            struct json_object *port = array_list_get_idx(allow_array, i);
            cfg->allowed[i] = json_object_get_int(port);
            cfg->nallowed++;
        }
    }

    struct json_object *tunnels;
    json_object_object_get_ex(jobj, "tunnels", &tunnels);
    if (json_object_get_type(tunnels) == json_type_array) {
        array_list *tunnels_array = json_object_get_array(tunnels);

        int i;
        for (i = 0; i < array_list_length(tunnels_array); i++) {
            struct json_object *tunnel = array_list_get_idx(tunnels_array, i);
            struct json_object *t_cloud, *t_device, *t_port, *t_port_local;

#define TUN(m_dst, m_name, m_src)\
        json_object_object_get_ex(tunnel, m_name, &m_src);\
        gcsn_setr(m_dst,\
                (char *)json_object_get_string(m_src),\
                json_object_get_string_len(m_src));

#define TUN_INT(m_dst, m_name, m_src)\
        json_object_object_get_ex(tunnel, m_name, &m_src);\
        m_dst = json_object_get_int(m_src);

            TUN(cfg->tunnels[i].cloud,      "cloud",     t_cloud)
            TUN(cfg->tunnels[i].device,     "device",    t_device)

            TUN_INT(cfg->tunnels[i].port,       "port",      t_port)
            TUN_INT(cfg->tunnels[i].port_local, "portLocal", t_port_local)

            cfg->tunnels[i].pid.n = 0;

            cfg->ntunnels++;
        }
    }

    cfg->jobj = jobj;
    cfg->content = content;

    return GC_OK;
}

void gc_config_dump(struct gc_config_s *cfg)
{
    int i;

    hm_log(GCLOG_DEBUG, cfg->log, "Using config: %s", cfg->file);
    hm_log(GCLOG_DEBUG, cfg->log, "Username: [%.*s]", gcsn_p(cfg->username));
    hm_log(GCLOG_DEBUG, cfg->log, "Password: [%s]", cfg->password.n > 0 ? "Set" : "Not Set");
    hm_log(GCLOG_DEBUG, cfg->log, "Device: [%.*s]", gcsn_p(cfg->device));

    hm_log(GCLOG_DEBUG, cfg->log, "Allowed ports total: [%d]", cfg->nallowed);

    for (i = 0; i < cfg->nallowed; i++) {
        hm_log(GCLOG_DEBUG, cfg->log, "Allowed port: [%d]", cfg->allowed[i]);
    }

    hm_log(GCLOG_DEBUG, cfg->log, "Allowed tunnels total: [%d]", cfg->ntunnels);

    for (i = 0; i < cfg->ntunnels; i++) {
        hm_log(GCLOG_DEBUG, cfg->log, "Tunnel %d: Cloud: [%.*s] Device: [%.*s] Port: [%d] Local port: [%d]",
                                    i,
                                    gcsn_p(cfg->tunnels[i].cloud),
                                    gcsn_p(cfg->tunnels[i].device),
                                    cfg->tunnels[i].port,
                                    cfg->tunnels[i].port_local);
    }

    hm_log(GCLOG_DEBUG, cfg->log, "Backends total: [%d]", cfg->backends.n);

    for (i = 0; i < cfg->backends.n; i++) {
        hm_log(GCLOG_DEBUG, cfg->log, "Backend IP: [%.*s] Hostname: [%.*s]",
                                    gcsn_p(cfg->backends.item[i].ip),
                                    gcsn_p(cfg->backends.item[i].hostname));
    }

    hm_log(GCLOG_DEBUG, cfg->log, "Backends compare: [%d]", cfg->backends.compare);

    hm_log(GCLOG_DEBUG, cfg->log, "Config dump ended");
}

int gc_fread(struct hm_pool_s *pool, char **dst, const char *fname)
{
    FILE *pfile;
    int lsize;
    char *buffer;
    int result;

    pfile = fopen(fname, "rb");
    if (pfile == NULL) {
        return -1;
    }

    fseek(pfile , 0 , SEEK_END);
    lsize = ftell(pfile);
    rewind(pfile);

    if (lsize > GC_MAX_FILE_SIZE) {
        fclose(pfile);
        return -1;
    }

    buffer = hm_palloc(pool, sizeof(char) * lsize);
    if (buffer == NULL) {
        fclose(pfile);
        return -1;
    }

    result = fread(buffer, sizeof(char), lsize, pfile);
    if (result != lsize) {
        fclose(pfile);
        hm_pfree(pool, buffer);
        return -1;
    }

    *dst = buffer;

    fclose(pfile);
    return result;
}

int gc_fwrite(char *fname, const char *mode, char *content, int ncontent)
{
    FILE *pfile;
    int result;

    pfile = fopen(fname, mode);
    if (pfile == NULL) {
        return -1;
    }

    result = fwrite(content, sizeof(char), ncontent, pfile);

    fclose(pfile);
    return !(result == ncontent);
}

int gc_fremove(const char *fname)
{
    return remove(fname);
}

void snbin2hexstr(gcsnb *dst, gcsnb *src)
{
    int i;
    char tmp[8];

    for (i = 0, dst->n = 0; i < src->n; i++) {
        snprintf(tmp, sizeof(tmp), "%x", (unsigned char)src->s[i]);
        int n = strlen(tmp);
        if ((dst->n + n) > sizeof(dst->s)) break;
        memcpy(dst->s + dst->n, tmp, n);
        dst->n += n;
    }
}
