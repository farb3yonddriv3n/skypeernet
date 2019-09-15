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

int gc_sigterm = 0;

static struct gc_s *gclocal = NULL;
/*
int endpoint_stop(struct gc_s *gc, struct proto_s *p, char **argv, int argc)
{
    gcsn_initz(ext, "ext");

    gc_endpoint_stop(gc->pool, &gc->log,
                     p->u.message_from.from_address,
                     ext,
                     ext);

    return GC_OK;
}

static int message_from(struct gc_s *gc, struct proto_s *p)
{
    char **argv;
    int argc;
    int ret;

    ret = gc_parse_delimiter(gc->pool, p->u.message_from.tp,
                             &argv, &argc, '/');
    if (ret != GC_OK) {
        if (argv) hm_pfree(gc->pool, argv);
        return ret;
    }

    if (argc < 1) {
        if (argv) hm_pfree(gc->pool, argv);
        return GC_ERROR;
    }

    gcsn_initr(type, argv[0], strlen(argv[0]));
    gcsn_initz(response, "tunnel_response");
    gcsn_initz(request, "tunnel_request");
    gcsn_initz(update, "tunnel_update");
    gcsn_initz(estop, "endpoint_stop");

    if (gcsn_cmps(type, request)) {
        ret = gc_endpoint_request(gc, p, argv, argc);
        if (ret != GC_OK) {
            hm_log(GCLOG_TRACE, &gc->log, "Tunnel request failed");
        }
    } else if (gcsn_cmps(type, response)) {
        ret = gc_tunnel_response(gc, p, argv, argc);
        if (ret != GC_OK) {
            hm_log(GCLOG_TRACE, &gc->log, "Tunnel reponse failed");
        }
    } else if (gcsn_cmps(type, update)) {
        ret = gc_tunnel_update(gc, p, argv, argc);
        if (ret != GC_OK) {
            hm_log(GCLOG_TRACE, &gc->log, "Tunnel update failed");
        }
    } else if (gcsn_cmps(type, estop)) {
        ret = endpoint_stop(gc, p, argv, argc);
        if (ret != GC_OK) {
            hm_log(GCLOG_TRACE, &gc->log, "Endpoint stop failed");
        }
    } else {
        abort();
    }

    hm_pfree(gc->pool, argv);

    return GC_OK;
}

static void pairs_offline(struct gc_s *gc, sn address)
{
    int i;
    for (i = 0; i < gc->config.ntunnels; i++) {
        if (address.n == 0) {
            if (gcsn_len(gc->config.tunnels[i].pid) > 0)
                fs_unpair(&gc->log, &gc->config.tunnels[i].pid, gc->config.tunnels[i].port_local);
            gc->config.tunnels[i].pid.n = 0;
        } else if (gcsn_cmps(gc->config.tunnels[i].pid, address)) {
            hm_log(GCLOG_TRACE, &gc->log, "Tunnel marking pair [cloud:device:port:port_local] [%.*s:%.*s:%d:%d] offline",
                                      gcsn_p(gc->config.tunnels[i].cloud),
                                      gcsn_p(gc->config.tunnels[i].device),
                                      gc->config.tunnels[i].port,
                                      gc->config.tunnels[i].port_local);
            fs_unpair(&gc->log, &gc->config.tunnels[i].pid, gc->config.tunnels[i].port_local);
            gc->config.tunnels[i].pid.n = 0;
        }
    }
}

static void cloud_offline(struct gc_s *gc, struct proto_s *p)
{
    hm_log(GCLOG_TRACE, &gc->log, "Cloud device offline [cloud:device:port:port_local] [%.*s:%.*s]",
                                gcsn_p(p->u.offline_set.cloud),
                                gcsn_p(p->u.offline_set.device));

    pairs_offline(gc, p->u.offline_set.address);
    gc_endpoint_stop(gc->pool, &gc->log,
                     p->u.offline_set.address,
                     p->u.offline_set.cloud,
                     p->u.offline_set.device);

    gc_tunnel_stop(gc->pool, &gc->log, p->u.offline_set.address);
}

static void gc_upstream_force_stop(struct ev_loop *loop)
{
    hm_log(GCLOG_TRACE, &gclocal->log, "Upstream force stop");
    ev_timer_stop(loop, &gclocal->connect_timer);
    if (gclocal->client.base.active) {
        async_client_ssl_shutdown(&gclocal->client);
        gclocal->client.base.active = 0;
    }
}

static void upstream_error(struct gc_gen_client_ssl_s *c, enum gcerr_e error)
{
    hm_log(GCLOG_TRACE, c->base.log, "Upstream error %d", error);

    // Remove tunnels' pid's
    gcsn_initr(empty_pid, "", 0);
    pairs_offline(gclocal, empty_pid);

    // Stop pair timer
    ev_timer_stop(gclocal->loop, &gclocal->config.pair_timer);

    // Stop hand ang ping timers
    ev_timer_stop(c->base.loop, &c->base.gc->ping_timer);
    ev_timer_stop(c->base.loop, &c->base.gc->hang_timer);

    gc_tunnel_stop_all(c->base.pool, c->base.log);
    gc_endpoints_stop_all();
    if (c->base.active) {
        async_client_ssl_shutdown(c);
        c->base.active = 0;
    }
    ev_timer_again(gclocal->loop, &gclocal->connect_timer);
}

static void callback_error(struct gc_gen_client_ssl_s *c, enum gcerr_e error)
{
    upstream_error(c, error);
}

static void device_pair_reply(struct gc_s *gc, struct gc_device_pair_s *pair)
{
    if (gc_tunnel_add(gc, pair, pair->type) != GC_OK) {
        gc_force_stop();
        return;
    }

    gcsn_initz(forced, "forced");
    if (gcsn_cmps(pair->type, forced)) {
        return;
    }

    int i;
    for (i = 0; i < gc->config.ntunnels; i++) {
        gcsn_itoa(port,       gc->config.tunnels[i].port, 8);
        gcsn_itoa(port_local, gc->config.tunnels[i].port_local,  8);

        if (gcsn_cmps(gc->config.tunnels[i].cloud, pair->cloud) &&
           gcsn_cmps(gc->config.tunnels[i].device, pair->device) &&
           gcsn_cmps(port, pair->port_remote) &&
           gcsn_cmps(port_local, pair->port_local)) {

            hm_log(GCLOG_TRACE, &gc->log, "Tunnel [cloud:device:port:port_local] [%.*s:%.*s:%.*s:%.*s] active",
                                        gcsn_p(pair->cloud), gcsn_p(pair->device),
                                        gcsn_p(port), gcsn_p(port_local));
            snb_cpy_ds(gc->config.tunnels[i].pid, pair->pid);
            fs_pair(&gc->log, pair);
            return;
        }
    }

    hm_log(GCLOG_WARNING, &gc->log, "Tunnel [cloud:device:port:port_local] [%.*s:%.*s:%.*s:%.*s] not paired",
                                  gcsn_p(pair->cloud),
                                  gcsn_p(pair->device),
                                  gcsn_p(pair->port_local),
                                  gcsn_p(pair->port_remote));
}

static void traffic_mi(struct gc_s *gc)
{
    struct proto_s pr = { .type = TRAFFIC_MI };

    int ret;
    ret = gc_packet_send(gc, &pr);
    if (ret != GC_OK) CALLBACK_ERROR(&gc->log, "traffic_mi");
}

static void devices_pair(struct ev_loop *loop, struct ev_timer *timer, int revents)
{
    struct gc_s *gc = (struct gc_s *)timer->data;

    assert(gc);

    int i;
    for (i = 0; i < gc->config.ntunnels; i++) {
        if (gcsn_len(gc->config.tunnels[i].pid) != 0) continue;

        struct proto_s pr = { .type = DEVICE_PAIR };
        gcsn_set(pr.u.device_pair.cloud,       gc->config.tunnels[i].cloud);
        gcsn_set(pr.u.device_pair.device,      gc->config.tunnels[i].device);

        gcsn_itoa(port,       gc->config.tunnels[i].port, 8);
        gcsn_itoa(port_local, gc->config.tunnels[i].port_local, 8);

        gcsn_set(pr.u.device_pair.local_port,  port_local);
        gcsn_set(pr.u.device_pair.remote_port, port);

        hm_log(GCLOG_TRACE, &gc->log, "Attempt to pair [cloud:device:port:port_local] [%.*s:%.*s:%.*s:%.*s]",
                                    gcsn_p(gc->config.tunnels[i].cloud),
                                    gcsn_p(gc->config.tunnels[i].device),
                                    gcsn_p(port), gcsn_p(port_local));
        int ret;
        ret = gc_packet_send(gc, &pr);
        if (ret != GC_OK) CALLBACK_ERROR(&gc->log, "device_pair");
    }
}

static void modules_stop(struct gc_s *gc)
{
    int i;
    for (i = 0; i < MAX_MODULES; i++) {
        if (modules_available[i]->stop)
            modules_available[i]->stop(gc, modules_available[i]);
    }
}

static enum gc_e modules_start(struct gc_s *gc)
{
    int i;
    for (i = 0; i < MAX_MODULES; i++) {
        if ((gc->modules & modules_available[i]->id) &&
            modules_available[i]->start)
            if (modules_available[i]->start(gc, modules_available[i]) != GC_OK)
                return GC_ERROR;
    }

    return GC_OK;
}

static void client_logged(struct gc_s *gc, sn error)
{
    gcsn_initz(ok, "ok");
    gcsn_initz(ok_reg, "ok_registered");

    if (gcsn_cmps(ok, error)) {
        gcsn_initz(traffic, "traffic");
        if (gcsn_cmps(gc->config.action, traffic)) {
            traffic_mi(gc);
        } else {
            ev_init(&gc->config.pair_timer, devices_pair);
            gc->config.pair_timer.repeat = 2.0;
            gc->config.pair_timer.data = gc;
            ev_timer_again(gc->loop, &gc->config.pair_timer);
        }
    } else if (!gcsn_cmps(ok_reg, error)) {
        gc_force_stop();
    }
}

static void parse_traffic(struct gc_s *gc, sn error, sn list)
{
    int i;
    char *s = list.s;

    if (list.n == 0) {
        gcsn_initz(type,     "");
        gcsn_initz(cloud,    "");
        gcsn_initz(device,   "");
        gcsn_initz(upload,   "");
        gcsn_initz(download, "");
        if (gc->callback.traffic) gc->callback.traffic(gc, error, type, cloud,
                                                      device, upload, download);
        return;
    }

    for (i = 0; ; ) {
#define PT(m_dst)\
        if (i >= list.n) break;\
        sn m_dst = { .n = *(int *)(s + i), .s = s + i + sizeof(int) };\
        gc_swap_memory((void *)&m_dst.n, sizeof(m_dst.n));\
        i += sizeof(int) + m_dst.n;

        PT(type);
        PT(cloud);
        PT(device);
        PT(upload);
        PT(download);

        if (gc->callback.traffic) gc->callback.traffic(gc, error, type, cloud,
                                                      device, upload, download);
    }
}

static void state_changed(struct gc_s *gc, enum gc_state_e state)
{
    if (state == GC_HANDSHAKE_SUCCESS) {
        ev_timer_again(gc->loop, &gc->hang_timer);
        ev_timer_again(gc->loop, &gc->ping_timer);
    }

    if (gc && gc->callback.state_changed)
        gc->callback.state_changed(gc, state);
}

static void hang_reset(struct gc_s *gc)
{
    ev_timer_stop(gc->loop, &gc->hang_timer);
    ev_timer_again(gc->loop, &gc->hang_timer);
}

static void hang(struct ev_loop *loop, struct ev_timer *timer, int revents)
{
    struct gc_s *gc = timer->data;
    upstream_error(&gc->client, GC_PING_ERR);
}

static void ping(struct ev_loop *loop, struct ev_timer *timer, int revents)
{
    struct gc_s *gc = timer->data;
    struct proto_s pr = { .type = PING_SET };

    int ret;
    ret = gc_packet_send(gc, &pr);
    if (ret != GC_OK) CALLBACK_ERROR(&gc->log, "ping_set");
}

static void upstream_connect(struct ev_loop *loop, struct ev_timer *timer, int revents)
{
    struct gc_s *gc;
    gc = (struct gc_s *)timer->data;

    ev_timer_stop(loop, &gc->connect_timer);

    memset(&gc->client, 0, sizeof(gc->client));

    gc->client.base.loop = loop;
    gc->client.base.pool = gc->pool;
    gc->client.base.log  = &gc->log;

    gc->client.base.net.port = gc->port;
    snb_cpy_ds(gc->client.base.net.ip, gc->hostname);

    gc->client.callback.data  = callback_data;
    gc->client.callback.error = callback_error;

    async_client_ssl(gc);
    (void )revents;
}

void gc_deinit(struct gc_s *gc)
{
    if (gc->net.buf.s) hm_pfree(gc->pool, gc->net.buf.s);

    hm_log_close(&gc->log);

    FIPS_mode_set(0);
    ENGINE_cleanup();
    CONF_modules_unload(1);
    EVP_cleanup();
    CRYPTO_cleanup_all_ex_data();
#if (OPENSSL_VERSION_NUMBER <= 0x100020ffL)
    ERR_remove_state(0);
#endif
    ERR_free_strings();

    struct hm_pool_s *pool = gc->pool;
    hm_pfree(pool, gc);

    hm_destroy_pool(pool);

    ev_default_destroy();
}

static void sigh_terminate(int __attribute__ ((unused)) signo)
{
    if (gc_sigterm == 1) return;

    gc_sigterm = 1;
    hm_log(GCLOG_TRACE, &gclocal->log, "Received SIGTERM");
    ev_timer_again(gclocal->loop, &gclocal->shutdown_timer);
}

static void gc_signals(struct gc_s *gc)
{
    struct sigaction act;

    sigemptyset(&act.sa_mask);
    act.sa_flags = 0;
    act.sa_handler = SIG_IGN;

    if (sigaction(SIGPIPE, &act, NULL) < 0) {
        hm_log(GCLOG_CRIT, &gc->log, "Sigaction cannot be examined");
    }

    act.sa_flags = SA_NOCLDSTOP;

    act.sa_flags = 0;
    act.sa_handler = sigh_terminate;
    if (sigaction(SIGINT, &act, NULL) < 0) {
        hm_log(GCLOG_CRIT, &gc->log, "Unable to register SIGINT signal handler: %s", strerror(errno));
        exit(1);
    }

    if (sigaction(SIGTERM, &act, NULL) < 0) {
        hm_log(GCLOG_CRIT, &gc->log, "Unable to register SIGTERM signal handler: %s", strerror(errno));
        exit(1);
    }
}

static int config_init(struct hm_pool_s *pool, struct gc_config_s *cfg,
                       const char *cfgfile, const char *backendfile)
{
    int ret;

    ret = gc_config_parse(pool, cfg, cfgfile);
    if (ret != GC_OK) {
        hm_log(GCLOG_CRIT, cfg->log, "Parsing config file [%s] failed", cfgfile);
        return GC_ERROR;
    }

    ret = gc_backend_parse(pool, cfg, backendfile);
    if (ret != GC_OK) {
        hm_log(GCLOG_CRIT, cfg->log, "Parsing backend file [%s] failed", backendfile);
        return GC_ERROR;
    }

    assert(cfg->log);
    gc_config_dump(cfg);

    return GC_OK;
}

static int config_required(struct gc_config_s *cfg)
{
    assert(cfg);

    if ((cfg->username.n == 0 || cfg->password.n == 0) ||
        (cfg->device.n == 0 && cfg->action.n == 0)) {
        hm_log(GCLOG_CRIT, cfg->log, "Username, password and device or action must be set");
        return GC_ERROR;
    }

    if (cfg->ntunnels == 0 && cfg->nallowed == 0 && cfg->action.n == 0) {
        hm_log(GCLOG_CRIT, cfg->log, "Neither tunnels, allowed ports nor action specified");
        return GC_ERROR;
    }

    if (cfg->ntunnels > 0 && cfg->nallowed > 0) cfg->type = GC_TYPE_HYBRID;
    else if (cfg->ntunnels > 0)                 cfg->type = GC_TYPE_CLIENT;
    else if (cfg->nallowed > 0)                 cfg->type = GC_TYPE_SERVER;
    else if (cfg->action.n > 0)                 cfg->type = GC_TYPE_ACTION;
    else                                       return GC_ERROR;

    return GC_OK;
}

static void stop(struct ev_loop *loop, struct ev_timer *timer, int revents);

struct gc_s *gc_init(struct gc_init_s *init)
{
    struct gc_s *gc = NULL;

    assert(init);

    struct hm_pool_s *pool = hm_create_pool();

    if (pool == NULL) {
        return NULL;
    }

    gc = gclocal = hm_palloc(pool, sizeof(*gc));
    memset(gc, 0, sizeof(*gc));

    if (hm_log_open(&gc->log, init->logfile, init->loglevel) != GC_OK) {
        return NULL;
    }

    pool->log = &gc->log;

    // Set memory pool
    gc->pool = pool;

    hm_log(GCLOG_DEBUG, &gc->log, "Openssl version: 0x%lx", OPENSSL_VERSION_NUMBER);
    hm_log(GCLOG_DEBUG, &gc->log, "Json-c version: %s",     JSON_C_VERSION);
    hm_log(GCLOG_DEBUG, &gc->log, "Libev version: %d.%d",   EV_VERSION_MAJOR,
                                                          EV_VERSION_MINOR);

    gc->config.log = &gc->log;
    if (config_init(gc->pool, &gc->config, init->cfgfile, init->backendfile) != GC_OK) {
        hm_log(GCLOG_CRIT, &gc->log, "Could not initialize config file");
        return NULL;
    }

    if (config_required(&gc->config) != GC_OK) {
        hm_log(GCLOG_CRIT, &gc->log, "Mandatory configuration parameters are missing");
        return NULL;
    }

    // Copy over initialization settings
    gc->loop                     = init->loop;
    gc->callback.state_changed   = init->callback.state_changed;
    gc->callback.login           = init->callback.login;
    gc->callback.traffic         = init->callback.traffic;
    gc->callback.account_set     = init->callback.account_set;
    gc->callback.account_exists  = init->callback.account_exists;
    gc->callback.account_data_set= init->callback.account_data_set;
    gc->modules                  = init->module;

    gc->internal.state_changed   = state_changed;

    if (gc_backend_init(gc, &gc->hostname) != GC_OK) {
        return NULL;
    }

    gc->port = init->port > 0 ? init->port : GC_DEFAULT_PORT;
    gc->clientterm = init->clientterm;

    // Initialize signals
    gc_signals(gc);

    if (SSL_library_init() < 0) {
        hm_log(GCLOG_CRIT, &gc->log, "Could not initialize OpenSSL library");
        return NULL;
    }

    SSL_load_error_strings();

    ev_init(&gc->hang_timer, hang);
    gc->hang_timer.repeat = 15;
    gc->hang_timer.data = gc;

    ev_init(&gc->ping_timer, ping);
    gc->ping_timer.repeat = 10;
    gc->ping_timer.data = gc;

    // Try to connect to upstream every .repeat seconds
    ev_init(&gc->connect_timer, upstream_connect);
    gc->connect_timer.repeat = 2.0;
    gc->connect_timer.data = gc;
    ev_timer_again(gc->loop, &gc->connect_timer);

    ev_init(&gc->shutdown_timer, stop);
    gc->shutdown_timer.repeat = 0.1;
    gc->shutdown_timer.data = gc;

    if (modules_start(gc) != GC_OK) {
        hm_log(GCLOG_CRIT, &gc->log, "Modules initialization failed");
        return NULL;
    }

    return gc;
}

static void gc_config_free(struct hm_pool_s *pool, struct gc_config_s *cfg)
{
    ev_timer_stop(gclocal->loop, &cfg->pair_timer);
    json_object_put(cfg->jobj);
    json_object_put(cfg->backends.jobj);
    hm_pfree(pool, cfg->content);
    hm_pfree(pool, cfg->backends.content);
}

static void stop(struct ev_loop *loop, struct ev_timer *timer, int revents)
{
    struct gc_s *gc = (struct gc_s *)timer->data;

    ev_timer_stop(gc->loop, &gc->shutdown_timer);
    ev_timer_stop(gc->loop, &gc->ping_timer);
    ev_timer_stop(gc->loop, &gc->hang_timer);

    modules_stop(gc);
    gc_config_free(gc->pool, &gc->config);
    gc_upstream_force_stop(gc->loop);
    gc_tunnel_stop_all(gc->pool, &gc->log);
    gc_endpoints_stop_all();
}
*/

void gc_force_stop()
{
    ev_timer_again(gclocal->loop, &gclocal->shutdown_timer);
}
