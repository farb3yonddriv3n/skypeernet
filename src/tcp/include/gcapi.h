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
#ifndef GC_API_H_
#define GC_API_H_

/**
 * @brief Upstream port.
 */
#define GC_DEFAULT_PORT         17040

/**
 * @brief Upstream admin port.
 */
#define GC_ADMIN_PORT           17041

/**
 * @brief Maximum configured tunnels.
 */
#define GC_CFG_MAX_TUNNELS     32

/**
 * @brief Maximum allowed ports.
 */
#define GC_CFG_MAX_ALLOW_PORTS 32

/**
 * @brief Maximum backend nodes.
 */
#define GC_CFG_MAX_BACKENDS 32

/**
 * @brief GC state enum.
 *
 * Client's state related to upstream.
 */
enum gc_state_e {
    GC_CONNECTED = 0,       /**< Connected. */
    GC_HANDSHAKE_SUCCESS,   /**< TLS handshake. */
    GC_DISCONNECTED         /**< Disconnected. */
};

/**
 * @brief Configuration type.
 *
 * User configuration can specify any of the following.
 */
enum gc_cfg_type_e {
    GC_TYPE_HYBRID,         /**< Both server and client type. */
    GC_TYPE_SERVER,         /**< Instance only accepts incoming connections. */
    GC_TYPE_CLIENT,         /**< Instance only establishes tunnels to other GC instances. */
    GC_TYPE_ACTION          /**< Instance to perform single action. */
};

/**
 * @brief Tunnel configuration.
 *
 */
struct gc_config_tunnel_s {
    gcsn cloud;              /**< Destination cloud. */
    gcsn device;             /**< Destination device name. */
    int port;                /**< Destination port. */
    int port_local;          /**< Local port. */
    gcsnb pid;               /**< Paired process ID. */
};

struct gc_backend_item_s {
    gcsn ip;                 /**< IP address. */
    gcsn hostname;           /**< Hostname. */
};

struct gc_config_backend_s {
    int n;                                              /**< Number of backends. */
    struct gc_backend_item_s item[GC_CFG_MAX_BACKENDS]; /**< Array of backends. */

    int compare;                                        /**< Comparisson flag. */

    char file[64];                                      /**< Configuration file path. */
    struct json_object *jobj;                           /**< Parsed json configuration. */
    char *content;                                      /**< File buffer. */
};

/**
 * @brief GC configuration.
 *
 */
struct gc_config_s {
    gcsn username;                                      /**< Login username. */
    gcsn password;                                      /**< Login password. */
    gcsn device;                                        /**< Login device. */

    gcsn action;                                        /**< User action. */

    int ntunnels;                                       /**< Number of tunnels. */
    struct gc_config_tunnel_s tunnels[GC_CFG_MAX_TUNNELS];  /**< Array of tunnels. */

    struct ev_timer pair_timer;                         /**< Pair timer. */

    int nallowed;                                       /**< Number of allowed ports. */
    int allowed[GC_CFG_MAX_ALLOW_PORTS];                /**< Array of allowed ports. */

    enum gc_cfg_type_e type;                            /**< Type of configuration. */

    char file[64];                                      /**< Configuration file path. */

    struct hm_log_s *log;                               /**< Log stream. */
    struct json_object *jobj;                           /**< Parsed json configuration. */
    char *content;                                      /**< File buffer. */

    struct gc_config_backend_s backends;                /**< Backend nodes strcuture. */
};

/**
 * @brief Pair device as callback reply.
 *
 */
struct gc_device_pair_s {
    gcsn cloud;                                           /**< Paired cloud. */
    gcsn pid;                                             /**< Paired process ID. */
    gcsn device;                                          /**< Paired device name. */
    gcsnb port_local;                                     /**< Local port. */
    gcsn port_remote;                                     /**< Rmote port. */
    gcsn type;                                            /**< If "forced" entity is being paired. */
};

/**
 * @brief Library initialization structure.
 *
 */
struct gc_init_s {
    int            port;                                /**< Upstream port. */
    struct ev_loop *loop;                               /**< Event loop. */
    const char     *cfgfile;                            /**< Configuration file. */
    const char     *logfile;                            /**< Log file. */
    const char     *backendfile;                        /**< Backends file. */
    enum loglevel_e loglevel;                           /**< Log level. */
    int clientterm;                                     /**< Terminate when first client disconnects. */

    struct {
        void (*state_changed)(struct gc_s *gc, enum gc_state_e state);       /**< Upstream socket state cb. */
        void (*login)(struct gc_s *gc, gcsn error, gcsn data);                   /**< Login callback. */
        void (*traffic)(struct gc_s *gc, gcsn error, gcsn type, gcsn cloud,
                        gcsn device, gcsn upload, gcsn download);                  /**< Traffic callback. */
        void (*account_set)(struct gc_s *gc, gcsn error);                      /**< Account set callback. */
        void (*account_exists)(struct gc_s *gc, gcsn error, gcsn data);          /**< Account exists callback. */
        void (*account_data_set)(struct gc_s *gc, gcsn error);                 /**< Account data set callback. */
    } callback;
};

/**
 * @brief Main library strcutre.
 *
 */
struct gc_s {
    struct ev_loop      *loop;                          /**< Event loop. */
    struct hm_pool_s    *pool;                          /**< Memory pool. */
    struct hm_log_s     log;                            /**< Log structure. */
    struct ev_timer     connect_timer;                  /**< Event timer to re-establish upstream connection. */
    struct ev_timer     shutdown_timer;                 /**< Shutdown timer to close asynchronouslly. */
    struct ev_timer     hang_timer;                     /**< Hang timer. */
    struct ev_timer     ping_timer;                     /**< Ping timer. */
    gcsnb               hostname;                       /**< Upstream. */
    int                 port;                           /**< Upstream's port. */
    struct gc_gen_client_ssl_s client;                  /**< Client's structure. */
    struct gc_config_s  config;                         /**< Parsed config. */
    unsigned int        modules;                        /**< Flag of active modules. */
    int                 clientterm;                     /**< Terminate when first client disconnects. */

    struct {
        gcsn buf;                                         /**< Network buffer. */
    } net;

    struct {
        void (*state_changed)(struct gc_s *gc, enum gc_state_e state);       /**< Upstream socket state cb. */
        void (*login)(struct gc_s *gc, gcsn error, gcsn data);                   /**< Login callback. */
        void (*traffic)(struct gc_s *gc, gcsn error, gcsn type, gcsn cloud,
                        gcsn device, gcsn upload, gcsn download);                  /**< Traffic callback. */
        void (*account_set)(struct gc_s *gc, gcsn error);                      /**< Account set callback. */
        void (*account_exists)(struct gc_s *gc, gcsn error, gcsn data);          /**< Account exists callback. */
        void (*account_data_set)(struct gc_s *gc, gcsn error);                 /**< Account data set. */
    } callback;

    struct {
        void (*state_changed)(struct gc_s *gc, enum gc_state_e state);       /**< Upstream socket state cb. */
    } internal;
};

struct gc_s *gc_init(struct gc_init_s *init);

/**
 * @brief Deinitialization call.
 *
 * Clean network buffer, ssl, gc structure and event loop.
 * @return void.
 */
void gc_deinit(struct gc_s *gc);

/**
 * @brief Interrupt activity and clean any library related structures.
 *
 * Called after receiving SIGTERM.
 * @return void.
 */
void gc_force_stop();

extern int gc_sigterm;

#endif
