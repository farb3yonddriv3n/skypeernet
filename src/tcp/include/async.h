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
#ifndef GC_ASYNC_H_
#define GC_ASYNC_H_

#ifndef MSG_NOSIGNAL
#define MSG_NOSIGNAL 0
#endif

#define GC_DEFAULT_BACKLOG  8

/**
 * @brief Client flags.
 *
 */
enum gcflags_e {
    GC_WANT_SHUTDOWN = (1 << 0),    /**< Client's marked for shutdown. */
    GC_HANDSHAKED    = (1 << 1),    /**< Client already TLS handshaked. */
};

/**
 * @brief Socket related errors.
 *
 */
enum gcerr_e {
    GC_NOERROR,             /**< No error. */
    GC_WANTSHUTDOWN_ERR,    /**< Socket wants to shut down. */
    GC_READRBFULL_ERR,      /**< Read buffer is full. */
    GC_READZERO_ERR,        /**< End of file read. */
    GC_READ_ERR,            /**< Error reading from socket. */
    GC_WRITE_ERR,           /**< Error writing to socket. */
    GC_PACKETEXPECT_ERR,    /**< Packet length unexpected. */
    GC_SOCKET_ERR,          /**< Generic socket error. */
    GC_PING_ERR,            /**< Ping error. */
};

struct gc_gen_client_s;

/**
 * @brief Generic server structure.
 *
 */
struct gc_gen_server_s {
    struct ev_loop     *loop;         /**< Event loop. */
    struct hm_pool_s   *pool;         /**< Memory pool. */
    struct hm_log_s    *log;          /**< Log structure. */

    struct ev_io       listener;      /**< Incomming connections listener. */
    int                fd;            /**< File descriptor. */

    const char         *host;         /**< Listening hostname. */
    const char         *port;         /**< Listening port. */
    unsigned short     port_local;

    struct ht_s        *clients;      /**< Hashtable of clients. */

    void               *tunnel;       /**< Tunnel structure. */

    struct {
        void (*data)(struct gc_gen_client_s *data, char *buf, const int len);
    } callback;
};

/**
 * @brief Client template structure.
 *
 */
struct gc_client_s {
    struct ev_loop         *loop;       /**< Event loop. */
    struct hm_pool_s       *pool;       /**< Memory pool. */
    struct hm_log_s        *log;        /**< Log structure. */

    struct ev_io           read;        /**< Socket read event. */
    struct ev_io           write;       /**< Socket write event. */
    int                    fd;          /**< File descriptor. */

    struct gc_ringbuffer_s rb;          /**< Ringbuffer for both read and write. */

    enum gcflags_e         flags;       /**< Client flags. */

    struct {
        gcsnb ip;
        int port;
    } net;

    char                   date[32];    /**< Alive since date. */

    int                    active;      /**< Client shutdown or still active. */

    void                   *tunnel;     /**< Tunnel strucutre. */

    void                   *endpoint;   /**< Endpoint strucutre. */

    int                    reqidx;      /**< Request index. */

    struct ht_s            *packets;    /**< Client's TCP packets. */
};

struct gc_gen_client_s {
    struct gc_client_s     base;        /**< Client template structure. */

    struct gc_gen_server_s *parent;     /**< Server parent structure. */

    struct {
        void (*data)(struct gc_gen_client_s *client, char *buf, int len);
        void (*error)(struct gc_gen_client_s *client, enum gcerr_e error);
    } callback;
};

struct gc_gen_client_ssl_s {
    struct gc_client_s base;            /**< Client template structure. */

    struct sockaddr_in servaddr;        /**< Address structure. */

    struct ev_io       ev_w_connect;    /**< Connect event. */
    struct ev_io       ev_r_handshake;  /**< Handshake read event. */
    struct ev_io       ev_w_handshake;  /**< Handshake write event. */

    struct {
        char *buf;
        int n;
        int expect;
    } net;

    struct {
        void (*data)(struct gc_s *gc, const void *buffer, const int nbuffer);
        void (*error)(struct gc_gen_client_ssl_s *client, enum gcerr_e error);
        void (*terminate)(struct gc_gen_client_ssl_s *client, int error);
        void (*connected)(struct gc_gen_client_ssl_s *client);
    } callback;
};

/**
 * @brief Initialize generic server.
 *
 * @param cs Generic server structure.
 * @param tunnel External tunnel structure.
 * @return GC_OK on success, GC_ERROR on failure.
 */
int async_server(struct gc_gen_server_s *cs, void *tunnel);

/**
 * @brief Shutdown generic server.
 *
 * @param cs Generic server structure.
 * @return void.
 */
void async_server_shutdown(struct gc_gen_server_s *cs);

/**
 * @brief Initialize generic client.
 *
 * @param cs Generic client structure.
 * @return GC_OK on success, GC_ERROR on failure.
 */
int async_client(struct gc_gen_client_s *client);

/**
 * @brief Shutdown generic client.
 *
 * @param c Generic client structure.
 * @return void.
 */
void async_client_shutdown(struct gc_gen_client_s *c);

/**
 * @brief Initialize generic ssl client.
 *
 * @param gc GC structure.
 * @return GC_OK on success, GC_ERROR on failure.
 */
int async_client_ssl(struct gc_s *gc);

/**
 * @brief Shutdown generic ssl client.
 *
 * @param c Generic ssl client structure.
 * @return void.
 */
void async_client_ssl_shutdown(struct gc_gen_client_ssl_s *c);

/**
 * @brief Shutdown generic ssl client.
 *
 * @param c Generic ssl client structure.
 * @return void.
 */
void async_handle_socket_errno(struct hm_log_s *l);

/**
 * @brief Send encrypted data to upstream.
 *
 * @param client Generic ssl client.
 * @param buf Data to send.
 * @param len Length of data.
 * @return void.
 */
void gc_ssl_ev_send(struct gc_gen_client_ssl_s *client, char *buf, const int len);

/**
 * @brief Send unencrypted data to endpoint or tunnel.
 *
 * @param client Generic client.
 * @param buf Data to send.
 * @param len Length of data.
 * @return void.
 */
void gc_gen_ev_send(struct gc_gen_client_s *client, char *buf, const int len);

#endif
