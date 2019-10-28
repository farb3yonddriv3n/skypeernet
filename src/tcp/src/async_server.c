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

/*
static void endpoint_stop(struct gc_gen_client_s *client, struct gc_tunnel_s *tunnel, int fd)
{
    gcsn_initz(payload, "data");

    // Message header
    char header[64];
    snprintf(header, sizeof(header), "endpoint_stop/%d", fd);

    gcsn_initz(snheader, header);

    struct proto_s m = { .type = MESSAGE_TO };
    gcsn_set(m.u.message_to.to,      tunnel->device);
    gcsn_set(m.u.message_to.address, tunnel->pid);
    gcsn_set(m.u.message_to.body,    payload);
    gcsn_set(m.u.message_to.tp,      snheader);

    gc_packet_send(client->base.gc, &m);
}
*/

void async_client_shutdown(struct gc_gen_client_s *c)
{
    assert(c);
    struct hm_pool_s *p = c->base.pool;

    ev_io_stop(c->base.loop, &c->base.read);
    ev_io_stop(c->base.loop, &c->base.write);

    c->base.flags |= GC_WANT_SHUTDOWN;

    gc_ringbuffer_send_pop_all(c->base.pool, &c->base.rb);

    hm_log(GCLOG_DEBUG, c->base.log, "Removing TCP client [%.*s:%d] fd: [%d] alive since: [%s]",
                                   gcsn_p(c->base.net.ip), c->base.net.port,
                                   c->base.fd, c->base.date);

    /*
    if (c->parent && c->parent->tunnel) {
        endpoint_stop(c, c->parent->tunnel, c->base.fd);
    }
    */

    int ret;
    ret = gc_fd_close(c->base.fd);
    if (ret != GC_OK) {
        hm_log(GCLOG_TRACE, c->base.log, "File descriptor %d failed to closed",
                                       c->base.fd);
    }

    if (c->parent) {
        char key[16];
        snprintf(key, sizeof(key), "%d", c->base.fd);
        ht_rem(c->parent->clients, key, strlen(key));
    }

    if (c->base.packets) ht_free(c->base.packets);
    //struct gc_s *gc = c->base.gc;
    hm_pfree(p, c);

    /*
    if (gc->clientterm) {
        gc_force_stop();
    }
    */
}

static void client_error(struct gc_gen_client_s *c, enum gcerr_e err)
{
    hm_log(GCLOG_TRACE, c->base.log, "Client error %d on fd %d",
                                   err, c->base.fd);
    async_client_shutdown(c);
}

inline static void recv_append(struct gc_gen_client_s *c)
{
    int sz;
    char *buffer;

    assert(c);

    buffer = gc_ringbuffer_recv_read(&c->base.rb, &sz);
    c->callback.data(c, buffer, sz);
    gc_ringbuffer_recv_pop(c->base.pool, &c->base.rb);
}

void async_handle_socket_errno(struct hm_log_s *l)
{
    if (errno == EAGAIN || errno == EWOULDBLOCK || errno == EINTR) {
        hm_log(GCLOG_TRACE, l, "Client error: Socket errno %d", errno);
        return;
    }

    if (errno == ECONNRESET) {
        hm_log(GCLOG_TRACE, l, "Client error: connection reset by peer");
    } else if (errno == ETIMEDOUT) {
        hm_log(GCLOG_TRACE, l, "Client error: connection to backend timed out");
    } else if (errno == EPIPE) {
        hm_log(GCLOG_TRACE, l, "Client error: broken pipe to backend (EPIPE)");
    } else {
        hm_log(GCLOG_TRACE, l, "Client error: errno %d error: %s", errno, strerror(errno));
    }
}

static void async_read(struct ev_loop *loop, ev_io *w, int revents)
{
    (void)revents;
    int sz;
    struct gc_gen_client_s *c;
    int fd;

    if (gc_sigterm == 1) return;

    assert(w);
    c = (struct gc_gen_client_s *)w->data;
    fd = w->fd;

    assert(c);

    if (EQFLAG(c->base.flags, GC_WANT_SHUTDOWN)) {
        if (c->callback.error) {
            c->callback.error(c, GC_WANTSHUTDOWN_ERR);
        }
        return;
    }

    sz = recv(fd, c->base.rb.recv.tmp, RB_SLOT_SIZE, 0);

    if (sz > 0) {
        gc_ringbuffer_recv_append(c->base.pool, &c->base.rb, sz);

        if (gc_ringbuffer_recv_is_full(&c->base.rb)) {
            ev_io_stop(c->base.loop, &c->base.read);
            if (c->callback.error) {
                c->callback.error(c, GC_READRBFULL_ERR);
            }
            return;
        }

        recv_append(c);

    } else if (sz == 0) {
        ev_io_stop(c->base.loop, &c->base.read);
        async_handle_socket_errno(c->base.log);
        if (c->callback.error) {
            c->callback.error(c, GC_READZERO_ERR);
        }
     } else if (sz == -1 &&
        (errno == EAGAIN || errno == EWOULDBLOCK || errno == EINTR)) {
        hm_log(GCLOG_TRACE, c->base.log, "Socket read EAGAIN|EWOULDBLOCK|EINTR");
        async_handle_socket_errno(c->base.log);
    } else {
        ev_io_stop(c->base.loop, &c->base.read);
        async_handle_socket_errno(c->base.log);
        if (c->callback.error) {
            c->callback.error(c, GC_READ_ERR);
        }
    }
}

static void async_write(struct ev_loop *loop, ev_io *w, int revents)
{
    (void)revents;
    struct gc_gen_client_s *c;
    int fd;
    int sz;

    if (gc_sigterm == 1) return;

    assert(w);
    c = (struct gc_gen_client_s *)w->data;
    fd = w->fd;

    assert(c);

    if (gc_ringbuffer_send_is_empty(&c->base.rb)) {
        ev_io_stop(loop, &c->base.write);
        return;
    }

    char *next = gc_ringbuffer_send_next(&c->base.rb, &sz);

    if (sz == 0) {
        ev_io_stop(loop, &c->base.write);
        return;
    }

    sz = send(fd, next, sz, MSG_NOSIGNAL);
    if (sz > 0) {
        gc_ringbuffer_send_skip(c->base.pool, &c->base.rb, sz);
        if (gc_ringbuffer_send_is_empty(&c->base.rb)) {
            ev_io_stop(loop, &c->base.write);
        }
    } else {
        async_handle_socket_errno(c->base.log);
        if (c->callback.error) {
            c->callback.error(c, GC_WRITE_ERR);
        }
    }
}

static int async_client_accept(struct gc_gen_client_s *client)
{
    ev_io_init(&client->base.write, async_write, client->base.fd, EV_WRITE);
    ev_io_init(&client->base.read, async_read, client->base.fd, EV_READ);

    client->base.read.data = client;
    client->base.write.data = client;

    ev_io_start(client->base.loop, &client->base.read);

    gc_timestring(client->base.date, sizeof(client->base.date));

    return GC_OK;
}

static int connector_addclient(struct gc_gen_server_s *cs, struct gc_gen_client_s *cc)
{
    char key[8];
    snprintf(key, sizeof(key), "%d", cc->base.fd);
    if (HT_ADD_WA(cs->clients, key, strlen(key), cc, sizeof(cc)) != GC_OK) {
        hm_log(GCLOG_ERR, cs->log, "Cannot add key [%s] to hashtable", key);
        return GC_ERROR;
    }

    hm_log(GCLOG_DEBUG, cs->log, "Adding tunnel TCP client [%.*s:%d] fd: [%d]",
                                gcsn_p(cc->base.net.ip), cc->base.net.port, cc->base.fd);

    return GC_OK;
}

static void server_async_client(struct ev_loop *loop, ev_io *w, int revents)
{
    (void) revents;
    struct sockaddr_storage addr;
    socklen_t sl = sizeof(addr);
    int client;
    struct gc_gen_client_s *cc;
    struct gc_gen_server_s *cs = w->data;

    if (gc_sigterm == 1) return;

    assert(cs);

    client = accept(w->fd, (struct sockaddr *) &addr, &sl);
    if (client == -1) {
        switch (errno) {
            case EMFILE:
                hm_log(GCLOG_ERR, cs->log, "Accept() failed; too many open files for this process");
                break;

            case ENFILE:
                hm_log(GCLOG_ERR, cs->log, "Accept() failed; too many open files for this system");
                break;

            default:
                assert(errno == EINTR || errno == EWOULDBLOCK || errno == EAGAIN);
                break;
        }
        return;
    }

    int flag = 1;
    int ret = setsockopt(client, IPPROTO_TCP, TCP_NODELAY, (char *)&flag, sizeof(flag));
    if (ret != GC_OK) {
        hm_log(GCLOG_ERR, cs->log, "Couldn't setsockopt on client (TCP_NODELAY)");
    }
#ifdef TCP_CWND
    int cwnd = 10;
    ret = setsockopt(client, IPPROTO_TCP, TCP_CWND, &cwnd, sizeof(cwnd));
    if (ret != GC_OK) {
        hm_log(GCLOG_ERR, cs->log, "Couldn't setsockopt on client (TCP_CWND)");
    }
#endif

    ret = gc_fd_setnonblock(client);
    if (ret != GC_OK) {
        hm_log(GCLOG_TRACE, cs->log, "Failed to set nonblock() on fd %d", client);
    }

    ret = gc_fd_setkeepalive(client);
    if (ret != GC_OK) {
        hm_log(GCLOG_TRACE, cs->log, "Failed to set keepalive() on fd %d", client);
    }

#define PEER_NAME
#ifdef PEER_NAME
    socklen_t len;
    struct sockaddr_storage paddr;
    char ipstr[INET6_ADDRSTRLEN];
    int pport, peer;

    len = sizeof(paddr);
    peer = getpeername(client, (struct sockaddr*)&paddr, &len);

    // deal with both IPv4 and IPv6:
    if (paddr.ss_family == AF_INET) {
        struct sockaddr_in *s = (struct sockaddr_in *)&paddr;
        pport = ntohs(s->sin_port);
        inet_ntop(AF_INET, &s->sin_addr, ipstr, sizeof(ipstr));
    } else { // AF_INET6
        struct sockaddr_in6 *s = (struct sockaddr_in6 *)&paddr;
        pport = ntohs(s->sin6_port);
        inet_ntop(AF_INET6, &s->sin6_addr, ipstr, sizeof(ipstr));
    }

    if (peer == -1) {
        hm_log(GCLOG_WARNING, cs->log, "Couldn't retrieve peer name");
    }
#endif

    cc = hm_palloc(cs->pool, sizeof(struct gc_gen_client_s));
    if (cc == NULL) {
        return;
    }

    memset(cc, 0, sizeof(struct gc_gen_client_s));

    cc->base.loop = loop;
    cc->base.fd = client;
    cc->base.pool = cs->pool;
    cc->base.log = cs->log;
    cc->base.read.data = cc;
    cc->base.write.data = cc;
    cc->base.tunnel = cs->tunnel;
    cc->parent = cs;
    cc->callback.error = client_error;
#ifdef PEER_NAME
    gcsn_initz(snip, ipstr);
    gcsnb_cpy_ds(cc->base.net.ip, snip);
    cc->base.net.port = pport;
#endif
    cc->base.reqidx = 0;
    cc->base.packets = ht_init(cs->pool);

    if (connector_addclient(cs, cc) != GC_OK) {
        hm_pfree(cs->pool, cc);
        return;
    }

    cc->callback.data = cs->callback.data;
    async_client_accept(cc);
}

int async_server(struct gc_gen_server_s *cs, void *tunnel)
{
    struct addrinfo *ai, hints;
    memset(&hints, 0, sizeof hints);
    hints.ai_family = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_flags = AI_PASSIVE | AI_ADDRCONFIG;
    const int gai_err = getaddrinfo(cs->host, cs->port, &hints, &ai);

    if (gai_err != GC_OK) {
        hm_log(GCLOG_CRIT, cs->log, "Server get address info failed with [%s] %s:%s", gai_strerror(gai_err), cs->host, cs->port);
        return GC_ERROR;
    }

    cs->fd = socket(ai->ai_family, SOCK_STREAM, IPPROTO_TCP);
    if (cs->fd == -1) {
        hm_log(GCLOG_CRIT, cs->log, "Server socket() initialization failed");
        return GC_ERROR;
    }

    int ret = gc_fd_setnonblock(cs->fd);
    if (ret != GC_OK) {
        hm_log(GCLOG_TRACE, cs->log, "Failed to set nonblock() on fd %d", cs->fd);
    }

#ifdef GC_SO_REUSEADDR
    int reuseaddr = 1;
    setsockopt(cs->fd, SOL_SOCKET, SO_REUSEADDR, &reuseaddr, sizeof(reuseaddr));
#endif
#ifdef GC_SO_REUSEPORT
    int reuseport = 1;
    setsockopt(cs->fd, SOL_SOCKET, SO_REUSEPORT, &reuseport, sizeof(reuseport));
#endif

    if (bind(cs->fd, ai->ai_addr, ai->ai_addrlen)) {
        hm_log(GCLOG_CRIT, cs->log, "Server bind() failed [%s:%s]", cs->host, cs->port);
        return GC_ERROR;
    }

#ifdef GC_TCP_DEFER_ACCEPT
    int timeout = 1;
    setsockopt(cs->fd, IPPROTO_TCP, TCP_DEFER_ACCEPT, &timeout, sizeof(timeout));
#endif

    freeaddrinfo(ai);
    listen(cs->fd, GC_DEFAULT_BACKLOG);

    struct sockaddr_in sin;
    socklen_t len = sizeof(sin);
    if (getsockname(cs->fd, (struct sockaddr *)&sin, &len) == -1) {
        hm_log(GCLOG_CRIT, cs->log, "Server getsockname() failed");
        return GC_ERROR;
    }

    if (strcmp(cs->port, "0") == 0)
        cs->port_local = ntohs(sin.sin_port);
    else
        cs->port_local = atoi(cs->port);

    ev_io_init(&cs->listener, server_async_client, cs->fd, EV_READ);
    cs->listener.data = cs;
    ev_io_start(cs->loop, &cs->listener);

    cs->clients = ht_init(cs->pool);
    if (!cs->clients) {
        hm_log(GCLOG_CRIT, cs->log, "Hashtable failed to initialize");
        return GC_ERROR;
    }

    hm_log(GCLOG_TRACE, cs->log, "Opening async server on %s:%d fd: %d %p",
                               cs->host, cs->port_local, cs->fd, cs);

    cs->tunnel = tunnel;

    return GC_OK;
}

static void client_rem(void *v, int nv, void *ud)
{
    struct gc_gen_client_s *c = (struct gc_gen_client_s *)v;
    if (c) async_client_shutdown(c);
}

void async_server_shutdown(struct gc_gen_server_s *s)
{
    assert(s);
    struct hm_pool_s *p = s->pool;

    ev_io_stop(s->loop, &s->listener);

    (void )gc_fd_close(s->fd);

    ht_map(s->clients, client_rem, NULL);

    ht_free(s->clients);

    hm_pfree(p, s);
}
