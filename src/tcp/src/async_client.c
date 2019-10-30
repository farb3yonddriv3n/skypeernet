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

static int hostname_to_ip(char *hostname, char *ip)
{
    struct hostent *he;
    struct in_addr **addr_list;
    int i;

    if ((he = gethostbyname(hostname)) == NULL) {
        return 1;
    }

    addr_list = (struct in_addr **)he->h_addr_list;

    for (i = 0; addr_list[i] != NULL; i++) {
        strcpy(ip, inet_ntoa(*addr_list[i]));
        return 0;
    }

    return 1;
}

static void recv_append_client(struct gc_gen_client_s *c)
{
    int sz;
    char *next;

    next = gc_ringbuffer_recv_read(&c->base.rb, &sz);
    c->callback.data(c, next, sz);
    gc_ringbuffer_recv_pop(c->base.pool, &c->base.rb);
}

static void async_read(struct ev_loop *loop, ev_io *w, int revents)
{
    (void) revents;
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

    //hm_log(GCLOG_TRACE, c->base.log, "Received %d bytes from fd %d", sz, fd);

    if (sz > 0) {
        gc_ringbuffer_recv_append(c->base.pool, &c->base.rb, sz);

        if (gc_ringbuffer_recv_is_full(&c->base.rb)) {
            ev_io_stop(c->base.loop, &c->base.read);
            if (c->callback.error) {
                c->callback.error(c, GC_READRBFULL_ERR);
            }
            return;
        }
        recv_append_client(c);
    } else if (sz == 0) {
        //async_handle_socket_errno(c->base.log);
        if (c->callback.error) {
            c->callback.error(c, GC_READZERO_ERR);
        }
    } else if (sz == -1 &&
        (errno == EAGAIN || errno == EWOULDBLOCK || errno == EINTR)) {
        hm_log(GCLOG_TRACE, c->base.log, "Socket read EAGAIN|EWOULDBLOCK|EINTR");
        async_handle_socket_errno(c->base.log);
    } else {
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

    //hm_log(GCLOG_TRACE, c->base.log, "%d bytes sent to fd %d", sz, fd);

    if (sz > 0) {
        gc_ringbuffer_send_skip(c->base.pool, &c->base.rb, sz);
        if (gc_ringbuffer_send_is_empty(&c->base.rb)) {
            ev_io_stop(loop, &c->base.write);
        }
    } else {
        async_handle_socket_errno(c->base.log);
        ev_io_stop(loop, &c->base.write);
        if (c->callback.error) {
            c->callback.error(c, GC_WRITE_ERR);
        }
    }
}

int async_client(struct gc_gen_client_s *client)
{
    struct sockaddr_in servaddr;
    char ip[32];

    assert(client);

    client->base.fd = socket(AF_INET, SOCK_STREAM, 0);
    if (client->base.fd == -1) {
        hm_log(GCLOG_ERR, client->base.log, "{Connector}: client init socket error: %d", errno);
        client->callback.error(client, GC_SOCKET_ERR);
        return GC_ERROR;
    }

    snprintf(ip, sizeof(ip), "%.*s", gcsn_p(client->base.net.ip));

    int ret = gc_fd_setkeepalive(client->base.fd);
    if (ret != GC_OK) {
        hm_log(GCLOG_TRACE, client->base.log, "Failed to set keepalive() on fd %d", client->base.fd);
    }

    ret = gc_fd_setnonblock(client->base.fd);
    if (ret != GC_OK) {
        hm_log(GCLOG_TRACE, client->base.log, "Failed to set nonblock() on fd %d", client->base.fd);
    }

    memset(&servaddr, 0, sizeof(servaddr));
    servaddr.sin_family = AF_INET;
    servaddr.sin_addr.s_addr = inet_addr(ip);
    servaddr.sin_port = htons(client->base.net.port);

    ev_io_init(&client->base.write, async_write, client->base.fd, EV_WRITE);
    ev_io_init(&client->base.read, async_read, client->base.fd, EV_READ);

    client->base.read.data = client;
    client->base.write.data = client;
    gc_timestring(client->base.date, sizeof(client->base.date));

    client->base.packets = ht_init(client->base.pool);

    ev_io_start(client->base.loop, &client->base.read);
    if (connect(client->base.fd, (struct sockaddr *)&servaddr, sizeof(servaddr)) != -1
       && errno != EINPROGRESS) {
        async_client_shutdown(client);
        hm_log(GCLOG_ERR, client->base.log, "{Connector}: connect() errno: %d", errno);
        return GC_ERROR;
    }

    hm_log(GCLOG_DEBUG, client->base.log, "Adding endpoint TCP client [%.*s:%d] fd: [%d]",
                                        gcsn_p(client->base.net.ip), client->base.net.port,
                                        client->base.fd);
    return GC_OK;
}
