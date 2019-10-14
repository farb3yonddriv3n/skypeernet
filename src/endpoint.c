#include <common.h>

static void response(struct gc_gen_client_s *client, char *buf, int len)
{
    if (!client || !buf) return;
    struct endpoint_s *ep = client->base.endpoint;
    if (!ep) return;
    struct tcp_s tcp = { .port.src = ep->tcp.src,
                         .port.dst = ep->tcp.dst,
                         .cidx     = ep->tcp.cidx,
                         .reqtype  = TCP_RESPONSE };
    unsigned char filename[SHA256HEX];
    sha256hex((unsigned char *)buf, len, filename);
    char filenamestr[256];
    snprintf(filenamestr, sizeof(filenamestr), "%s/%.*s", ep->peer->cfg.dir.tcp,
                                                          SHA256HEX, filename);
    os.filewrite(filenamestr, "wb", buf, len);
    (void )task.add(ep->peer, ep->peer->cfg.dir.tcp, filename, sizeof(filename),
                    ep->remote.host, ep->remote.port, NULL,
                    TASK_FILE_DELETE, &tcp);
}

static void response_error(struct gc_gen_client_s *c, enum gcerr_e error)
{
    hm_log(GCLOG_TRACE, c->base.log, "Client error %d on fd %d, endpoint %p",
                                     error, c->base.fd, c);
    struct endpoint_s *ep = c->base.endpoint;
    (void )list.del(&ep->peer->tcp.endpoints, ep);
}

static int clean(void *ud)
{
    if (!ud) return -1;
    struct endpoint_s *ep = (struct endpoint_s *)ud;
    ifr(api_endpoint_change(ep->peer, ep->tcp.dst, ep->tcp.src, API_ENDPOINTCLOSE));
    async_client_shutdown(ep->client);
    free(ep);
    return 0;
}

struct allowed_s {
    int port;
    struct allowed_s *found;
};

static int request(struct peer_s *p, struct header_s *header, int host,
                   unsigned short port, char *data, int ndata)
{
    if (!p || !header || !data) return -1;
    struct allowed_s ap = { .port  = header->tcp.port.dst,
                            .found = NULL };
    LMAP(allowed, allowed_s)
        LCOND(dst->port == src->port)
    LMAPE
    ifr(list.map(&p->cfg.tcp.ports, allowed, &ap));
    if (!ap.found) {
        char msg[256];
        snprintf(msg, sizeof(msg), "Port %d not allowed on peer %.*s..",
                                   header->tcp.port.dst,
                                   10,
                                   p->cfg.keys.local.hash.public);
        SEND_MSG(p, host, port, msg)
        return 0;
    }
    struct endpoint_s fnep = { .remote.host = host,
                               .remote.port = port,
                               .tcp.cidx    = header->tcp.cidx,
                               .tcp.src     = header->tcp.port.src,
                               .tcp.dst     = header->tcp.port.dst,
                               .found       = NULL };
    int find(struct list_s *l, void *ex, void *ud) {
        if (!l || !ex) return -1;
        struct endpoint_s *lep = (struct endpoint_s *)ex;
        struct endpoint_s *rep  = (struct endpoint_s *)ud;
        if (lep->remote.host == rep->remote.host &&
            lep->remote.port == rep->remote.port &&
            lep->tcp.cidx == rep->tcp.cidx &&
            lep->tcp.src == rep->tcp.src &&
            lep->tcp.dst == rep->tcp.dst) {
            rep->found = lep;
            return 1;
        }
        return 0;
    }
    ifr(list.map(&p->tcp.endpoints, find, &fnep));
    if (fnep.found) {
        gc_gen_ev_send(fnep.found->client, data, ndata);
        return 0;
    }

    struct endpoint_s *ep = malloc(sizeof(*ep));
    if (!ep) return -1;
    memcpy(ep, &fnep, sizeof(*ep));

    struct gc_gen_client_s *c = malloc(sizeof(*c));
    if (!c) return -1;
    memset(c, 0, sizeof(*c));
    ep->client = c;
    c->base.loop = p->ev.loop;
    c->base.log  = &p->log;
    c->base.pool = NULL;

    sn_initz(ip, "0.0.0.0");
    snb_cpy_ds(c->base.net.ip, ip);
    c->base.net.port = fnep.tcp.dst;

    c->callback.data  = response;
    c->callback.error = response_error;

    c->base.endpoint = ep;
    ep->peer = p;
    int ret = async_client(c);
    if (ret != GC_OK) return ret;

    ifr(list.add(&p->tcp.endpoints, ep, clean));
    gc_gen_ev_send(c, data, ndata);
    return api_endpoint_change(p, fnep.tcp.dst, fnep.tcp.src, API_ENDPOINTOPEN);
}

static int dump(struct peer_s *p, json_object **obj)
{
    int cb(struct list_s *l, void *ut, void *ud) {
        struct endpoint_s  *e = (struct endpoint_s *)ut;
        json_object      *eps = (json_object *)ud;
        json_object *port = json_object_new_int(e->remote.port);
        json_object     *cidx = json_object_new_int(e->tcp.cidx);
        json_object *src_port = json_object_new_int(e->tcp.src);
        json_object *dst_port = json_object_new_int(e->tcp.dst);
        json_object *jt = json_object_new_object();
        char hostbuf[32];
        snprintf(hostbuf, sizeof(hostbuf), "%x", e->remote.host);
        json_object *jhost = json_object_new_string_len(hostbuf, strlen(hostbuf));
        json_object_object_add(jt, "host", jhost);
        json_object_object_add(jt, "port", port);
        json_object_object_add(jt, "cidx", cidx);
        json_object_object_add(jt, "src_port", src_port);
        json_object_object_add(jt, "dst_port", dst_port);
        json_object_array_add(eps, jt);
        return 0;
    }
    if (!p) return -1;
    *obj = json_object_new_object();
    json_object *jep = json_object_new_array();
    json_object_object_add(*obj, "endpoints", jep);
    int count;
    ifr(list.size(&p->tcp.endpoints, &count));
    json_object *jcount = json_object_new_int(count);
    json_object_object_add(*obj, "count", jcount);
    return list.map(&p->tcp.endpoints, cb, jep);
}

struct module_endpoint_s endpoint = {
    .request = request,
    .dump    = dump
};
