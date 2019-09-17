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
    async_client_shutdown(ep->client);
    free(ep);
    return 0;
}

static int request(struct peer_s *p, struct header_s *header, int host,
                   unsigned short port, char *data, int ndata)
{
    if (!p || !header || !data) return -1;
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
    return 0;
}

struct module_endpoint_s endpoint = {
    .request = request
};
