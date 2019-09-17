#include <common.h>

static void tunnel_data(struct gc_gen_client_s *client, char *buf, int len)
{
    struct tunnel_s *t = (struct tunnel_s *)client->base.tunnel;
    struct tcp_s tcp = { .port.src = t->tcp.src,
                         .port.dst = t->tcp.dst,
                         .cidx     = client->base.fd,
                         .reqtype  = TCP_REQUEST };
    unsigned char filename[SHA256HEX];
    sha256hex((unsigned char *)buf, len, filename);
    char filenamestr[256];
    snprintf(filenamestr, sizeof(filenamestr), "%s/%.*s", t->peer->cfg.dir.tcp,
                                                          SHA256HEX, filename);
    os.filewrite(filenamestr, "wb", buf, len);
    int ret = task.add(t->peer, t->peer->cfg.dir.tcp, filename, sizeof(filename),
                    t->remote.host, t->remote.port, NULL,
                    TASK_FILE_DELETE, &tcp);
}

static int find(struct list_s *l, void *ex, void *ud)
{
    if (!l || !ex) return -1;
    struct tunnel_s *tunnel = (struct tunnel_s *)ex;
    struct world_peer_s *wp = (struct world_peer_s *)ud;
    if (wp->host == tunnel->remote.host &&
        wp->port == tunnel->remote.port) {
        wp->found = wp;
        return 1;
    }
    return 0;
}
// replace this with "queue" and keep only last 100-ish packets
static int packet_sent(struct ht_s **ht, int pidx, bool *sent)
{
    if (!ht || !sent) return 0;
    *sent = false;
    char key[32];
    snprintf(key, sizeof(key), "%d", pidx);
    struct ht_s *kv = ht_get(ht, key, strlen(key));
    if (!kv) return 0;
    *sent = true;
    return 0;
}

static int response(struct peer_s *p, struct header_s *h, char *buf, int len)
{
    struct client_find_s {
        unsigned short         src;
        int                    cidx;
        struct gc_gen_client_s *client;
    };
    int client_find(struct list_s *l, void *ex, void *ud) {
        struct tunnel_s *t = (struct tunnel_s *)ex;
        struct client_find_s *cf = (struct client_find_s *)ud;
        if (t->tcp.src != cf->src) return 0;
        char key[16];
        snprintf(key, sizeof(key), "%d", cf->cidx);
        struct ht_s *kv = ht_get(t->server->clients, key, strlen(key));
        if (!kv) return 0;
        cf->client = (struct gc_gen_client_s *)kv->v;
        return 1;
    }
    struct client_find_s cf = { .src    = h->tcp.port.src,
                                .cidx   = h->tcp.cidx,
                                .client = NULL };
    ifr(list.map(&p->tcp.tunnels, client_find, &cf));
    if (!cf.client) return 0;
    bool sent;
    ifr(packet_sent(cf.client->base.packets, h->pidx, &sent));
    if (sent) return 0;
    char key[32];
    snprintf(key, sizeof(key), "%d", h->pidx);
    HT_ADD_WA(cf.client->base.packets, key, strlen(key), key, strlen(key));
    gc_gen_ev_send(cf.client, buf, len);
    return 0;
}

static int clean(void *ud)
{
    if (!ud) return -1;
    struct tunnel_s *t = (struct tunnel_s *)ud;
    async_server_shutdown(t->server);
    free(t);
    return 0;
}

static int tunnel_open(struct peer_s *p, unsigned char *pubkeyhash,
                       unsigned short *port_local,
                       unsigned short dstport)
{
    if (!p || !pubkeyhash) return -1;
    struct world_peer_s wp = { .found = NULL };
    memcpy(wp.pubkeyhash, pubkeyhash, sizeof(wp.pubkeyhash));
    ifr(list.map(&p->peers, world.peer.findpubkeyhash, &wp));
    if (!wp.found) return -1;
    wp.host = wp.found->host;
    wp.port = wp.found->port;
    wp.found = NULL;
    ifr(list.map(&p->tcp.tunnels, find, &wp));
    if (wp.found) return 0; // tunnel already opened
    struct tunnel_s *t;
    t = malloc(sizeof(*t));
    if (!t) return -1;
    memset(t, 0, sizeof(*t));
    struct gc_gen_server_s *server = malloc(sizeof(*server));
    memset(server, 0, sizeof(*server));
    t->peer   = p;
    t->server = server;
    server->loop = p->ev.loop;
    server->log  = &p->log;
    server->pool = NULL;
    server->callback.data = tunnel_data;
    server->host = "0.0.0.0";
    server->port = "0";
    t->remote.host = wp.host;
    t->remote.port = wp.port;
    t->tcp.dst     = dstport;
    memcpy(t->remote.pubkeyhash, pubkeyhash, sizeof(t->remote.pubkeyhash));
    int ret;
    ret = async_server(server, t);
    if (ret != GC_OK) {
        free(t);
        return ret;
    }
    *port_local = t->tcp.src = server->port_local;
    ifr(list.add(&p->tcp.tunnels, t, clean));
    return 0;
}

const struct module_tunnel_s tunnel = {
    .open     = tunnel_open,
    .response = response
};
