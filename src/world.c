#include <common.h>

#define PUBKEY_TMP ".pubkey.tmp"

static int peer_auth(struct peer_s *p, struct world_peer_s *wp)
{
    if (!p || !wp) return -1;
    double timems;
    ifr(os.gettimems(&timems));
    char buffer[2048];
    snprintf(buffer, sizeof(buffer), "%f%.*s%f", timems,
            sn_p(p->cfg.keys.local.str.private),
            timems);
    sha256hex((unsigned char *)buffer, strlen(buffer), wp->authstr);
    ifr(eioie_fwrite(PUBKEY_TMP, "w", wp->pubkey.s, wp->pubkey.n));
    RSA *tmpkey;
    sn strkey;
    unsigned char hash[SHA256HEX];
    ifr(rsa_loadkey(PEM_read_RSAPublicKey, PUBKEY_TMP,
                    &tmpkey, &strkey, hash));
    unsigned char *authstr;
    int            nauthstr;
    ifr(rsa_encrypt(tmpkey, wp->authstr, sizeof(wp->authstr),
                    &authstr, &nauthstr));
    p->send_buffer.type = BUFFER_AUTH;
    sn_setr(p->send_buffer.u.auth.str, (char *)authstr, nauthstr);
    ifr(payload.send(p, COMMAND_AUTH,
                     wp->host, wp->port, 0, 0, NULL, NULL));
    free(authstr);
    RSA_free(tmpkey);
    sn_free(strkey);
    return 0;
}

static int peer_find(struct list_s *l, void *existing, void *uwp)
{
    struct world_peer_s *ex = (struct world_peer_s *)existing;
    struct world_peer_s *wp = (struct world_peer_s *)uwp;
    if (wp->host == ex->host &&
        wp->port == ex->port) {
        wp->found = ex;
        return 1;
    }
    return 0;
}

static int peer_findpubkeyhash(struct list_s *l, void *existing, void *ud)
{
    struct world_peer_s *ex = (struct world_peer_s *)existing;
    struct world_peer_s *wp = (struct world_peer_s *)ud;
    if (dmemcmp(ex->pubkeyhash, sizeof(ex->pubkeyhash),
                wp->pubkeyhash, sizeof(wp->pubkeyhash))) {
        wp->found = ex;
        return 1;
    }
    return 0;
}

static int peer_findauthstr(struct list_s *l, void *existing, void *ud)
{
    struct world_peer_s *ex = (struct world_peer_s *)existing;
    struct world_peer_s *wp = (struct world_peer_s *)ud;
    if (dmemcmp(ex->authstr, sizeof(ex->authstr),
                wp->authstr, sizeof(wp->authstr))) {
        wp->found = ex;
        return 1;
    }
    return 0;
}

static int peer_findproxy(struct list_s *l, void *existing, void *ud)
{
    struct world_peer_s *ex = (struct world_peer_s *)existing;
    struct world_peer_s *wp = (struct world_peer_s *)ud;
    if (ex->flags & WORLD_PEER_PROXY) {
        wp->found = ex;
        return 1;
    }
    return 0;
}

static int peer_broadcast(struct peer_s *p, struct world_peer_s *wp)
{
    if (!p || !wp) return -1;
    if (p->type != INSTANCE_TRACKER) return -1;
    int cb(struct list_s *l, void *existing, void *uwp) {
        struct world_peer_s *ex = (struct world_peer_s *)existing;
        struct world_peer_s *wp = (struct world_peer_s *)uwp;
        if ((wp->host == ex->host &&
            wp->port == ex->port) ||
            !(wp->flags & WORLD_PEER_AUTHED)) {
            return 0;
        }
        int item(int dst, unsigned short dstport,
                 int src, unsigned short srcport,
                 sn *key, struct list_s *tcpports,
                 char *tcpdesc, int version, unsigned int flags) {
            p->send_buffer.type = BUFFER_TRACKER_ANNOUNCE_PEER;
            p->send_buffer.u.tracker_peer.key  = key;
            p->send_buffer.u.tracker_peer.host = src;
            p->send_buffer.u.tracker_peer.port = srcport;
            p->send_buffer.u.tracker_peer.proxy = (flags & WORLD_PEER_PROXY);
            sn_setr(p->send_buffer.u.tracker_peer.tcpdesc,
                    tcpdesc, strlen(tcpdesc));
            p->send_buffer.u.tracker_peer.version = version;
            p->send_buffer.u.tracker_peer.tcpports = tcpports;
            if (payload.send(p, COMMAND_TRACKER_ANNOUNCE_PEER,
                             dst, dstport, 0, 0, NULL, NULL) != 0) return -1;
            return 0;
        }
        ifr(item(wp->host, wp->port, ex->host, ex->port, &ex->pubkey,
                 &ex->tcp.ports, ex->tcp.description, ex->version,
                 ex->flags));
        ifr(item(ex->host, ex->port, wp->host, wp->port, &wp->pubkey,
                 &wp->tcp.ports, wp->tcp.description, wp->version,
                 wp->flags));
        return 0;
    }
    ifr(list.map(&p->peers, cb, wp));
    return 0;
}

static int peer_isreachable(struct peer_s *p, int host, unsigned short port,
                            bool *reachable)
{
    if (!p || !reachable) return -1;
    struct world_peer_s wp = { .host  = host, .port  = port, .found = NULL };
    ifr(list.map(&p->peers, peer_find, &wp));
    if (wp.found) *reachable = true;
    else          *reachable = false;
    return 0;
}

static int peer_reachable(struct peer_s *p, int host, unsigned short port)
{
    struct world_peer_s wp = { .host = host, .port = port, .found = NULL };
    ifr(list.map(&p->peers, peer_find, &wp));
    if (!wp.found) return 0;
    wp.found->unreachable = 0;
    return 0;
}

static int peer_offline_shadow(struct peer_s *p, struct world_peer_s *wp)
{
    if (!p || !wp) return -1;
    if (p->type == INSTANCE_PEER) {
        wp->flags |= WORLD_PEER_QUERIED;
        p->send_buffer.type = BUFFER_QUERY;
        p->send_buffer.u.query.host = wp->host;
        p->send_buffer.u.query.port = wp->port;
        return payload.send(p, COMMAND_QUERY,
                            p->tracker.host, p->tracker.port,
                            0, 0, NULL, NULL);
    } else {
        ifr(p->user.cb.offline && p->user.cb.offline(p, wp));
        return list.del(&p->peers, wp);
    }
}

static void peer_check(struct ev_loop *loop, struct ev_timer *timer, int revents)
{
    struct peer_s *p = (struct peer_s *)timer->data;
    if (!p) return;
    ev_timer_stop(p->ev.loop, &p->ev.peers_reachable);
    int cb(struct list_s *l, void *un, void *ud) {
        struct peer_s       *p  = (struct peer_s *)ud;
        struct world_peer_s *wp = (struct world_peer_s *)un;
        syslog(LOG_DEBUG, "Checking peer's availability: %x:%d", wp->host, wp->port);
        if (++wp->unreachable >= p->cfg.net.max.peer_unreachable) {
            return peer_offline_shadow(p, wp);
        } else {
            return payload.send(p, COMMAND_PING,
                                wp->host,
                                wp->port, 0, 0, NULL, NULL);
        }
    }
    if (list.map(&p->peers, cb, p) != 0)
        syslog(LOG_ERR, "Peers check failed");
    ev_timer_again(p->ev.loop, &p->ev.peers_reachable);
}

static int wp_clean(void *uwp)
{
    if (!uwp) return -1;
    struct world_peer_s *wp = (struct world_peer_s *)uwp;
    if (wp->pubkey.s) free(wp->pubkey.s);
    ifr(list.clean(&wp->tcp.ports));
    free(wp);
    return 0;
}

static int peer_add(struct peer_s *p, struct world_peer_s *wp,
                    bool *added)
{
    if (!p || !wp || !added) return -1;
    wp->found = NULL;
    *added = false;
    ifr(list.map(&p->peers, world.peer.find, wp));
    if (wp->found) return wp_clean(wp);
    ifr(list.add(&p->peers, wp, wp_clean));
    *added = true;
    return 0;
}

static int peer_shadow(struct peer_s *p, int *host, unsigned short *port)
{
    if (!p || !host || !port) return -1;
    struct world_peer_s wp = { .host = *host, .port = *port, .found = NULL };
    ifr(list.map(&p->peers, peer_find, &wp));
    if (!wp.found) return 0;
    wp.found->stats.sent++;
    if (wp.found->flags & WORLD_PEER_SHADOW) {
        struct world_peer_s proxy = { .found = NULL };
        ifr(list.map(&p->peers, peer_findproxy, &proxy));
        if (!proxy.found) return 0;
        *host = proxy.found->host;
        *port = proxy.found->port;
    }
    return 0;
}

const struct module_world_s world = {
    .peer.reachable      = peer_reachable,
    .peer.check          = peer_check,
    .peer.isreachable    = peer_isreachable,
    .peer.find           = peer_find,
    .peer.findpubkeyhash = peer_findpubkeyhash,
    .peer.findauthstr    = peer_findauthstr,
    .peer.findproxy      = peer_findproxy,
    .peer.broadcast      = peer_broadcast,
    .peer.auth           = peer_auth,
    .peer.add            = peer_add,
    .peer.shadow         = peer_shadow,
};
