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
                     wp->host, wp->port, 0, 0, NULL));
    free(authstr);
    RSA_free(tmpkey);
    sn_free(strkey);
    return 0;
}

static int peer_find(struct list_s *l, void *existing, void *uwp) {
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
                wp->pubkeyhash, sizeof(wp->pubkeyhash)) &&
        ex->unreachable == 0) {
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

static int peer_broadcast(struct peer_s *p, struct world_peer_s *wp)
{
    if (!p || !wp) return -1;
    if (p->type != INSTANCE_TRACKER) return -1;
    int cb(struct list_s *l, void *existing, void *uwp) {
        struct world_peer_s *ex = (struct world_peer_s *)existing;
        struct world_peer_s *wp = (struct world_peer_s *)uwp;
        if ((wp->host == ex->host &&
            wp->port == ex->port) ||
            wp->authed == false) {
            return 0;
        }
        int item(int dst, unsigned short dstport,
                 int src, unsigned short srcport,
                 sn *key) {
            p->send_buffer.type = BUFFER_TRACKER_ANNOUNCE_PEER;
            p->send_buffer.u.tracker_peer.key  = key;
            p->send_buffer.u.tracker_peer.host = src;
            p->send_buffer.u.tracker_peer.port = srcport;
            if (payload.send(p, COMMAND_TRACKER_ANNOUNCE_PEER,
                             dst, dstport, 0, 0, NULL) != 0) return -1;
            return 0;
        }
        ifr(item(wp->host, wp->port, ex->host, ex->port, &ex->pubkey));
        ifr(item(ex->host, ex->port, wp->host, wp->port, &wp->pubkey));
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
    if (wp.found && wp.found->unreachable == 0) *reachable = true;
    else                                        *reachable = false;
    return 0;
}

static int reachable_set(struct peer_s *p, int host, unsigned short port,
                         bool reachable)
{
    struct world_peer_s wp = { .host = host, .port = port, .found = NULL };
    ifr(list.map(&p->peers, peer_find, &wp));
    if (wp.found) {
        if (!reachable && ++wp.found->unreachable == p->cfg.net.max.peer_unreachable) {
            if (p->user.cb.offline && p->user.cb.offline(p, wp.found) != 0) return -1;
            ifr(list.del(&p->peers, wp.found));
        } else if (reachable) wp.found->unreachable = 0;
    }
    return 0;
}

static int peer_unreachable(struct peer_s *p, int host, unsigned short port)
{
    return reachable_set(p, host, port, false);
}

static int peer_reachable(struct peer_s *p, int host, unsigned short port)
{
    return reachable_set(p, host, port, true);
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
        return payload.send(p, COMMAND_PING,
                            wp->host,
                            wp->port, 0, 0, NULL);
    }
    if (list.map(&p->peers, cb, p) != 0)
        syslog(LOG_ERR, "Peers check failed");
    ev_timer_again(p->ev.loop, &p->ev.peers_reachable);
}

struct peer_root_s {
    const char    *transaction_name;
    unsigned char *pubkeyhash;
    struct file_s *file;
};

const struct module_world_s world = {
    .peer.reachable      = peer_reachable,
    .peer.unreachable    = peer_unreachable,
    .peer.check          = peer_check,
    .peer.isreachable    = peer_isreachable,
    .peer.find           = peer_find,
    .peer.findpubkeyhash = peer_findpubkeyhash,
    .peer.findauthstr    = peer_findauthstr,
    .peer.broadcast      = peer_broadcast,
    .peer.auth           = peer_auth,
};
