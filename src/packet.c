#include <common.h>

static int packet_create(struct peer_s *pr, enum command_e cmd, char *buffer, int nbuffer,
                         struct packet_s **packets, int *npackets,
                         uint64_t offset, unsigned int chunks, unsigned int *pidx,
                         unsigned int gidx, unsigned int tidx, unsigned int parts,
                         unsigned char *filename, struct tcp_s *tcp,
                         int host, unsigned short port)
{
    if (!packets || !npackets) return -1;
    (*npackets)++;
    *packets = realloc(*packets, sizeof(**packets) * (*npackets));
    if (!(*packets)) return -1;
    struct packet_s *p = &(*packets)[*npackets - 1];
    memset(p, 0, sizeof(*p));
    p->header.pidx    = (*pidx)++;
    p->header.gidx    = gidx;
    p->header.tidx    = tidx;
    p->header.offset  = offset;
    p->header.chunks  = chunks;
    p->header.parts   = parts;
    p->header.length  = nbuffer;
    p->header.command = cmd;
    p->header.dst.host = host;
    p->header.dst.port = port;
    p->header.src.host = ADDR_IP(pr->net.self.addr);
    p->header.src.port = ADDR_PORT(pr->net.self.addr);
    ifr(os.gettimems(&p->header.ts));
    if (tcp) memcpy(&p->header.tcp, tcp, sizeof(*tcp));
    if (filename) memcpy(p->header.filename, filename, sizeof(p->header.filename));
    memcpy(p->buffer.payload, buffer, nbuffer);
    unsigned char md[SHA256HEX];
    sha256hex((unsigned char *)p, sizeof(struct header_s) + nbuffer, md);
    memcpy(p->buffer.hash, md, SHA256HEX);
    return 0;
}

static int chunk(struct peer_s *p, enum command_e command, char *buffer, size_t nbuffer,
                 struct packet_s **packets, int *npackets,
                 unsigned int *pidx, unsigned int gidx,
                 unsigned int tidx, unsigned int parts,
                 unsigned char *filename, struct tcp_s *tcp,
                 int host, unsigned short port)
{
    unsigned int i, rm;
    unsigned int chunks    = nbuffer / UDP_PACKET_PAYLOAD;
    unsigned int remaining = nbuffer % UDP_PACKET_PAYLOAD;
    if (remaining > 0) chunks++;
    for (i = 0; i < chunks; i++) {
        rm = (i + 1 == chunks && remaining > 0) ? remaining : UDP_PACKET_PAYLOAD;
        ifr(packet_create(p, command, buffer + (i * UDP_PACKET_PAYLOAD), rm,
                          packets, npackets, (i * UDP_PACKET_PAYLOAD), chunks, pidx,
                          gidx, tidx, parts, filename, tcp, host, port));
    }
    return 0;
}

static int serialize_init(struct peer_s *p, enum command_e cmd, char *buffer, int nbuffer,
                          struct packet_s **packets, int *npackets,
                          struct send_buffer_s *sb, unsigned int tidx,
                          unsigned int parts, unsigned char *filename,
                          struct tcp_s *tcp, int host, unsigned short port)
{
    if (!p || !buffer || nbuffer < 1) return -1;
    *packets = NULL;
    *npackets = 0;
    ifr(chunk(p, cmd, buffer, nbuffer, packets,
              npackets, &sb->pidx, sb->gidx++,
              tidx, parts, filename, tcp, host, port));
    return 0;
}

static int clean(void *p)
{
    if (!p) return -1;
    free(p);
    return 0;
}

static int serialize_validate(struct packet_s *packets, size_t npackets,
                              bool *valid)
{
    int i;
    *valid = false;
    for (i = 0; i < npackets; i++) {
        struct packet_s p;
        if (packet.validate((char *)(&packets[i]), sizeof(packets[i]),
                            valid, &p) != 0) return -1;
        if (*valid == false) break;
    }
    *valid = true;
    return 0;
}

static int validate(char *buffer, size_t nbuffer, bool *valid, struct packet_s *p)
{
    if (!buffer || nbuffer < 1 || !p) return -1;
    *valid = false;
    //if (nbuffer > sizeof(struct packet_s)) return 0;
    if (data.get(p, buffer, nbuffer) != 0) return -1;
    if (p->header.length > UDP_PACKET_PAYLOAD) return 0;
    unsigned char md[SHA256HEX];
    sha256hex((unsigned char *)p, sizeof(struct header_s) + p->header.length, md);
    if (memcmp(md, p->buffer.hash, SHA256HEX) != 0) return 0;
    *valid = true;
    return 0;
}

static int deserialize_init(char *buffer, size_t nbuffer, bool *valid)
{
    struct packet_s p;
    if (validate(buffer, nbuffer, valid, &p) != 0) return -1;
    return 0;
}

static int tcpsent(struct gc_gen_client_s *c, int tidx, bool *sent)
{
    if (!c || !sent) return -1;
    struct tcpidx_s {
        int  tidx;
        bool *sent;
    };
    struct tcpidx_s ti = { .tidx = tidx, .sent = sent };
    *sent = false;
    int find(struct list_s *l, void *ex, void *ud) {
        struct tcpidx_s *tis = (struct tcpidx_s *)ud;
        if (*(int *)ex == tis->tidx) {
            *tis->sent = true;
            return 1;
        }
        return 0;
    }
    ifr(list.map(&c->base.packets, find, (void *)&ti));
    return 0;
}

static int cleanidx(void *idx)
{
    if (!idx) return -1;
    free(idx);
    return 0;
}

static int tcpsend(struct gc_gen_client_s *c, char *buf, int len, int tidx)
{
    if (!c || !buf) return -1;
    /*
    char key[32];
    snprintf(key, sizeof(key), "%d", tidx);
    HT_ADD_WA(c->base.packets, key, strlen(key), key, strlen(key));
    */
    int *idx = malloc(sizeof(*idx));
    if (!idx) return -1;
    *idx = tidx;
    ifr(list.queue_add(&c->base.packets, idx, cleanidx));
    gc_gen_ev_send(c, buf, len);
    return 0;
}

static void dump(struct packet_s *p)
{
    printf("Index: %d\n",   p->header.pidx);
    printf("Group: %d\n",   p->header.gidx);
    printf("Task: %d\n",    p->header.tidx);
    printf("Offset: %ld\n", p->header.offset);
    printf("Chunks: %d\n",  p->header.chunks);
    printf("Length: %d\n",  p->header.length);
    printf("Command: %d\n", p->header.command);
    printf("Hash: %.*s\n", (int)sizeof(p->buffer.hash), p->buffer.hash);
}

const struct module_packet_s packet = {
    .validate           = validate,
    .clean              = clean,
    .dump               = dump,
    .serialize.init     = serialize_init,
    .serialize.validate = serialize_validate,
    .deserialize.init   = deserialize_init,
    .tcpsent            = tcpsent,
    .tcpsend            = tcpsend
};
