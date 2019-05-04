#include <common.h>

#define UDP_PACKET_PAYLOAD (534 - sizeof(struct header_s) - SHA256HEX)

struct header_s {
    unsigned int index;
    unsigned int sequence;
    unsigned int total;
    unsigned int length;
};

struct packet_s {
    struct header_s header;
    char            payload[UDP_PACKET_PAYLOAD];
    char            hash[SHA256HEX];
};

static int packet_create(char *buffer, int nbuffer, struct packet_s **packets,
                         int *npackets, int index, int sequence, int total)
{
    if (!packets || !npackets) return -1;
    (*npackets)++;
    *packets = realloc(*packets, sizeof(**packets) * (*npackets));
    if (!(*packets)) return -1;
    struct packet_s *p = &(*packets)[*npackets - 1];
    memset(p, 0, sizeof(*p));
    p->header.index    = index;
    p->header.sequence = sequence;
    p->header.total    = total;
    p->header.length   = nbuffer;
    memcpy(p->payload, buffer, nbuffer);
    unsigned char md[SHA256HEX];
    sha256hex((unsigned char *)p, sizeof(struct header_s) + nbuffer, md);
    memcpy(p->hash, md, SHA256HEX);
    return 0;
}

static int chunk(char *buffer, size_t nbuffer, struct packet_s **packets,
                 int *npackets)
{
    int i;
    int chunks    = nbuffer / UDP_PACKET_PAYLOAD;
    int remaining = nbuffer % UDP_PACKET_PAYLOAD;
    if (remaining > 0) chunks++;
    for (i = 0; i < chunks; i++) {
        if (packet_create(buffer + (i * UDP_PACKET_PAYLOAD), UDP_PACKET_PAYLOAD,
                          packets, npackets, 0, i, chunks)) return -1;
        if (i + 1 == chunks)
            if (packet_create(buffer + (i * UDP_PACKET_PAYLOAD), remaining,
                             packets, npackets, 0, i, chunks) != 0) return -1;
    }
    return 0;
}

static int send(char *buffer, size_t nbuffer)
{
    if (!buffer || nbuffer < 1) return -1;
    struct packet_s *packets = NULL;
    int npackets = 0;
    if (chunk(buffer, nbuffer, &packets, &npackets) != 0) return -1;

    int i;
    for (i = 0; i < npackets; i++) {
        if (packet.receive((char *)(&packets[i]), sizeof(packets[i])) != 0)
            return -1;
    }
    return 0;
}

static int validate(char *buffer, size_t nbuffer, bool *valid)
{
    if (!buffer || nbuffer < 1) return -1;
    *valid = false;
    if (nbuffer > sizeof(struct packet_s)) return -1;
    struct packet_s *p = (struct packet_s *)buffer;
    if (p->header.length > UDP_PACKET_PAYLOAD) return -1;
    unsigned char md[SHA256HEX];
    sha256hex((unsigned char *)p, sizeof(struct header_s) + p->header.length, md);
    if (memcmp(md, p->hash, sizeof(p->hash)) != 0) return -1;
    *valid = true;
    return 0;
}

static int receive(char *buffer, size_t nbuffer)
{
    bool valid;
    if (validate(buffer, nbuffer, &valid) != 0) return -1;
    return 0;
}

const struct module_packet_s packet = {
    .send    = send,
    .receive = receive,
};
