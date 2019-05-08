#include <common.h>

static int packet_create(enum command_e command, char *buffer, int nbuffer,
                         struct packet_s **packets, int *npackets, int index,
                         int sequence, int total)
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
    p->header.command  = command;
    memcpy(p->buffer.payload, buffer, nbuffer);
    unsigned char md[SHA256HEX];
    sha256hex((unsigned char *)p, sizeof(struct header_s) + nbuffer, md);
    memcpy(p->buffer.hash, md, SHA256HEX);
    return 0;
}

static int chunk(enum command_e command, char *buffer, size_t nbuffer,
                 struct packet_s **packets, int *npackets, int index)
{
    int i;
    int chunks    = nbuffer / UDP_PACKET_PAYLOAD;
    int remaining = nbuffer % UDP_PACKET_PAYLOAD;
    if (remaining > 0) chunks++;
    for (i = 0; i < chunks; i++) {
        if (i + 1 == chunks && remaining > 0) {
            if (packet_create(command, buffer + (i * UDP_PACKET_PAYLOAD), remaining,
                              packets, npackets, index, i, chunks) != 0) return -1;
        } else {
            if (packet_create(command, buffer + (i * UDP_PACKET_PAYLOAD), UDP_PACKET_PAYLOAD,
                              packets, npackets, index, i, chunks) != 0) return -1;
        }
    }
    return 0;
}

static int serialize_init(enum command_e command, char *buffer, size_t nbuffer,
                          struct packet_s **packets, int *npackets, int index)
{
    if (!buffer || nbuffer < 1) return -1;
    *packets = NULL;
    *npackets = 0;
    if (chunk(command, buffer, nbuffer, packets, npackets, index) != 0) return -1;
    return 0;
}

static int clean(struct packet_s *packets)
{
    if (!packets) return -1;
    free(packets);
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

static int validate(char *buffer, size_t nbuffer, bool *valid,
                    struct packet_s *p)
{
    if (!buffer || nbuffer < 1) return -1;
    *valid = false;

    if (nbuffer > sizeof(struct packet_s)) return 0;
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

const struct module_packet_s packet = {
    .validate           = validate,
    .clean              = clean,
    .serialize.init     = serialize_init,
    .serialize.validate = serialize_validate,
    .deserialize.init   = deserialize_init,
};
