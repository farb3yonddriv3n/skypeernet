#ifndef PACKET_H_
#define PACKET_H_

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

struct module_packet_s {
    int (*validate)(char *buffer, size_t nbuffer, bool *valid);
    int (*clean)(struct packet_s *packets);
    struct {
        int (*init)(char *buffer, size_t nbuffer, struct packet_s **packets,
                    int *npackets, int index);
        int (*validate)(struct packet_s *packets, size_t nbuffer, bool *valid);
    } serialize;
    struct {
        int (*init)(char *buffer, size_t nbuffer, bool *valid);
    } deserialize;
};

extern const struct module_packet_s packet;

#endif
