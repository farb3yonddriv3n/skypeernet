#ifndef PACKET_H_
#define PACKET_H_

#define UDP_PACKET         534
#define UDP_PACKET_PAYLOAD (UDP_PACKET - sizeof(struct header_s) - SHA256HEX)

enum command_e {
    COMMAND_NONE,
    COMMAND_PEER_ANNOUNCE_PEER,
    COMMAND_TRACKER_ANNOUNCE_PEER,
    COMMAND_MESSAGE,
};

struct header_s {
    unsigned int   index;
    unsigned int   sequence;
    unsigned int   total;
    unsigned int   length;
    enum command_e command;
};

struct packet_s {
    struct header_s header;
    struct {
        char        payload[UDP_PACKET_PAYLOAD];
        char        hash[SHA256HEX];
    } buffer;
};

struct module_packet_s {
    int (*validate)(char *buffer, size_t nbuffer, bool *valid, struct packet_s *p);
    int (*clean)(struct packet_s *packets);
    struct {
        int (*init)(enum command_e, char *buffer, size_t nbuffer,
                    struct packet_s **packets, int *npackets, int index);
        int (*validate)(struct packet_s *packets, size_t nbuffer, bool *valid);
    } serialize;
    struct {
        int (*init)(char *buffer, size_t nbuffer, bool *valid);
    } deserialize;
};

extern const struct module_packet_s packet;

#endif
