#ifndef PACKET_H_
#define PACKET_H_

#define UDP_PACKET         534
#define UDP_PACKET_PAYLOAD (UDP_PACKET - sizeof(struct header_s) - SHA256HEX)
//#define UDP_PACKET_PAYLOAD 1

enum command_e {
    COMMAND_NONE = 0,
    COMMAND_ACK,
    COMMAND_TRACKER_ANNOUNCE_PEER,
    COMMAND_PEER_ANNOUNCE_PEER,
    COMMAND_MESSAGE,
    COMMAND_FILE,
    COMMAND_FILE_SEND,
};

struct header_s {
    unsigned int   index;
    unsigned int   group;
    uint64_t       sequence;
    unsigned int   total;
    unsigned int   length;
    enum command_e command;
    ALIGN16(4);
};

struct packet_internal_s {
    int            host;
    unsigned short port;
};

struct packet_s {
    struct header_s header;
    struct {
        char payload[UDP_PACKET_PAYLOAD];
        char hash[SHA256HEX];
    } buffer;
    struct packet_internal_s internal;
};

struct send_buffer_s;

struct module_packet_s {
    int (*validate)(char *buffer, size_t nbuffer, bool *valid,
                    int host, unsigned short port, struct packet_s *p);
    int (*clean)(void *p);
    void (*dump)(struct packet_s *p);
    struct {
        int (*init)(enum command_e, char *buffer, int nbuffer,
                    struct packet_s **packets, int *npackets,
                    struct send_buffer_s *sb);
        int (*validate)(struct packet_s *packets, size_t nbuffer, bool *valid);
    } serialize;
    struct {
        int (*init)(char *buffer, size_t nbuffer, bool *valid);
    } deserialize;
};

extern const struct module_packet_s packet;

#endif
