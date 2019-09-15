#ifndef PACKET_H_
#define PACKET_H_

#define UDP_PACKET_SIZE    534
#define UDP_PACKET_PAYLOAD (UDP_PACKET_SIZE - sizeof(struct header_s) - SHA256HEX)
//#define UDP_PACKET_PAYLOAD 1

enum command_e {
    COMMAND_NONE = 0,
    COMMAND_ACK,
    COMMAND_TRACKER_ANNOUNCE_TRACKER,
    COMMAND_TRACKER_ANNOUNCE_PEER,
    COMMAND_PEER_ANNOUNCE_PEER,
    COMMAND_MESSAGE,
    COMMAND_FILE,
    COMMAND_FILEASK,
    COMMAND_AUTH,
    COMMAND_AUTH_REPLY,
    COMMAND_PING,
    COMMAND_TCP,
};

#define TCP_NONE     0
#define TCP_REQUEST  1
#define TCP_RESPONSE 2

struct tcp_s {
    unsigned short reqtype;
    unsigned short cidx;
    struct {
        unsigned short src;
        unsigned short dst;
    } port;
    ALIGN16(8);
};

struct header_s {
    unsigned int   pidx;
    unsigned int   gidx;
    unsigned int   tidx;
    uint64_t       offset;
    unsigned int   chunks;
    unsigned int   parts;
    unsigned int   length;
    enum command_e command;
    unsigned char  filename[SHA256HEX];
    struct tcp_s   tcp;
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
                    struct send_buffer_s *sb,
                    unsigned int tidx, unsigned int parts,
                    unsigned char *filename,
                    struct tcp_s *tcp);
        int (*validate)(struct packet_s *packets, size_t nbuffer, bool *valid);
    } serialize;
    struct {
        int (*init)(char *buffer, size_t nbuffer, bool *valid);
    } deserialize;
};

extern const struct module_packet_s packet;

#endif
