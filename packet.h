#ifndef PACKET_H_
#define PACKET_H_

struct module_packet_s {
    int (*send)(char *buffer, size_t nbuffer);
    int (*receive)(char *buffer, size_t nbuffer);
};

extern const struct module_packet_s packet;

#endif
