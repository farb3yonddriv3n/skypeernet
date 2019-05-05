#include <common.h>
#include <cu.h>

void t6_packet()
{
    char tblock[8192];
    memset(tblock, 0, sizeof(tblock));
    struct packet_s *packets;
    int npackets;
    A(packet.serialize.init(tblock, sizeof(tblock), &packets,
                            &npackets, 0), 0);
    bool valid;
    A(packet.serialize.validate(packets, npackets, &valid), 0);
    A(valid, true);

    A(packet.clean(packets), 0);
}
