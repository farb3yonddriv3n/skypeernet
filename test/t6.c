#include <common.h>
#include <cu.h>

void t6_packet()
{
    char tblock[8192];
    memset(tblock, 0, sizeof(tblock));
    struct packet_s *packets;
    int npackets;
    int idx = 0;
    A(packet.serialize.init(COMMAND_MESSAGE, tblock, sizeof(tblock), &packets,
                            &npackets, &idx), 0);
    bool valid;
    A(packet.serialize.validate(packets, npackets, &valid), 0);
    A(valid, true);

    A(packet.clean(packets), 0);
}
