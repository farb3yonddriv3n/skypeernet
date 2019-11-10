#include <common.h>
#include <cu.h>

void t3_packet()
{
    char tblock[8192];
    memset(tblock, 0, sizeof(tblock));
    struct packet_s *packets;
    int npackets;
    int tidx = 0;
    int parts = 0;
    struct send_buffer_s sb;
    memset(&sb, 0, sizeof(sb));
    struct peer_s p;
    memset(&p, 0, sizeof(p));
    A(packet.serialize.init(&p, COMMAND_MESSAGE, tblock, sizeof(tblock), &packets,
                            &npackets, &sb, tidx, parts, NULL, NULL, 0, 0), 0);
    bool valid;
    A(packet.serialize.validate(packets, npackets, &valid), 0);
    A(valid, true);

    A(packet.clean(packets), 0);
}
