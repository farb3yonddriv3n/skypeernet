#include <common.h>
#include <cu.h>

void t6_packet()
{
    char tblock[8192];
    memset(tblock, 0, sizeof(tblock));
    A(packet.send(tblock, sizeof(tblock)), 0);
}
