#include <common.h>

/*
 * COMMAND_KEY - key grant.
 *
 * Payload: type byte (KEY_TYPE_*), hash (SHA256HEX bytes), then
 * base64(RSA(recipient_pub, secret)). Lets a key holder grant another
 * peer access to an already-shared file (CEK) or to a group (group
 * key) without re-mining any block.
 */

int key_write(struct data_s *d, void *userdata)
{
    struct peer_s *p = (struct peer_s *)userdata;
    if (!p || !d) return -1;
    if (p->send_buffer.type != BUFFER_KEY) return -1;
    ifr(data.write.raw(d, &p->send_buffer.u.key.ktype, 1));
    ifr(data.write.raw(d, (char *)p->send_buffer.u.key.file,
                       SHA256HEX));
    ifr(data.write.raw(d, p->send_buffer.u.key.key.s,
                       p->send_buffer.u.key.key.n));
    return 0;
}

int key_read(struct peer_s *p)
{
    if (!p) return -1;
    if (p->user.cb.key)
        ifr(p->user.cb.key(p,
                           p->received.header.src.host,
                           p->received.header.src.port,
                           p->recv_buffer.available->data.s,
                           p->recv_buffer.available->data.n));
    return 0;
}

int key_size(int *sz, void *userdata)
{
    struct peer_s *p = (struct peer_s *)userdata;
    if (!p || !sz) return -1;
    if (p->send_buffer.type != BUFFER_KEY) return -1;
    *sz = 1 + SHA256HEX + p->send_buffer.u.key.key.n;
    return 0;
}
