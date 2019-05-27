#include <common.h>

#define KB_TO_BYTES(m_dst) (m_dst * 1024)

static int update_send(struct peer_s *p, ssize_t bytes, bool *suspend)
{
    if (!p || !suspend || bytes < 1) return -1;
    *suspend = false;
    double timestampms;
    if (os.gettimems(&timestampms) != 0) return -1;
    double diff = 1.0f - p->cfg.net.interval.resend;
    if (p->traffic.send.start == .0f || (timestampms - diff) >= p->traffic.send.start) {
        p->traffic.send.bytes = bytes;
        return os.gettimems(&p->traffic.send.start);
    }
    if (p->traffic.send.bytes + bytes >= KB_TO_BYTES(p->cfg.net.max.upload)) {
        *suspend = true;
        return 0;
    }
    p->traffic.send.bytes += bytes;
    return 0;
}

const struct module_traffic_s traffic = {
    .update.send = update_send,
};
