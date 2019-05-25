#include <common.h>

#define KB_TO_BYTES(m_dst) (m_dst * 1024)

static int update(struct peer_s *p, ssize_t bytes, bool *suspend)
{
    if (!p || !suspend || bytes < 1) return -1;
    int limit = 50;
    *suspend = false;
    p->traffic.sent += bytes;
    if (p->traffic.start == .0f)
        return os.gettimems(&p->traffic.start);
    double diff = 1.0f - p->cfg.net.interval.peer_resend;
    double timestampms;
    if (os.gettimems(&timestampms) != 0) return -1;
    if ((timestampms - diff) < p->traffic.start &&
        p->traffic.sent >= KB_TO_BYTES(limit)) {
        p->traffic.start = .0f;
        p->traffic.sent  = 0;
        *suspend = true;
        return 0;
    } else if ((timestampms - diff) >= p->traffic.start) {
        p->traffic.start = .0f;
        p->traffic.sent  = 0;
    }
    return 0;
}

const struct module_traffic_s traffic = {
    .update = update,
};
