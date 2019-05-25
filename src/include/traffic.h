#ifndef TRAFFIC_H_
#define TRAFFIC_H_

struct traffic_s {
    double start;
    size_t sent;
};

struct peer_s;

struct module_traffic_s {
    int (*update)(struct peer_s *p, ssize_t bytes, bool *suspend);
};

extern const struct module_traffic_s traffic;

#endif
