#ifndef TRAFFIC_H_
#define TRAFFIC_H_

struct traffic_s {
    struct {
        double start;
        size_t bytes;
    } send;
    struct {
        double start;
        size_t bytes;
    } recv;
};

struct peer_s;

struct module_traffic_s {
    struct {
        int (*send)(struct peer_s *p, ssize_t bytes, bool *suspend);
        int (*recv)(struct peer_s *p, ssize_t bytes, bool *suspend);
    } update;
    int (*dump)(struct peer_s *p, json_object **obj);
};

extern const struct module_traffic_s traffic;

#endif
