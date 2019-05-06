#ifndef TRACKER_H_
#define TRACKER_H_

#define TRACKER_PORT        5775
#define TRACKER_BUFFER_SIZE 1024

#define PEER_NEW (1 << 0)

struct peer_s {
    int            host;
    unsigned short port;
    unsigned int   flags;
    struct peer_s *prev;
    struct peer_s *next;
};

struct tracker_s {
    int                sd;
    struct sockaddr_in addr;
    int                addr_len;
    char               data[TRACKER_BUFFER_SIZE];
    int                index;
    struct ev_loop    *loop;
    struct ev_io       read;
    struct ev_io       write;
    struct {
        struct peer_s *list;
        int            count;
    } peers;
};

#endif
