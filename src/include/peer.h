#ifndef PEER_H_
#define PEER_H_

enum instance_e {
    INSTANCE_TRACKER,
    INSTANCE_PEER,
};

enum buffer_e {
    BUFFER_MESSAGE,
    BUFFER_FILE,
    BUFFER_FILEASK,
    BUFFER_TRACKER_ANNOUNCE_PEER,
};

struct send_buffer_s {
    unsigned int pidx;
    unsigned int gidx;
    enum buffer_e type;
    union {
        struct {
            const char *str;
        } message;
        struct {
            sn bin;
        } file;
        struct {
            unsigned char *file;
            unsigned char *chunk;
        } fileask;
        struct {
            int host;
            unsigned short port;
            sn *key;
        } tracker_peer;
    } u;
};

struct cache_s {
    int            group;
    int            host;
    unsigned short port;
    struct {
        int           total;
        struct list_s all;
        struct {
            int *idx;
            int  size;
        } received;
    } packets;
    struct {
        size_t received;
        size_t total;
    } file_size;
    sn data;
};

struct seal_s {
    int           *group;
    int            size;
    int            host;
    unsigned short port;
};

struct recv_buffer_s {
    struct list_s   cache;
    struct cache_s *available;
    struct list_s   sealed;
};

struct world_peer_s;

struct peer_s {
    enum instance_e      type;
    struct net_s         net;
    struct net_ev_s      ev;
    struct net_send_s    send;
    struct net_recv_s    recv;
    struct packet_s      received;
    struct list_s        peers;
    struct config_s      cfg;
    struct traffic_s     traffic;
    struct send_buffer_s send_buffer;
    struct recv_buffer_s recv_buffer;
    struct list_s        backtrace;
    struct {
        struct list_s list;
        int           idx;
    } tasks;
    struct {
        int            host;
        unsigned short port;
    } tracker;
    struct {
        struct {
            int (*clean)(struct peer_s *p, void *data);
            int (*message)(struct peer_s *p, int host,
                           unsigned short port,
                           char *msg, int len);
            int (*file)(struct peer_s *p, int host,
                        unsigned short port,
                        unsigned char *keyhash,
                        const char *fullpath,
                        const char *fullname);
            int (*fileask)(struct peer_s *p, int host,
                           unsigned short port,
                           char *msg, int len);
            int (*online)(struct peer_s *p,
                          struct world_peer_s *wp);
            int (*cli)(struct peer_s *p, char **argv,
                       int argc);
        } cb;
        void *data;
    } user;
};

struct module_peer_s {
    struct {
        int (*mpeer)(struct peer_s *p);
        int (*mtracker)(struct peer_s *p);
    } init;
    int (*clean)(struct peer_s *p);
};

extern const struct module_peer_s peer;

#endif
