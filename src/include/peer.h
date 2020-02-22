#ifndef PEER_H_
#define PEER_H_

enum instance_e {
    INSTANCE_TRACKER,
    INSTANCE_PEER,
};

enum buffer_e {
    BUFFER_NONE,
    BUFFER_MESSAGE,
    BUFFER_FILE,
    BUFFER_FILEASK,
    BUFFER_TRACKER_ANNOUNCE_PEER,
    BUFFER_AUTH,
    BUFFER_AUTH_REPLY,
    BUFFER_TCP,
    BUFFER_QUERY,
    BUFFER_QUERY_REPLY,
    BUFFER_PING,
    BUFFER_PONG,
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
            sn tcpdesc;
            int version;
            struct list_s *tcpports;
            char proxy;
        } tracker_peer;
        struct {
            sn str;
        } auth;
        struct {
            int host;
            unsigned short port;
        } query;
        struct {
            int host;
            unsigned short port;
            bool reachable;
        } query_reply;
        struct {
            double ts;
        } ping;
        struct {
            double ts;
        } pong;
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
    struct list_s        rogue;
    struct list_s        whitelist;
    struct {
        struct list_s tunnels;
        struct list_s endpoints;
    } tcp;
    struct {
        char *ptr;
        int   size;
    } miningtarget;
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
            int (*file)(struct peer_s *p, struct header_s *header,
                        int host, unsigned short port,
                        unsigned char *keyhash,
                        char *fullpath, int nfullpath,
                        char *filename, int nfilename);
            int (*fileask)(struct peer_s *p, int host,
                           unsigned short port,
                           char *msg, int len);
            int (*online)(struct peer_s *p,
                          struct world_peer_s *wp);
            int (*offline)(struct peer_s *p,
                           struct world_peer_s *wp);
            int (*cli)(struct peer_s *p, char **argv,
                       int argc);
            int (*auth)(struct peer_s *p, int host,
                        unsigned short port,
                        char *msg, int len);
            int (*authrpl)(struct peer_s *p, int host,
                           unsigned short port,
                           char *msg, int len);
            int (*query)(struct peer_s *p, int host,
                         unsigned short port,
                         int query_host, unsigned short query_port);
            int (*query_reply)(struct peer_s *p, int host,
                               unsigned short port,
                               int query_host, unsigned short query_port,
                               bool reachable);
            int (*ping)(struct peer_s *p, int host,
                        unsigned short port, double ts);
            int (*pong)(struct peer_s *p, int host,
                        unsigned short port, double ts);
        } cb;
        void *data;
    } user;
    struct {
        struct {
            int read;
            int write;
        } pipes;
        struct {
            int (*message)(struct peer_s *p, int host, unsigned short port,
                           char *msg, int len);
            int (*online)(struct peer_s *p, struct world_peer_s *wp);
            int (*offline)(struct peer_s *p, struct world_peer_s *wp);
            struct {
                int (*done)(struct peer_s *p, unsigned char *filename,
                            int nfilename);
            } job;
        } cb;
        struct {
            struct ev_io write;
            struct ev_io read;
        } ev;
        struct {
            struct list_s buffer;
        } write;
        struct {
            int dst;
            int size;
            char buffer[4096];
        } read;
    } api;
    struct hm_log_s log;
};

struct module_peer_s {
    struct {
        int (*mpeer)(struct peer_s *p);
        int (*mtracker)(struct peer_s *p);
    } init;
    int (*clean)(struct peer_s *p);
};

extern const struct module_peer_s peer;
extern struct peer_s *psig;

#endif
