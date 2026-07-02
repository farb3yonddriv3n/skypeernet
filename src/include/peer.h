/**
 * @file peer.h
 * @brief Core peer state and send buffer definitions.
 *
 * PEER ARCHITECTURE
 * =================
 *
 * A peer is the central state object for a node in the skypeernet network.
 * It can operate in two modes:
 *
 *   INSTANCE_TRACKER - Coordinates peer discovery, maintains peer list
 *   INSTANCE_PEER    - Regular network participant
 *
 * The peer structure contains:
 *   - Network state (socket, addresses)
 *   - Event loop integration (libev)
 *   - Send/receive buffers and queues
 *   - Connected peer list
 *   - User callbacks for application-level handling
 *   - API pipes for external control
 *
 * SEND BUFFER SYSTEM
 * ==================
 *
 * The send_buffer is a tagged union that holds command-specific data
 * before serialization. The workflow is:
 *
 *   1. Application sets send_buffer.type to indicate command
 *   2. Application fills appropriate union member (e.g., u.message.str)
 *   3. Application calls payload.send()
 *   4. Payload handler reads from send_buffer
 *   5. Data is serialized and queued for transmission
 *
 * This avoids passing many parameters through function calls and provides
 * a consistent interface for all command types.
 *
 * RECEIVE BUFFER SYSTEM
 * =====================
 *
 * Incoming packets are cached until a complete message is reassembled:
 *
 *   cache   - List of cache_s, one per in-progress message (keyed by gidx+host+port)
 *   available - Pointer to a cache_s when a message is complete and ready
 *   sealed  - (Commented out) Would track completed gidx to reject duplicates
 *
 * Each cache_s tracks:
 *   - group: The gidx this cache is collecting
 *   - packets: List of received packets, plus bitmap of received pidx values
 *   - data: Reassembled payload (built when all chunks arrive)
 *
 * CALLBACK SYSTEM
 * ===============
 *
 * The peer.user.cb structure allows applications to hook into protocol events:
 *
 *   clean   - Called on shutdown for cleanup
 *   message - Received a COMMAND_MESSAGE
 *   file    - Received a complete file
 *   fileask - Someone requested a file chunk
 *   online  - A peer came online
 *   offline - A peer went offline
 *   cli     - Command-line input received
 *   auth    - Authentication challenge received
 *   authrpl - Authentication response received
 *   query   - Reachability query received
 *   query_reply - Reachability response received
 *   ping/pong - Latency probes
 *
 * The API subsystem (peer.api) provides a separate callback path for
 * external control via named pipes, used by plugins like skynet.
 */

#ifndef PEER_H_
#define PEER_H_

/**
 * @brief Instance type - tracker or regular peer.
 */
enum instance_e {
    INSTANCE_TRACKER,  /**< This node is a tracker (coordinates discovery) */
    INSTANCE_PEER,     /**< This node is a regular peer */
};

/**
 * @brief Send buffer type tags.
 *
 * Indicates which union member in send_buffer_s is valid.
 * Set before calling payload.send() to specify what data to serialize.
 */
enum buffer_e {
    BUFFER_NONE,                  /**< No data / invalid */
    BUFFER_MESSAGE,               /**< Text message (u.message) */
    BUFFER_FILE,                  /**< File chunk (u.file) */
    BUFFER_FILEASK,               /**< File chunk request (u.fileask) */
    BUFFER_TRACKER_ANNOUNCE_PEER, /**< Tracker announcing a peer (u.tracker_peer) */
    BUFFER_AUTH,                  /**< Auth challenge (u.auth) */
    BUFFER_AUTH_REPLY,            /**< Auth response (u.auth, reused) */
    BUFFER_TCP,                   /**< TCP tunnel data (unused?) */
    BUFFER_QUERY,                 /**< Reachability query (u.query) */
    BUFFER_QUERY_REPLY,           /**< Reachability response (u.query_reply) */
    BUFFER_PING,                  /**< Ping probe (u.ping) */
    BUFFER_PONG,                  /**< Pong response (u.pong) */
    BUFFER_KEY,                   /**< CEK grant for a file (u.key) */
};

/**
 * @brief Send buffer - holds data pending serialization.
 *
 * Before sending a message, fill the appropriate union member
 * and set 'type' to indicate which one. The payload handler
 * will read from here during serialization.
 */
struct send_buffer_s {
    unsigned int pidx;   /**< Packet index counter (increments per packet sent) */
    unsigned int gidx;   /**< Group index counter (increments per message sent) */
    enum buffer_e type;  /**< Which union member is valid */

    union {
        /** For BUFFER_MESSAGE - text message */
        struct {
            const char *str;  /**< Message string (null-terminated) */
        } message;

        /** For BUFFER_FILE - file chunk data */
        struct {
            sn bin;  /**< Binary chunk data */
        } file;

        /** For BUFFER_FILEASK - request file chunk */
        struct {
            unsigned char *file;   /**< File hash (SHA256HEX) */
            unsigned char *chunk;  /**< Chunk hash (SHA256HEX) */
        } fileask;

        /** For BUFFER_TRACKER_ANNOUNCE_PEER - tracker broadcasting peer info */
        struct {
            int host;                  /**< Peer's IP address */
            unsigned short port;       /**< Peer's port */
            sn *key;                   /**< Peer's public key */
            sn tcpdesc;                /**< TCP services description */
            int version;               /**< Peer's protocol version */
            struct list_s *tcpports;   /**< List of exposed TCP ports */
            char proxy;                /**< 1 if peer can act as proxy */
        } tracker_peer;

        /** For BUFFER_AUTH / BUFFER_AUTH_REPLY - authentication data */
        struct {
            sn str;  /**< Encrypted challenge or decrypted response */
        } auth;

        /** For BUFFER_QUERY - ask about peer reachability */
        struct {
            int host;             /**< Host to query about */
            unsigned short port;  /**< Port to query about */
        } query;

        /** For BUFFER_QUERY_REPLY - reachability answer */
        struct {
            int host;             /**< Host that was queried */
            unsigned short port;  /**< Port that was queried */
            bool reachable;       /**< True if host:port is reachable */
        } query_reply;

        /** For BUFFER_PING - latency probe */
        struct {
            double ts;  /**< Timestamp when ping was sent */
        } ping;

        /** For BUFFER_PONG - latency response */
        struct {
            double ts;  /**< Original timestamp from ping */
        } pong;

        /** For BUFFER_KEY - key grant (file CEK or group key) */
        struct {
            char           ktype; /**< KEY_TYPE_CEK or KEY_TYPE_GROUP */
            unsigned char *file;  /**< File hash or group key hash (SHA256HEX) */
            sn             key;   /**< base64(RSA(recipient_pub, secret)) */
        } key;
    } u;
};

/**
 * @brief Packet cache for message reassembly.
 *
 * When receiving a multi-packet message, packets are cached here
 * until all chunks (identified by common gidx) arrive.
 */
struct cache_s {
    int            group;  /**< Group index (gidx) being assembled */
    int            host;   /**< Source host IP */
    unsigned short port;   /**< Source port */

    struct {
        int           total;     /**< Expected number of packets (from header.chunks) */
        struct list_s all;       /**< List of received packet_s */
        struct {
            int *idx;            /**< Array of received pidx values */
            int  size;           /**< Number of pidx values received */
        } received;
    } packets;

    struct {
        size_t received;  /**< Bytes received so far */
        size_t total;     /**< Total expected bytes */
    } file_size;

    sn data;  /**< Reassembled payload (populated when complete) */
};

/**
 * @brief Completed message tracking (for duplicate rejection).
 *
 * Tracks which (gidx, host, port) combinations have been fully
 * processed, to reject duplicate retransmissions.
 * (Currently commented out in the codebase)
 */
struct seal_s {
    int           *group;  /**< Array of completed gidx values */
    int            size;   /**< Number of entries */
    int            host;   /**< Source host */
    unsigned short port;   /**< Source port */
};

/**
 * @brief Receive buffer state.
 */
struct recv_buffer_s {
    struct list_s   cache;      /**< List of cache_s for in-progress messages */
    struct cache_s *available;  /**< Points to completed message (or NULL) */
    struct list_s   sealed;     /**< Completed message tracking (unused) */
};

struct world_peer_s;

/**
 * @brief Main peer state structure.
 *
 * This is the central data structure for a node. It contains all
 * state needed for network operation, including:
 *
 *   - Instance type (tracker vs peer)
 *   - Network configuration and sockets
 *   - Event loop integration
 *   - Send/receive buffers
 *   - Peer list (known peers)
 *   - Application callbacks
 *   - API interface for external control
 */
struct peer_s {
    enum instance_e      type;         /**< INSTANCE_TRACKER or INSTANCE_PEER */
    struct net_s         net;          /**< Network state (socket, addresses) */
    struct net_ev_s      ev;           /**< libev event watchers */
    struct net_send_s    send;         /**< Send queue */
    struct net_recv_s    recv;         /**< Receive buffer */
    struct packet_s      received;     /**< Most recently received packet */
    struct list_s        peers;        /**< List of known world_peer_s */
    struct config_s      cfg;          /**< Configuration */
    struct traffic_s     traffic;      /**< Traffic statistics */
    struct send_buffer_s send_buffer;  /**< Outgoing data staging area */
    struct recv_buffer_s recv_buffer;  /**< Incoming message reassembly */
    struct list_s        backtrace;    /**< Debug backtrace info */
    struct list_s        rogue;        /**< Rogue peer tracking */
    struct list_s        whitelist;    /**< Allowed peer public key hashes */

    /** TCP tunneling state */
    struct {
        struct list_s tunnels;    /**< Active tunnel_s instances */
        struct list_s endpoints;  /**< Active endpoint_s instances */
    } tcp;

    /** Mining configuration */
    struct {
        char *ptr;   /**< Mining target data */
        int   size;  /**< Mining target size */
    } miningtarget;

    /** Task queue for background operations */
    struct {
        struct list_s list;  /**< List of pending tasks */
        int           idx;   /**< Task index counter */
    } tasks;

    /** Tracker connection info (for peers) */
    struct {
        int            host;  /**< Tracker IP */
        unsigned short port;  /**< Tracker port */
    } tracker;

    /**
     * User (application) callbacks.
     *
     * Applications set these to handle protocol events.
     * The 'data' pointer is passed to callbacks as context.
     */
    struct {
        struct {
            /** Called on clean shutdown */
            int (*clean)(struct peer_s *p, void *data);
            /** Received a text message */
            int (*message)(struct peer_s *p, int host,
                           unsigned short port,
                           char *msg, int len);
            /** Received a complete file */
            int (*file)(struct peer_s *p, struct header_s *header,
                        int host, unsigned short port,
                        unsigned char *keyhash,
                        char *fullpath, int nfullpath,
                        char *filename, int nfilename);
            /** Received a file chunk request */
            int (*fileask)(struct peer_s *p, int host,
                           unsigned short port,
                           char *msg, int len);
            /** A peer came online */
            int (*online)(struct peer_s *p,
                          struct world_peer_s *wp);
            /** A peer went offline */
            int (*offline)(struct peer_s *p,
                           struct world_peer_s *wp);
            /** CLI command received */
            int (*cli)(struct peer_s *p, char **argv,
                       int argc);
            /** Authentication challenge received */
            int (*auth)(struct peer_s *p, int host,
                        unsigned short port,
                        char *msg, int len);
            /** Authentication response received */
            int (*authrpl)(struct peer_s *p, int host,
                           unsigned short port,
                           char *msg, int len);
            /** Reachability query received */
            int (*query)(struct peer_s *p, int host,
                         unsigned short port,
                         int query_host, unsigned short query_port);
            /** Reachability response received */
            int (*query_reply)(struct peer_s *p, int host,
                               unsigned short port,
                               int query_host, unsigned short query_port,
                               bool reachable);
            /** Ping received */
            int (*ping)(struct peer_s *p, int host,
                        unsigned short port, double ts);
            /** Pong received */
            int (*pong)(struct peer_s *p, int host,
                        unsigned short port, double ts);
            /** CEK grant received (COMMAND_KEY) */
            int (*key)(struct peer_s *p, int host,
                       unsigned short port,
                       char *data, int len);
        } cb;
        void *data;  /**< User context pointer (e.g., distfs_s) */
    } user;

    /**
     * API subsystem for external control.
     *
     * Uses named pipes for IPC with external tools (like skynet plugin).
     * Provides a JSON-based command interface.
     */
    struct {
        struct {
            int read;   /**< Read pipe file descriptor */
            int write;  /**< Write pipe file descriptor */
        } pipes;
        struct {
            /** Message notification callback */
            int (*message)(struct peer_s *p, int host, unsigned short port,
                           char *msg, int len);
            /** Peer online notification */
            int (*online)(struct peer_s *p, struct world_peer_s *wp);
            /** Peer offline notification */
            int (*offline)(struct peer_s *p, struct world_peer_s *wp);
            struct {
                /** Job completion notification */
                int (*done)(struct peer_s *p, unsigned char *filename,
                            int nfilename);
            } job;
        } cb;
        struct {
            struct ev_io write;  /**< Write pipe watcher */
            struct ev_io read;   /**< Read pipe watcher */
        } ev;
        struct {
            struct list_s buffer;  /**< Pending write data */
        } write;
        struct {
            int dst;           /**< Expected message size */
            int size;          /**< Current buffer fill */
            char buffer[4096]; /**< Read buffer */
        } read;
    } api;

    struct hm_log_s log;  /**< Logging state */
};

/**
 * @brief Peer module interface.
 *
 * Provides initialization and cleanup functions.
 */
struct module_peer_s {
    struct {
        /** Initialize as a regular peer */
        int (*mpeer)(struct peer_s *p);
        /** Initialize as a tracker */
        int (*mtracker)(struct peer_s *p);
    } init;
    /** Clean up peer state */
    int (*clean)(struct peer_s *p);
};

extern const struct module_peer_s peer;

/** Global peer pointer for signal handlers */
extern struct peer_s *psig;

#endif
