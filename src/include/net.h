/**
 * @file net.h
 * @brief Network layer definitions for UDP communication.
 *
 * NETWORK ARCHITECTURE
 * ====================
 *
 * The network layer handles low-level UDP packet transmission with
 * a reliability layer built on top:
 *
 *   Application
 *       |
 *   Payload/Packet layers
 *       |
 *   Network (this module) - UDP send/receive, retries, event loop
 *       |
 *   Operating System (socket API)
 *
 * RELIABILITY MODEL
 * =================
 *
 * skypeernet implements its own reliability over UDP:
 *
 *   1. SEND: Packets are queued in nb_s (network buffer) structures
 *   2. WAIT: A timer periodically checks for unacknowledged packets
 *   3. RETRY: Unacked packets are retransmitted (up to some limit)
 *   4. ACK: When receiver processes a packet, it sends COMMAND_ACK
 *   5. COMPLETE: Sender marks packet as delivered on ACK receipt
 *
 * Two send queues exist:
 *   - nbl (normal list): Retryable packets, indexed for ACK matching
 *   - instant: Fire-and-forget packets (ACK, PING, PONG)
 *
 * PACKET STATUS STATES
 * ====================
 *
 *   NET_INIT    - Packet awaiting transmission/acknowledgment
 *                 Will be retried on timeout
 *
 *   NET_ONESHOT - Single transmission, no retry
 *                 Used for ACK (can't ACK an ACK), PING, PONG
 *
 * WHY UDP?
 * ========
 *
 * P2P networks often use UDP because:
 *   - NAT traversal is easier (hole punching works better)
 *   - Connection state doesn't need to be maintained
 *   - Custom reliability allows optimizing for P2P patterns
 *   - Multicast/broadcast possible where needed
 *
 * The tradeoff is implementing reliability manually, which this
 * network layer provides.
 *
 * ADDRESS MACROS
 * ==============
 *
 * ADDR_IP(addr)   - Extract IP from sockaddr_in
 * ADDR_PORT(addr) - Extract port from sockaddr_in
 *
 * These access sin_addr.s_addr and sin_port directly, keeping
 * addresses in network byte order throughout the codebase.
 */

#ifndef NET_H_
#define NET_H_

/** Extract IP address from sockaddr_in */
#define ADDR_IP(m_dst) m_dst.sin_addr.s_addr
/** Extract port from sockaddr_in */
#define ADDR_PORT(m_dst) m_dst.sin_port

/**
 * @brief Packet transmission status.
 */
enum net_status_e {
    NET_INIT,     /**< Normal packet - will be retried until ACKed */
    NET_ONESHOT,  /**< Fire-and-forget - no retry (ACK, PING, PONG) */
};

/**
 * @brief Network buffer - represents a packet in the send queue.
 *
 * Contains everything needed to (re)transmit a packet and match
 * it against incoming ACKs.
 */
struct nb_s {
    struct peer_s *peer;    /**< Owning peer context */
    int pidx;               /**< Packet index (for ACK matching) */
    int gidx;               /**< Group index (for ACK matching) */
    int tidx;               /**< Transaction index */
    enum command_e cmd;     /**< Command type */
    int sd;                 /**< Socket descriptor */
    snb buffer;             /**< Serialized packet data (see sn.h) */
    struct {
        struct sockaddr_in addr;  /**< Destination address */
        socklen_t          len;   /**< Address length */
    } remote;
    enum net_status_e status;  /**< NET_INIT or NET_ONESHOT */
    int               attempt; /**< Retry attempt counter */
};

/**
 * @brief Send queue state.
 *
 * Two queues for different transmission patterns:
 *   nbl     - Normal packets that need reliable delivery
 *   instant - Immediate packets without retry (ACK, PING, PONG)
 */
struct net_send_s {
    struct list_s nbl;      /**< Reliable send queue (indexed by pidx+tidx) */
    struct list_s instant;  /**< Immediate send queue (no retry) */
};

/**
 * @brief Timer context for retry handling.
 */
struct net_send_timer_s {
    struct nb_s     *nb;   /**< Network buffer being processed */
    struct list_s   *nbl;  /**< Send list reference */
    struct net_ev_s *nev;  /**< Event loop reference */
};

/**
 * @brief Receive buffer.
 *
 * Simple buffer for incoming UDP packets.
 * One packet at a time, size limited to UDP_PACKET_SIZE.
 */
struct net_recv_s {
    char data[UDP_PACKET_SIZE];  /**< Raw packet data */
};

/**
 * @brief Network address wrapper.
 */
struct net_addr_s {
    struct sockaddr_in addr;  /**< Socket address */
    socklen_t          len;   /**< Address length */
};

/**
 * @brief Core network state.
 *
 * Contains the UDP socket and address information.
 */
struct net_s {
    int               sd;      /**< UDP socket descriptor */
    int               pidx;    /**< Global packet index counter */
    struct net_addr_s self;    /**< Our bound address */
    struct net_addr_s remote;  /**< Last remote address (receive) */
    struct {
        int host;              /**< Tracker IP (for peers) */
        unsigned short port;   /**< Tracker port */
    } tracker;
};

/**
 * @brief Event loop integration (libev).
 *
 * Uses libev for asynchronous I/O:
 *   - read watcher: Triggers when data available on socket
 *   - write watcher: Triggers when socket writable
 *   - send timer: Periodic retry of unacked packets
 *   - peers_reachable timer: Periodic reachability checks
 *   - stdinwatch: CLI input handling
 */
struct net_ev_s {
    struct ev_loop *loop;  /**< libev event loop */
    struct {
        struct ev_io ev;       /**< Read event watcher */
        bool         started;  /**< Whether watcher is active */
    } read;
    struct ev_io    write;           /**< Write event for normal queue */
    struct ev_io    write_instant;   /**< Write event for instant queue */
    struct ev_io    stdinwatch;      /**< CLI input watcher */
    struct ev_timer send;            /**< Retry timer */
    struct ev_timer peers_reachable; /**< Reachability check timer */
};

/**
 * @brief Network module interface.
 */
struct module_net_s {
    /**
     * Receive a packet from the network.
     *
     * @param p      Peer context
     * @param sd     Socket descriptor
     * @param data   Buffer for received data
     * @param len    Buffer length
     * @param addr   Output: sender address
     * @param naddr  Output: address length
     */
    int (*receive)(struct peer_s *p, int sd, char *data, int len,
                   struct sockaddr_in *addr, socklen_t *naddr);

    /**
     * Process an ACK - remove acknowledged packet from send queue.
     *
     * @param ns    Send queue state
     * @param tidx  Transaction index from ACK
     * @param pidx  Packet index from ACK
     */
    int (*ack)(struct net_send_s *ns, int tidx, int pidx);

    /**
     * Dispatch queued packets for transmission.
     *
     * @param l  Send queue list
     */
    int (*dispatch)(struct list_s *l);

    /**
     * Timer callback for packet retry.
     * Called periodically by libev to retransmit unacked packets.
     */
    void (*retry)(struct ev_loop *loop, struct ev_timer *timer, int revents);

    /**
     * Resume network operations (start event watchers).
     */
    int (*resume)(struct net_ev_s *ev);

    /**
     * Suspend network operations (stop event watchers).
     */
    int (*suspend)(struct net_ev_s *ev);

    struct {
        /** Clean up network buffer */
        int (*clean)(void *ptr);
    } nb;
};

extern const struct module_net_s net;

#endif
