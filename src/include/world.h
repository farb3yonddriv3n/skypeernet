/**
 * @file world.h
 * @brief Peer discovery and world state management.
 *
 * WORLD MODEL
 * ===========
 *
 * The "world" represents our view of the network - all peers we know about
 * and their states. This includes:
 *
 *   - Trackers: Coordinate peer discovery
 *   - Peers: Regular network participants
 *   - Their public keys, addresses, capabilities
 *   - Connection state (authenticated, reachable, proxied)
 *
 * PEER LIFECYCLE
 * ==============
 *
 *   1. DISCOVERY: Learn about peer from tracker (COMMAND_TRACKER_ANNOUNCE_PEER)
 *   2. ADD: Create world_peer_s, add to peer list
 *   3. AUTH: If required, complete RSA challenge-response
 *   4. ONLINE: Peer is usable, callbacks fired
 *   5. QUERY: Optionally verify reachability
 *   6. OFFLINE: Peer becomes unreachable, callbacks fired
 *   7. REMOVE: Clean up world_peer_s
 *
 * PEER FLAGS
 * ==========
 *
 *   WORLD_PEER_AUTHED  - RSA authentication completed successfully
 *   WORLD_PEER_QUERIED - Reachability has been checked
 *   WORLD_PEER_SHADOW  - Packets should be routed through a proxy
 *   WORLD_PEER_PROXY   - This peer can act as a proxy for others
 *   WORLD_PEER_OFFLINE - Peer is known but currently unreachable
 *
 * SHADOW ROUTING (NAT Traversal)
 * ==============================
 *
 * When a peer is behind NAT and not directly reachable, traffic can be
 * "shadowed" through a proxy peer:
 *
 *   Peer A (behind NAT)
 *       |
 *       v
 *   Proxy Peer (WORLD_PEER_PROXY flag)
 *       |
 *       v
 *   Peer B (destination)
 *
 * The WORLD_PEER_SHADOW flag indicates this peer needs shadow routing.
 * world.peer.shadow() looks up an appropriate proxy peer.
 *
 * AUTHENTICATION FLOW
 * ===================
 *
 *   1. Peer announces with public key
 *   2. Tracker/peer generates random challenge
 *   3. Challenge encrypted with peer's public key (COMMAND_AUTH)
 *   4. Peer decrypts with private key
 *   5. Peer sends decrypted value back (COMMAND_AUTH_REPLY)
 *   6. Verifier checks response matches original challenge
 *   7. On success, WORLD_PEER_AUTHED flag is set
 *
 * REACHABILITY CHECKING
 * =====================
 *
 * Peers periodically check if other peers are reachable:
 *
 *   1. world.peer.check() timer fires
 *   2. For each unqueried peer, send COMMAND_QUERY to another peer
 *   3. That peer tries to contact the target
 *   4. COMMAND_QUERY_REPLY reports success/failure
 *   5. Unreachable peers get marked for shadow routing
 *
 * This helps detect NAT situations and establish proxy paths.
 */

#ifndef WORLD_H_
#define WORLD_H_

/**
 * @brief Peer type in the network.
 */
enum world_peer_e {
    WORLD_PEER_NONE,     /**< Invalid/uninitialized */
    WORLD_PEER_TRACKER,  /**< This is a tracker node */
    WORLD_PEER_PEER,     /**< This is a regular peer */
};

/**
 * @brief Peer state flags (bitmask).
 */
#define WORLD_PEER_AUTHED  0x1   /**< RSA authentication completed */
#define WORLD_PEER_QUERIED 0x2   /**< Reachability has been checked */
#define WORLD_PEER_SHADOW  0x4   /**< Traffic should go through proxy */
#define WORLD_PEER_PROXY   0x8   /**< Can proxy for other peers */
#define WORLD_PEER_OFFLINE 0x10  /**< Currently unreachable */

/**
 * @brief Information about a known peer.
 *
 * Represents everything we know about another node in the network.
 * Stored in peer->peers list.
 */
struct world_peer_s {
    enum world_peer_e    type;          /**< TRACKER or PEER */
    int                  host;          /**< IP address (network byte order) */
    unsigned short       port;          /**< UDP port */
    struct world_peer_s *found;         /**< Used for list searches (temp result) */
    unsigned int         unreachable;   /**< Counter of failed contact attempts */
    sn                   pubkey;        /**< RSA public key (PEM format) */
    unsigned char        pubkeyhash[SHA256HEX]; /**< SHA256 of public key (identifier) */
    unsigned char        authstr[SHA256HEX];    /**< Auth challenge string */
    int                  version;       /**< Protocol version peer supports */
    int                  pingms;        /**< Last measured latency (ms) */
    unsigned int         flags;         /**< WORLD_PEER_* flags */

    /** TCP services this peer offers */
    struct {
        char description[256];  /**< Human-readable description */
        struct list_s ports;    /**< List of exposed TCP ports */
    } tcp;

    /** Statistics */
    struct {
        unsigned int sent;  /**< Packets sent to this peer */
    } stats;
};

/**
 * @brief World management module interface.
 */
struct module_world_s {
    struct {
        /**
         * Mark a peer as reachable.
         *
         * Called when we successfully communicate with a peer.
         * Clears WORLD_PEER_OFFLINE, updates latency.
         *
         * @param p     Peer context
         * @param host  Peer IP
         * @param port  Peer port
         * @param ts    Timestamp for latency calculation
         */
        int (*reachable)(struct peer_s *p, int host, unsigned short port,
                         double ts);

        /**
         * Timer callback for periodic reachability checks.
         *
         * Iterates through known peers and verifies they're still
         * reachable, updating flags accordingly.
         */
        void (*check)(struct ev_loop *loop, struct ev_timer *timer, int revents);

        /**
         * Check if a peer is currently reachable.
         *
         * @param p         Peer context
         * @param host      Target IP
         * @param port      Target port
         * @param reachable Output: true if reachable
         */
        int (*isreachable)(struct peer_s *p, int host, unsigned short port,
                           bool *reachable);

        /**
         * Find peer by public key hash.
         *
         * List callback for searching peer list by pubkeyhash.
         * Sets found->found on match.
         */
        int (*findpubkeyhash)(struct list_s *l, void *existing, void *uwp);

        /**
         * Find peer by auth string.
         *
         * List callback for matching auth challenge responses.
         */
        int (*findauthstr)(struct list_s *l, void *existing, void *uwp);

        /**
         * Find peer by host:port.
         *
         * List callback for searching by network address.
         */
        int (*find)(struct list_s *l, void *existing, void *uwp);

        /**
         * Broadcast a message to all known peers.
         *
         * @param p   Peer context
         * @param wp  Message source (excluded from broadcast)
         */
        int (*broadcast)(struct peer_s *p, struct world_peer_s *wp);

        /**
         * Initiate authentication with a peer.
         *
         * Generates challenge, encrypts with peer's public key,
         * sends COMMAND_AUTH.
         */
        int (*auth)(struct peer_s *p, struct world_peer_s *wp);

        /**
         * Add a peer to the world.
         *
         * @param p     Peer context
         * @param wp    Peer to add
         * @param added Output: true if newly added (vs already known)
         */
        int (*add)(struct peer_s *p, struct world_peer_s *wp,
                   bool *added);

        /**
         * Apply shadow routing if needed.
         *
         * If the target peer has WORLD_PEER_SHADOW flag, this function
         * replaces host/port with a proxy peer's address.
         *
         * @param p    Peer context
         * @param host In/out: destination IP
         * @param port In/out: destination port
         */
        int (*shadow)(struct peer_s *p, int *host, unsigned short *port);

        /**
         * Find a peer that can act as proxy.
         *
         * List callback to find peers with WORLD_PEER_PROXY flag.
         */
        int (*findproxy)(struct list_s *l, void *existing, void *ud);

        /**
         * Clean up world_peer_s structure.
         */
        int (*clean)(void *uwp);
    } peer;
};

extern const struct module_world_s world;

#endif
