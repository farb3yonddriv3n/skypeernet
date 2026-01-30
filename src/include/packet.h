/**
 * @file packet.h
 * @brief Core packet structure definitions for the skypeernet P2P protocol.
 *
 * PROTOCOL OVERVIEW
 * =================
 * skypeernet uses a custom UDP-based protocol with the following characteristics:
 *
 * 1. PACKET STRUCTURE:
 *    - Fixed maximum size: 534 bytes (UDP_PACKET_SIZE)
 *    - Each packet contains: header + payload + SHA256 hash
 *    - Large messages are chunked into multiple packets (same gidx, different pidx)
 *
 * 2. PACKET FRAGMENTATION:
 *    - Messages larger than UDP_PACKET_PAYLOAD are split into chunks
 *    - Each chunk gets its own packet with sequential pidx (packet index)
 *    - All chunks share the same gidx (group index) for reassembly
 *    - header.chunks tells receiver how many packets to expect
 *    - header.offset tells where this chunk fits in the reassembled data
 *
 * 3. RELIABILITY:
 *    - Uses ACK-based reliability (not TCP-style sliding window)
 *    - Every command except ACK/PING/PONG expects an ACK reply
 *    - Sender retries unacknowledged packets
 *
 * 4. INTEGRITY:
 *    - Each packet ends with SHA256 hash of (header + payload)
 *    - Receiver validates hash before processing
 *
 * 5. ADDRESSING:
 *    - Uses raw IP addresses as 32-bit integers (network byte order)
 *    - Ports are unsigned shorts
 *    - Supports proxy/shadow routing for NAT traversal
 */

#ifndef PACKET_H_
#define PACKET_H_

/**
 * Maximum size of a single UDP packet.
 * Set to 534 bytes to maximize payload while staying under typical MTU (~1500).
 */
#define UDP_PACKET_SIZE    534

/**
 * Maximum payload bytes per packet after subtracting header and hash.
 * payload_size = total_size - header_size - hash_size
 *
 * Using CRC32 (8 bytes hex) instead of SHA256 (64 bytes hex) for packet
 * integrity saves 56 bytes per packet and is much faster to compute.
 */
#define UDP_PACKET_PAYLOAD (UDP_PACKET_SIZE - sizeof(struct header_s) - CRC32_SIZE)
//#define UDP_PACKET_PAYLOAD 1

/**
 * @brief Protocol command types.
 *
 * Commands define the type of message being sent. The protocol follows
 * a request-response pattern where most commands trigger an ACK reply.
 *
 * COMMAND FLOW DIAGRAMS:
 *
 * Peer Registration (peer joins network):
 *   Peer -> Tracker: COMMAND_PEER_ANNOUNCE_PEER
 *   Tracker -> Peer: ACK
 *   Tracker -> Peer: COMMAND_TRACKER_ANNOUNCE_TRACKER (confirms registration, tells peer its external IP)
 *   Tracker -> Other Peers: COMMAND_TRACKER_ANNOUNCE_PEER (broadcasts new peer)
 *
 * Peer Discovery:
 *   Tracker -> Peer: COMMAND_TRACKER_ANNOUNCE_PEER (info about another peer)
 *   Peer -> Peer: (direct communication now possible)
 *
 * Message Exchange:
 *   Peer A -> Peer B: COMMAND_MESSAGE
 *   Peer B -> Peer A: ACK
 *
 * File Transfer:
 *   Peer A -> Peer B: COMMAND_FILEASK (request specific chunk)
 *   Peer B -> Peer A: ACK
 *   Peer B -> Peer A: COMMAND_FILE (chunk data)
 *   Peer A -> Peer B: ACK
 *
 * Authentication (RSA challenge-response):
 *   Tracker -> Peer: COMMAND_AUTH (encrypted challenge)
 *   Peer -> Tracker: ACK
 *   Peer -> Tracker: COMMAND_AUTH_REPLY (decrypted challenge proves identity)
 *   Tracker -> Peer: ACK
 *
 * Reachability Check:
 *   Peer A -> Peer B: COMMAND_QUERY (asking about Peer C)
 *   Peer B -> Peer A: ACK
 *   Peer B -> Peer A: COMMAND_QUERY_REPLY (yes/no Peer C is reachable)
 *
 * Latency Measurement:
 *   Peer A -> Peer B: COMMAND_PING (with timestamp)
 *   Peer B -> Peer A: ACK
 *   Peer B -> Peer A: COMMAND_PONG (echo timestamp back)
 *   Peer A calculates RTT from timestamp difference
 */
enum command_e {
    COMMAND_NONE                        = 0,  /**< Invalid/uninitialized command */
    COMMAND_ACK,                              /**< Acknowledgment - confirms receipt of a packet */
    COMMAND_TRACKER_ANNOUNCE_TRACKER,         /**< Tracker -> Peer: "here's your external IP and I'm your tracker" */
    COMMAND_TRACKER_ANNOUNCE_PEER,            /**< Tracker -> Peer: "here's info about another peer in the network" */
    COMMAND_PEER_ANNOUNCE_PEER,               /**< Peer -> Tracker: "I want to join, here's my info" */
    COMMAND_MESSAGE                     = 5,  /**< Generic text message between peers */
    COMMAND_FILE,                             /**< File chunk transfer */
    COMMAND_FILEASK,                          /**< Request for a specific file chunk (by hash) */
    COMMAND_AUTH,                             /**< Authentication challenge (RSA encrypted) */
    COMMAND_AUTH_REPLY,                       /**< Authentication response (decrypted challenge) */
    COMMAND_PING                        = 10, /**< Latency probe (one-way, no ACK retry) */
    COMMAND_PONG,                             /**< Latency probe response */
    COMMAND_TCP,                              /**< TCP-over-UDP tunnel data (deprecated/unused?) */
    COMMAND_QUERY,                            /**< Ask peer about another peer's reachability */
    COMMAND_QUERY_REPLY,                      /**< Response to reachability query */
};

/**
 * @brief TCP tunneling request types.
 *
 * Used for tunneling TCP connections over the UDP P2P network.
 * This allows peers behind NAT to expose TCP services.
 */
#define TCP_NONE     0  /**< Not a TCP tunnel packet */
#define TCP_REQUEST  1  /**< Outgoing TCP data (client -> tunnel -> remote) */
#define TCP_RESPONSE 2  /**< Incoming TCP data (remote -> tunnel -> client) */

/**
 * @brief TCP tunnel metadata.
 *
 * When tunneling TCP over the P2P network, this structure tracks
 * the connection state for multiplexing multiple TCP streams.
 */
struct tcp_s {
    unsigned short reqtype;  /**< TCP_NONE, TCP_REQUEST, or TCP_RESPONSE */
    unsigned short cidx;     /**< Client connection index (file descriptor on tunnel server) */
    struct {
        unsigned short src;  /**< Local port on tunnel initiator */
        unsigned short dst;  /**< Target port on remote peer */
    } port;
};

/**
 * @brief Packet header structure.
 *
 * This header is present at the start of every packet and contains
 * all metadata needed for routing, reassembly, and processing.
 *
 * FIELD PURPOSES:
 *
 * Packet Identification & Reassembly:
 *   - pidx: Packet index within a group (0, 1, 2, ... for chunked messages)
 *   - gidx: Group index - all packets with same gidx belong together
 *   - chunks: Total number of packets in this group (for reassembly)
 *   - offset: Byte offset where this packet's payload goes in reassembled data
 *   - length: Actual payload bytes in this packet (may be < UDP_PACKET_PAYLOAD for last chunk)
 *
 * Reliability:
 *   - tidx: Transaction/task index for correlating related operations
 *   - parts: Number of parts in a multi-file transaction
 *   - ts: Timestamp for RTT calculation and ordering
 *
 * Routing:
 *   - src: Original sender (may differ from UDP source if proxied)
 *   - dst: Final destination (may differ from UDP dest if proxied)
 *   - command: What type of message this is
 *
 * File Transfer:
 *   - filename: SHA256 hash identifying the file (for COMMAND_FILE/COMMAND_FILEASK)
 *
 * TCP Tunneling:
 *   - tcp: Tunnel connection metadata
 */
struct header_s {
    unsigned int   pidx;     /**< Packet index within group (0-based) */
    unsigned int   gidx;     /**< Group index - identifies a complete message */
    unsigned int   tidx;     /**< Task/transaction index for multi-packet operations */
    uint64_t       offset;   /**< Byte offset in reassembled payload */
    unsigned int   chunks;   /**< Total chunks in this group */
    unsigned int   parts;    /**< Total parts in transaction (for multi-file ops) */
    unsigned int   length;   /**< Payload length in this packet */
    enum command_e command;  /**< Protocol command type */
    unsigned char  filename[SHA256HEX];  /**< File identifier (SHA256 hex string) */
    double         ts;       /**< Timestamp (milliseconds since epoch) */
    struct tcp_s   tcp;      /**< TCP tunnel metadata */
    struct {
        int            host; /**< Original source IP (network byte order) */
        unsigned short port; /**< Original source port */
    } src;
    struct {
        int            host; /**< Final destination IP (network byte order) */
        unsigned short port; /**< Final destination port */
    } dst;
};

/**
 * @brief Complete packet structure.
 *
 * WIRE FORMAT:
 * +------------------+-------------------+------------------+
 * | header_s         | payload           | CRC32 hash       |
 * | (variable size)  | (0 to PAYLOAD_MAX)| (8 bytes hex)    |
 * +------------------+-------------------+------------------+
 *
 * The hash covers header + payload (not itself) and is used to
 * detect corruption. CRC32 is used instead of SHA256 for speed.
 */
struct packet_s {
    struct header_s header;
    struct {
        char payload[UDP_PACKET_PAYLOAD];  /**< Message payload (variable length, up to max) */
        char hash[CRC32_SIZE];             /**< CRC32 hash of header + payload (hex encoded) */
    } buffer;
};

struct send_buffer_s;

/**
 * @brief Packet handling module interface.
 *
 * Provides functions for:
 * - validate: Check packet integrity (hash verification)
 * - clean: Free packet memory
 * - serialize: Convert data into packets for sending
 * - deserialize: Reconstruct data from received packets
 * - tcpsend/tcpsent: TCP tunnel packet management
 * - dump: Debug output
 */
struct module_packet_s {
    /**
     * Validate a received packet.
     * Checks that the SHA256 hash matches header + payload.
     * @param buffer Raw received data
     * @param nbuffer Length of received data
     * @param valid Output: true if packet is valid
     * @param p Output: Parsed packet structure
     */
    int (*validate)(char *buffer, size_t nbuffer, bool *valid, struct packet_s *p);

    /** Free packet memory */
    int (*clean)(void *p);

    /** Check if a TCP tunnel packet was already sent */
    int (*tcpsent)(struct gc_gen_client_s *c, int tidx, bool *sent);

    /** Queue TCP tunnel data for sending */
    int (*tcpsend)(struct gc_gen_client_s *c, char *buf, int len, int tidx);

    /** Print packet contents for debugging */
    void (*dump)(struct packet_s *p);

    struct {
        /**
         * Serialize data into packets for transmission.
         * Handles chunking of large messages into multiple packets.
         * @param p Peer context
         * @param cmd Command type
         * @param buffer Data to send
         * @param nbuffer Data length
         * @param packets Output: Array of packets
         * @param npackets Output: Number of packets created
         * @param sb Send buffer (tracks pidx/gidx counters)
         * @param tidx Transaction index
         * @param parts Total parts count
         * @param filename File hash (for file commands)
         * @param tcp TCP tunnel info (if applicable)
         * @param host Destination IP
         * @param port Destination port
         */
        int (*init)(struct peer_s *p, enum command_e, char *buffer, int nbuffer,
                    struct packet_s **packets, int *npackets,
                    struct send_buffer_s *sb,
                    unsigned int tidx, unsigned int parts,
                    unsigned char *filename,
                    struct tcp_s *tcp, int host, unsigned short port);

        /** Validate a batch of serialized packets */
        int (*validate)(struct packet_s *packets, size_t nbuffer, bool *valid);
    } serialize;

    struct {
        /** Initial validation of incoming raw data */
        int (*init)(char *buffer, size_t nbuffer, bool *valid);
    } deserialize;
};

extern const struct module_packet_s packet;

#endif
