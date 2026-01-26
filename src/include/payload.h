/**
 * @file payload.h
 * @brief Payload handling for the skypeernet protocol.
 *
 * PAYLOAD SYSTEM OVERVIEW
 * =======================
 *
 * The payload module provides the high-level interface for sending and
 * receiving protocol messages. It sits above the packet layer:
 *
 *   Application
 *       |
 *   Payload (this module) - Command dispatch, serialization callbacks
 *       |
 *   Packet - Chunking, reassembly, integrity
 *       |
 *   Network - UDP send/receive
 *
 * COMMAND HANDLER PATTERN
 * =======================
 *
 * Each command type (MESSAGE, FILE, ANNOUNCE, etc.) has four handlers:
 *
 *   1. write() - Serialize command data into payload buffer
 *   2. size()  - Calculate payload size (called before write)
 *   3. read()  - Process received command (after reassembly)
 *   4. reply() - Send automatic response (usually ACK)
 *
 * SENDING A MESSAGE
 * =================
 *
 *   1. Caller sets up send_buffer with command-specific data
 *   2. Calls payload.send(peer, COMMAND_XXX, host, port, ...)
 *   3. payload.send looks up handlers for COMMAND_XXX
 *   4. size() is called to allocate buffer
 *   5. write() serializes data into buffer
 *   6. Buffer passed to packet layer for chunking/sending
 *
 * RECEIVING A MESSAGE
 * ===================
 *
 *   1. Network layer receives UDP packets
 *   2. Packets cached by (gidx, host, port) until all chunks arrive
 *   3. When complete, payload.recv() is called
 *   4. reply() sends ACK (for most commands)
 *   5. read() processes the reassembled payload
 *   6. read() may call user callbacks (p->user.cb.xxx)
 *
 * ANNOUNCE PAYLOAD VARIANTS
 * =========================
 *
 * The announce system has multiple variants for different scenarios:
 *
 *   announce_pwp - Peer writes peer announcement (peer -> tracker)
 *   announce_twp - Tracker writes peer info to another peer
 *   announce_twt - Tracker writes tracker info (tracker -> peer)
 *
 *   announce_prp - Peer reads peer announcement (tracker received peer info)
 *   announce_trp - Tracker reads peer info (peer joined, broadcast it)
 *   announce_trt - Tracker reads tracker announcement
 *
 * ANNOUNCE PAYLOAD FORMAT
 * =======================
 *
 *   +----------+----------+-------+----------+---------+---------+-----------+--------+
 *   | host(4)  | port(2)  |proxy(1)| keylen(4)| pubkey  | ver(4)  | desclen(4)| desc   |
 *   +----------+----------+-------+----------+---------+---------+-----------+--------+
 *   | tcpport1 | tcpport2 | ...   | (up to MAX_ANNOUNCED_PORTS)                       |
 *   +----------+----------+-------+--------------------------------------------------|
 *
 *   - host: IP address (4 bytes, network order)
 *   - port: UDP port (2 bytes)
 *   - proxy: 1 if peer acts as proxy for NAT traversal
 *   - keylen: Length of public key
 *   - pubkey: RSA public key (PEM format)
 *   - ver: Protocol version
 *   - desclen: Length of TCP description string
 *   - desc: Human-readable description of TCP services
 *   - tcpports: List of exposed TCP ports
 */

#ifndef PAYLOAD_H_
#define PAYLOAD_H_

/** Maximum TCP ports a peer can announce (prevents flooding) */
#define MAX_ANNOUNCED_PORTS 8

/**
 * @brief Handler interface for payload operations.
 *
 * Each command type implements these functions for
 * serialization and size calculation.
 */
struct module_handler_s {
    /** Write payload data to buffer */
    int (*write)(struct data_s *d, void *userdata);
    /** Calculate payload size */
    int (*size)(struct data_s *d, void *userdata);
};

/**
 * @brief Main payload module interface.
 */
struct module_payload_s {
    /**
     * Send a command to a remote peer.
     *
     * @param parent   Peer context
     * @param cmd      Command type (COMMAND_MESSAGE, etc.)
     * @param host     Destination IP
     * @param port     Destination port
     * @param tidx     Transaction index (for multi-part operations)
     * @param parts    Total parts count (for file transfers)
     * @param filename File hash (for COMMAND_FILE/FILEASK)
     * @param tcp      TCP tunnel info (for TCP tunneling)
     */
    int (*send)(void *parent, enum command_e cmd,
                int host, unsigned short port,
                unsigned int tidx, unsigned int parts,
                unsigned char *filename,
                struct tcp_s *tcp);

    /**
     * Process a received (and reassembled) payload.
     * Called by network layer when all chunks of a message arrive.
     */
    int (*recv)(struct peer_s *p);

    struct {
        /**
         * Handle proxy reply (for shadow routing).
         * Used when packets need to be forwarded through a proxy peer.
         */
        int (*reply)(struct peer_s *p);
    } proxy;
};

/*
 * ANNOUNCE COMMAND HANDLERS
 * =========================
 * Peer discovery and registration.
 */

/** Peer announces itself to tracker (write handler) */
int announce_peer_write_peer(struct data_s *d, void *userdata);
/** Tracker writes peer info to another peer */
int announce_twp(struct data_s *d, void *userdata);
/** Tracker writes its own info to a peer */
int announce_twt(struct data_s *d, void *userdata);
/** Peer writes its announcement */
int announce_pwp(struct data_s *d, void *userdata);
/** Calculate announce payload size */
int announce_size(int *sz, void *userdata);
/** Tracker reads tracker announcement (processes registration confirm) */
int announce_trt(struct peer_s *p);
/** Tracker reads peer info (new peer joined network) */
int announce_trp(struct peer_s *p);
/** Peer reads peer info from tracker (learns about other peer) */
int announce_prp(struct peer_s *p);

/*
 * MESSAGE COMMAND HANDLERS
 * ========================
 * Generic text messaging between peers.
 *
 * Payload format: raw message bytes (no length prefix, use header.length)
 */

int message_write(struct data_s *d, void *userdata);
int message_size(int *sz, void *userdata);
int message_read(struct peer_s *p);

/*
 * ACK COMMAND HANDLERS
 * ====================
 * Acknowledgment for reliable delivery.
 *
 * Payload format:
 *   +-------+-------+-------+-------+----------+
 *   |ver(4) |tidx(4)|pidx(4)| cmd(4)| ts(8)    |
 *   +-------+-------+-------+-------+----------+
 *
 * The ACK echoes back the transaction index, packet index, and timestamp
 * so the sender can match it to outstanding packets and calculate RTT.
 */

int ack_write(struct data_s *d, void *userdata);
int ack_size(int *sz, void *userdata);
int ack_read(struct peer_s *ins);
/** Send ACK reply for the currently received packet */
int ack_reply(struct peer_s *ins);

/*
 * FILE COMMAND HANDLERS
 * =====================
 * File chunk transfer.
 *
 * Files are identified by SHA256 hash and split into chunks.
 * Each chunk is also identified by its own SHA256 hash.
 *
 * Payload format: raw file chunk data
 *
 * The filename field in packet header identifies the file.
 * Chunk identity is derived from content hash.
 */

int file_write(struct data_s *d, void *userdata);
int file_size(int *sz, void *userdata);
int file_read(struct peer_s *p);

/*
 * FILEASK COMMAND HANDLERS
 * ========================
 * Request a specific file chunk from a peer.
 *
 * Payload format:
 *   +----------------+----------------+
 *   | file_hash(64)  | chunk_hash(64) |
 *   +----------------+----------------+
 *
 * Both hashes are SHA256 in hex format (64 chars each).
 * Receiver looks up the chunk and sends COMMAND_FILE in response.
 */

int fileask_write(struct data_s *d, void *userdata);
int fileask_read(struct peer_s *p);
int fileask_size(int *sz, void *userdata);

/*
 * PING/PONG COMMAND HANDLERS
 * ==========================
 * Latency measurement and keepalive.
 *
 * Payload format:
 *   +----------+
 *   | ts(8)    |  (double, milliseconds)
 *   +----------+
 *
 * PING sender includes current timestamp.
 * PONG responder echoes the timestamp.
 * RTT = current_time - echoed_timestamp
 *
 * Note: PING/PONG use NET_ONESHOT status (no retry on missing ACK)
 * because lost pings are expected and don't need guaranteed delivery.
 */

int ping_write(struct data_s *d, void *userdata);
int ping_size(int *sz, void *userdata);
int ping_read(struct peer_s *p);

int pong_write(struct data_s *d, void *userdata);
int pong_read(struct peer_s *p);
int pong_size(int *sz, void *userdata);

/*
 * AUTH COMMAND HANDLERS
 * =====================
 * RSA-based peer authentication (challenge-response).
 *
 * Authentication flow:
 *   1. Tracker generates random challenge string
 *   2. Tracker encrypts challenge with peer's public key
 *   3. Tracker sends COMMAND_AUTH with encrypted challenge
 *   4. Peer decrypts with its private key
 *   5. Peer sends COMMAND_AUTH_REPLY with decrypted challenge
 *   6. Tracker verifies the response matches original challenge
 *
 * This proves the peer possesses the private key matching
 * the public key it announced.
 *
 * Payload format: RSA encrypted/decrypted data (variable length)
 */

int auth_write(struct data_s *d, void *userdata);
int auth_read(struct peer_s *p);
int auth_size(int *sz, void *userdata);

int authrpl_write(struct data_s *d, void *userdata);
int authrpl_read(struct peer_s *p);
int authrpl_size(int *sz, void *userdata);

/*
 * QUERY COMMAND HANDLERS
 * ======================
 * Ask a peer about another peer's reachability.
 *
 * Used for NAT detection and finding relay paths.
 *
 * Payload format (QUERY):
 *   +----------+----------+
 *   | host(4)  | port(2)  |
 *   +----------+----------+
 *
 * Payload format (QUERY_REPLY):
 *   +----------+----------+-------------+
 *   | host(4)  | port(2)  | reachable(1)|
 *   +----------+----------+-------------+
 */

int query_write(struct data_s *d, void *userdata);
int query_read(struct peer_s *p);
int query_size(int *sz, void *userdata);

int queryrpl_write(struct data_s *d, void *userdata);
int queryrpl_read(struct peer_s *p);
int queryrpl_size(int *sz, void *userdata);

extern const struct module_payload_s payload;

#endif
