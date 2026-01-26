/**
 * @file data.h
 * @brief Data serialization layer for the skypeernet protocol.
 *
 * SERIALIZATION SYSTEM
 * ====================
 *
 * This module provides the binary serialization primitives used to
 * construct protocol payloads. It handles:
 *
 *   1. Building payloads from primitive types (int, short, double, raw bytes)
 *   2. Converting payloads to packets (via packet layer)
 *   3. Submitting packets to the network queue
 *   4. Deserializing received packets back to data
 *
 * WIRE FORMAT
 * ===========
 *
 * The protocol uses native byte order (not network byte order!) for
 * all multi-byte integers. This works because both peers run on similar
 * architectures, but would cause issues with mixed-endian networks.
 *
 * Type sizes on wire:
 *   - double:    8 bytes (DATA_SIZE_DOUBLE)
 *   - int:       4 bytes (DATA_SIZE_INT)
 *   - short:     2 bytes (DATA_SIZE_SHORT)
 *   - byte/char: 1 byte  (DATA_SIZE_BYTE)
 *
 * Raw data is written without length prefix - the receiver must know
 * the expected size from context or a preceding length field.
 *
 * SENDING DATA FLOW
 * =================
 *
 *   1. data.init() - Set up data_s with command type, allocate buffer
 *      - Calls size callback to determine buffer size
 *      - Calls write callback to populate buffer
 *
 *   2. data.send() - Hand off to packet layer
 *      - Calls packet.serialize.init() to chunk data into packets
 *      - Calls data_submit() for each packet
 *
 *   3. data_submit() - Queue packet for transmission
 *      - Creates nb_s (network buffer) structure
 *      - Sets NET_INIT (retryable) or NET_ONESHOT (fire-and-forget) status
 *      - Adds to send queue (instant queue for ACK/PING/PONG)
 *
 * PACKET SERIALIZATION FORMAT
 * ===========================
 *
 * packet_set() serializes header fields individually:
 *
 *   +--------+--------+--------+----------+----------+--------+----------+---------+
 *   | pidx   | gidx   | tidx   | offset   | chunks   | parts  | length   | command |
 *   | (4)    | (4)    | (4)    | (8)      | (4)      | (4)    | (4)      | (4)     |
 *   +--------+--------+--------+----------+----------+--------+----------+---------+
 *   | filename (64)                       | tcp.reqtype| tcp.cidx | tcp.port.src  |
 *   +-------------------------------------+------------+----------+---------------+
 *   | tcp.port.dst | src.host | src.port | dst.host | dst.port | ts (8)          |
 *   +--------------+----------+----------+----------+----------+-----------------+
 *   | payload (0 to UDP_PACKET_PAYLOAD)                         | hash (64)      |
 *   +-----------------------------------------------------------+----------------+
 *
 * Note: This manual field-by-field serialization (rather than memcpy of struct)
 * could be for alignment/portability, though native byte order limits portability.
 *
 * NETWORK BUFFER STATUSES
 * =======================
 *
 *   NET_INIT    - Normal packet, will be retried if not ACKed
 *   NET_ONESHOT - Fire-and-forget (ACK, PING, PONG)
 *
 * ACK, PING, and PONG use NET_ONESHOT because:
 *   - ACK: Retrying an ACK creates infinite ACK loops
 *   - PING/PONG: Latency probes are informational, loss is acceptable
 */

#ifndef DATA_H_
#define DATA_H_

/** Size of double on wire (8 bytes) */
#define DATA_SIZE_DOUBLE (sizeof(double))
/** Size of int on wire (4 bytes) */
#define DATA_SIZE_INT    (sizeof(int))
/** Size of short on wire (2 bytes) */
#define DATA_SIZE_SHORT  (sizeof(short))
/** Size of byte/char on wire (1 byte) */
#define DATA_SIZE_BYTE   (sizeof(char))

/**
 * @brief Data buffer for payload construction.
 *
 * Used during serialization to accumulate payload bytes
 * before handing off to the packet layer.
 */
struct data_s {
    enum command_e command;  /**< Command type being serialized */
    sn             payload;  /**< Payload buffer (see sn.h for sn type) */
};

/**
 * @brief Data serialization module interface.
 */
struct module_data_s {
    /**
     * Initialize a data buffer for sending.
     *
     * @param d            Output data structure
     * @param cmd          Command type
     * @param callback     Write function to populate payload
     * @param callback_size Size function to calculate payload size
     * @param userdata     Context passed to callbacks (usually peer_s*)
     *
     * Flow:
     *   1. Calls callback_size to get required buffer size
     *   2. Allocates payload buffer
     *   3. Calls callback to write data into buffer
     */
    int (*init)(struct data_s *d, enum command_e cmd,
                int (*callback)(struct data_s*, void*),
                int (*callback_size)(int*, void*),
                void *userdata);

    /**
     * Send data to a remote peer.
     *
     * @param d        Initialized data buffer
     * @param ins      Peer context
     * @param host     Destination IP
     * @param port     Destination port
     * @param tidx     Transaction index (for correlation)
     * @param parts    Total parts (for multi-file ops)
     * @param filename File hash (for file commands)
     * @param tcp      TCP tunnel metadata
     *
     * Handles packet chunking and queue submission internally.
     */
    int (*send)(struct data_s *d, struct peer_s *ins,
                int host, unsigned short port,
                unsigned int tidx, unsigned int parts,
                unsigned char *filename, struct tcp_s *tcp);

    /**
     * Deserialize a packet into packet_s structure.
     *
     * @param p       Output packet structure
     * @param buffer  Raw received bytes
     * @param nbuffer Number of bytes
     */
    int (*get)(struct packet_s *p, char *buffer, int nbuffer);

    /**
     * Get total serialized size of a data buffer.
     *
     * @param d   Data buffer
     * @param sz  Output size
     */
    int (*size)(struct data_s *d, size_t *sz);

    /**
     * Submit a packet to the send queue.
     *
     * @param p    Peer context
     * @param pck  Packet to send
     * @param host Destination IP
     * @param port Destination port
     *
     * Creates an nb_s (network buffer) and adds it to the appropriate
     * send queue (instant for ACK/PING/PONG, normal for others).
     */
    int (*submit)(struct peer_s *p, struct packet_s *pck,
                  int host, unsigned short port);

    /**
     * Write primitives for building payloads.
     *
     * All write functions append to the data buffer at current offset.
     * No length prefixes are added - raw bytes only.
     */
    struct {
        /** Write 8-byte double */
        int (*tdouble)(struct data_s *d, const double src);
        /** Write 4-byte integer */
        int (*integer)(struct data_s *d, const int src);
        /** Write 2-byte short */
        int (*shortint)(struct data_s *d, const short src);
        /** Write 1-byte char */
        int (*byte)(struct data_s *d, const char src);
        /** Write raw bytes (no length prefix) */
        int (*raw)(struct data_s *d, char *src, const int nsrc);
    } write;
};

extern const struct module_data_s data;

#endif
