/*
 *
 * GrizzlyCloud library - simplified VPN alternative for IoT
 * Copyright (C) 2017 - 2018 Filip Pancik
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 */
#ifndef GC_RINGBUFFER_H
#define GC_RINGBUFFER_H

#define RB_SLOT_SIZE    (32 * 1024)

/**
 * @brief Ringbuffer slot specification.
 *
 */
struct gc_ringbuffer_slot_s {
    void   *buf;                       /**< Actual data. */
    int    len;                        /**< Data length. */
    int    sent;                       /**< Amount of data already sent. */
    struct gc_ringbuffer_slot_s *next; /**< Next slot in linked list. */
};

/**
 * @brief Core ringbuffer structure.
 *
 * Used for both, receiving and sending data.
 */
struct gc_ringbuffer_s {
    struct {
        char tmp[RB_SLOT_SIZE];        /**< Temporary buffer to receive data. */
        void *buf;                     /**< Dynamically allocated storage built of tmp parts. */
        int  len;                      /**< Length of storage. */
        int  target;                   /**< Target length to receive. */
    } recv;

    struct gc_ringbuffer_slot_s *send; /**< Linked list of slot buffers to send. */
    struct gc_ringbuffer_slot_s *tail; /**< Pointer to last slot in linked list for fast access. */
};

/**
 * @brief Get next buffer that is ready to be sent.
 *
 * @param rb Ringbuffer structure.
 * @param size Size of data.
 * @return Pointer to data.
 */
char *gc_ringbuffer_send_next(struct gc_ringbuffer_s *rb, int *size);

/**
 * @brief Mark data being already sent.
 *
 * @param pool Memory pool.
 * @param rb Ringbuffer structure.
 * @param offset Number of bytes already sent.
 * @return void.
 */
void gc_ringbuffer_send_skip(struct hm_pool_s *pool, struct gc_ringbuffer_s *rb,
                             int offset);

/**
 * @brief Check if there is anything to send.
 *
 * @param rb Ringbuffer structure.
 * @return 1 if empty, 0 if not empty.
 */
int gc_ringbuffer_send_is_empty(struct gc_ringbuffer_s *rb);

/**
 * @brief Clear send buffers.
 *
 * @param pool Memory pool.
 * @param rb Ringbuffer structure.
 * @return void.
 */
void gc_ringbuffer_send_pop_all(struct hm_pool_s *pool, struct gc_ringbuffer_s *rb);

/**
 * @brief Append data for sending.
 *
 * @param pool Memory pool.
 * @param rb Ringbuffer structure.
 * @param buf Data pointer.
 * @param len Length of data.
 * @return GC_OK on success, GC_ERROR on failure.
 */
int gc_ringbuffer_send_append(struct hm_pool_s *pool,
                              struct gc_ringbuffer_s *rb,
                              char *buf, const int len);

/**
 * @brief Total bytes to send.
 *
 * @param rb Ringbuffer structure.
 * @return Number of bytes.
 */
int gc_ringbuffer_send_size(struct gc_ringbuffer_s *rb);

/**
 * @brief Append received data.
 *
 * Take data from temporary buffer and copy them to storage.
 *
 * @param pool Memory pool.
 * @param rb Ringbuffer structure.
 * @param len Data length.
 * @return Number of bytes.
 */
void gc_ringbuffer_recv_append(struct hm_pool_s *pool, struct gc_ringbuffer_s *rb,
                               const int len);

/**
 * @brief Obtain received data.
 *
 * @param rb Ringbuffer structure.
 * @param size Size of received data.
 * @return Pointer to received data.
 */
char *gc_ringbuffer_recv_read(struct gc_ringbuffer_s *rb, int *size);

/**
 * @brief Release received data.
 *
 * @param pool Memory pool.
 * @param rb Ringbuffer structure.
 * @return void.
 */
void gc_ringbuffer_recv_pop(struct hm_pool_s *pool, struct gc_ringbuffer_s *rb);

/**
 * @brief Check if buffer is full.
 *
 * @param rb Ringbuffer structure.
 * @return 1 if full, 0 if not full.
 */
int gc_ringbuffer_recv_is_full(struct gc_ringbuffer_s *rb);

#endif
