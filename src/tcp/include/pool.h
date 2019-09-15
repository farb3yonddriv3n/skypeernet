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
#ifndef GC_POOL_H_
#define GC_POOL_H_

#define POOL_DEBUG
#define POOL_STDLIB

/**
 * @brief Pool structure.
 *
 * Generic pool structure.
 */
struct hm_pool_s {
    int size;                       /**< Pool size. */
    int used;                       /**< Used nodes. */
    struct hm_log_s *log;           /**< Log stream. */
    struct pool_node_s *freenode;   /**< List of free nodes. */
    struct pool_bucket_s *buckets;  /**< List of buckets. */
    struct hm_pool_s *next;         /**< Next pool in linked list. */
};

/**
 * @brief Pool node structure.
 *
 * Internal pool structure.
 */
struct pool_node_s {
    void *ptr;                      /**< Node memory region. */
    int size;                       /**< Usable size. */
    int realsize;                   /**< Real size. */
    int used;                       /**< Usage indicator. */
    struct hm_pool_s *pool;         /**< Parent pool. */
    struct pool_node_s *next;       /**< Next node in linked list. */
};

/**
 * @brief Create new pool.
 *
 * @return Pool structure on success or NULL on error.
 */
struct hm_pool_s *hm_create_pool();

/**
 * @brief Allocate memory from pool.
 *
 * @param pool Pool structure.
 * @param size Size of allocated block.
 * @return Memory pointer on success or NULL on error.
 */
void *hm_palloc(struct hm_pool_s *pool, int size);

/**
 * @brief Reallocate memory from pool.
 *
 * It shares some similiarities with realloc().
 *
 * @param pool Pool structure.
 * @param ptr Memory pointer.
 * @param size New size.
 * @return Memory pointer on success or NULL on error.
 * @see realloc()
 */
void *hm_prealloc(struct hm_pool_s *pool, void *ptr, const int size);

/**
 * @brief Free memory.
 *
 * @param pool Pool structure.
 * @param ptr Memory region.
 * @return 0 on success, -1 on failure.
 */
int hm_pfree(struct hm_pool_s *pool, void *ptr);

/**
 * @brief Destroy pool.
 *
 * @param pool Pool structure.
 * @return 0 on success, -1 on failure.
 */
int hm_destroy_pool(struct hm_pool_s *pool);

#endif
