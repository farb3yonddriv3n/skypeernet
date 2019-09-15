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
#include <gc.h>

char *gc_ringbuffer_send_next(struct gc_ringbuffer_s *rb, int *size)
{
    assert(rb);

    if (rb->send == NULL) {
        *size = 0;
        return NULL;
    }

    *size = rb->send->len - rb->send->sent;

    return (char *)(rb->send->buf + rb->send->sent);
}

static void gc_ringbuffer_next(struct hm_pool_s *pool, struct gc_ringbuffer_s *rb)
{
    struct gc_ringbuffer_slot_s *next;

    if (rb && rb->send && rb->send->sent == rb->send->len) {
        next = rb->send->next;
        hm_pfree(pool, rb->send->buf);
        hm_pfree(pool, rb->send);
        rb->send = next;
        if (rb->send == NULL) {
            rb->tail = NULL;
        }
    }
}

void gc_ringbuffer_send_skip(struct hm_pool_s *pool, struct gc_ringbuffer_s *rb,
                             int offset)
{
    assert(rb);
    assert(rb->send);
    rb->send->sent += offset;

    gc_ringbuffer_next(pool, rb);
}

int gc_ringbuffer_send_is_empty(struct gc_ringbuffer_s *rb)
{
    assert(rb);
    return (rb->send == NULL);
}

void gc_ringbuffer_send_pop_all(struct hm_pool_s *pool, struct gc_ringbuffer_s *rb)
{
    struct gc_ringbuffer_slot_s *r, *rdel;

    for (r = rb->send; r != NULL; ) {
        hm_pfree(pool, r->buf);
        rdel = r;
        r = r->next;
        hm_pfree(pool, rdel);
    }
}

int gc_ringbuffer_send_size(struct gc_ringbuffer_s *rb)
{
    int size = 0;
    struct gc_ringbuffer_slot_s *r;

    for (r = rb->send; r != NULL; r = r->next) {
        size += r->len;
    }

    return size;
}

int gc_ringbuffer_send_append(struct hm_pool_s *pool, struct gc_ringbuffer_s *rb,
                              char *buf, const int len)
{
    assert(rb);
    struct gc_ringbuffer_slot_s *slot;

    slot = hm_palloc(pool, sizeof(*slot));
    if (slot == NULL) {
        return GC_ERROR;
    }
    slot->buf = hm_palloc(pool, len);
    if (slot->buf == NULL) {
        hm_pfree(pool, slot);
        return GC_ERROR;
    }

    memcpy(slot->buf, buf, len);
    slot->len = len;
    slot->sent = 0;
    slot->next = NULL;

    if (rb->send == NULL && rb->tail == NULL) {
        rb->send = rb->tail = slot;
    } else {
        assert(rb->tail);
        rb->tail->next = slot;
        rb->tail = slot;
    }

    return GC_OK;
}

void gc_ringbuffer_recv_append(struct hm_pool_s *pool, struct gc_ringbuffer_s *rb,
                               const int len)
{
    assert(rb);
    rb->recv.buf = hm_prealloc(pool, rb->recv.buf, rb->recv.len + len);
    memcpy(rb->recv.buf + rb->recv.len, rb->recv.tmp, len);
    rb->recv.len += len;
}

void gc_ringbuffer_recv_pop(struct hm_pool_s *pool, struct gc_ringbuffer_s *rb)
{
    assert(rb);
    rb->recv.len = 0;
    rb->recv.target = 0;
    if (rb->recv.buf) {
        hm_pfree(pool, rb->recv.buf);
        rb->recv.buf = NULL;
    }
}

char *gc_ringbuffer_recv_read(struct gc_ringbuffer_s *rb, int *size)
{
    *size = rb->recv.len;
    return rb->recv.buf;
}

int gc_ringbuffer_recv_is_full(struct gc_ringbuffer_s *rb)
{
    assert(rb);
    return GC_OK;
    //return(rb->recv.len >= RB_SLOT_SIZE);
}
