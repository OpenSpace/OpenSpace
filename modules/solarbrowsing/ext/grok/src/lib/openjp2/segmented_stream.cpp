/*
*    Copyright (C) 2016-2017 Grok Image Compression Inc.
*
*    This source code is free software: you can redistribute it and/or  modify
*    it under the terms of the GNU Affero General Public License, version 3,
*    as published by the Free Software Foundation.
*
*    This source code is distributed in the hope that it will be useful,
*    but WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*    GNU Affero General Public License for more details.
*
*    You should have received a copy of the GNU Affero General Public License
*    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*
*/

#include "grk_includes.h"

/* #define DEBUG_SEG_BUF */


bool grk_min_buf_vec_copy_to_contiguous_buffer(opj_vec_t* min_buf_vec, uint8_t* buffer)
{
    int32_t i = 0;
    size_t offset = 0;

    if (!buffer || !min_buf_vec)
        return false;

    for (i = 0; i < min_buf_vec->size(); ++i) {
        grk_min_buf_t* seg = (grk_min_buf_t*)min_buf_vec->get(i);
        if (seg->len)
            memcpy(buffer + offset, seg->buf, seg->len);
        offset += seg->len;
    }
    return true;

}

bool grk_min_buf_vec_push_back(opj_vec_t* buf_vec, uint8_t* buf, uint16_t len)
{
    grk_min_buf_t* seg = NULL;
    if (!buf_vec || !buf || !len)
        return false;

    if (!buf_vec->data) {
        buf_vec->init();
    }

    seg = (grk_min_buf_t*)grk_malloc(sizeof(grk_buf_t));
    if (!seg)
        return false;

    seg->buf = buf;
    seg->len = len;
    if (!buf_vec->push_back(seg)) {
        grk_free(seg);
        return false;
    }

    return true;
}

uint16_t grk_min_buf_vec_get_len(opj_vec_t* min_buf_vec)
{
    size_t i = 0;
    uint16_t len = 0;
    if (!min_buf_vec || !min_buf_vec->data)
        return 0;
    for (i = 0; i < min_buf_vec->size(); ++i) {
        grk_min_buf_t* seg = (grk_min_buf_t*)min_buf_vec->get(i);
        if (seg)
            len += seg->len;
    }
    return len;

}



/*--------------------------------------------------------------------------*/

/*  Segmented Buffer Stream */

opj_seg_buf_t::opj_seg_buf_t() : data_len(0), cur_seg_id(0) {
}

opj_seg_buf_t::~opj_seg_buf_t()  {
	for (auto& seg : segments) {
		if (seg) {
			grk_buf_free(seg);
		}
	}
}



static void opj_seg_buf_increment(opj_seg_buf_t * seg_buf)
{
    grk_buf_t* cur_seg = NULL;
    if (seg_buf == NULL ||	seg_buf->cur_seg_id == seg_buf->segments.size()-1) {
        return;
    }

    cur_seg = seg_buf->segments[seg_buf->cur_seg_id];
    if ((size_t)cur_seg->offset == cur_seg->len  &&
            seg_buf->cur_seg_id < seg_buf->segments.size() -1)	{
        seg_buf->cur_seg_id++;
    }
}

static size_t opj_seg_buf_read(opj_seg_buf_t * seg_buf,
                               void * p_buffer,
                               size_t p_nb_bytes)
{
    size_t bytes_in_current_segment;
    size_t bytes_to_read;
    size_t total_bytes_read;
    size_t bytes_left_to_read;
    size_t bytes_remaining_in_file;

    if (p_buffer == NULL || p_nb_bytes == 0 || seg_buf == NULL)
        return 0;

    /*don't try to read more bytes than are available */
    bytes_remaining_in_file = seg_buf->data_len - (size_t)opj_seg_buf_get_global_offset(seg_buf);
    if (p_nb_bytes > bytes_remaining_in_file) {
#ifdef DEBUG_SEG_BUF
        printf("Warning: attempt to read past end of segmented buffer\n");
#endif
        p_nb_bytes = bytes_remaining_in_file;
    }

    total_bytes_read = 0;
    bytes_left_to_read = p_nb_bytes;
    while (bytes_left_to_read > 0 && seg_buf->cur_seg_id < seg_buf->segments.size()) {
        grk_buf_t* cur_seg = seg_buf->segments[seg_buf->cur_seg_id];
        bytes_in_current_segment = (cur_seg->len - (size_t)cur_seg->offset);

        bytes_to_read = (bytes_left_to_read < bytes_in_current_segment) ?
                        bytes_left_to_read : bytes_in_current_segment;

        if (p_buffer) {
            memcpy((uint8_t*)p_buffer + total_bytes_read,cur_seg->buf + cur_seg->offset, bytes_to_read);
        }
        opj_seg_buf_incr_cur_seg_offset(seg_buf,(int64_t)bytes_to_read);

        total_bytes_read	+= bytes_to_read;
        bytes_left_to_read	-= bytes_to_read;


    }
    return total_bytes_read ? total_bytes_read : (size_t)-1;
}

/* Disable this method for now, since it is not needed at the moment */
#if 0
static int64_t opj_seg_buf_skip(int64_t p_nb_bytes, opj_seg_buf_t * seg_buf)
{
    size_t bytes_in_current_segment;
    size_t bytes_remaining;

    if (!seg_buf)
        return p_nb_bytes;

    if (p_nb_bytes + opj_seg_buf_get_global_offset(seg_buf)> (int64_t)seg_buf->data_len) {
#ifdef DEBUG_SEG_BUF
        printf("Warning: attempt to skip past end of segmented buffer\n");
#endif
        return p_nb_bytes;
    }

    if (p_nb_bytes == 0)
        return 0;

    bytes_remaining = (size_t)p_nb_bytes;
    while (seg_buf->cur_seg_id < seg_buf->segments.size && bytes_remaining > 0) {

        grk_buf_t* cur_seg = (grk_buf_t*)opj_vec_get(&seg_buf->segments, seg_buf->cur_seg_id);
        bytes_in_current_segment = 	(size_t)(cur_seg->len -cur_seg->offset);

        /* hoover up all the bytes in this segment, and move to the next one */
        if (bytes_in_current_segment > bytes_remaining) {

            opj_seg_buf_incr_cur_seg_offset(seg_buf, bytes_in_current_segment);

            bytes_remaining	-= bytes_in_current_segment;
            cur_seg = (grk_buf_t*)opj_vec_get(&seg_buf->segments, seg_buf->cur_seg_id);
        } else { /* bingo! we found the segment */
            opj_seg_buf_incr_cur_seg_offset(seg_buf, bytes_remaining);
            return p_nb_bytes;
        }
    }
    return p_nb_bytes;
}
#endif
static grk_buf_t* opj_seg_buf_add_segment(opj_seg_buf_t* seg_buf, uint8_t* buf, size_t len)
{
    grk_buf_t* new_seg = NULL;
    if (!seg_buf)
        return NULL;
    new_seg = (grk_buf_t*)grk_malloc(sizeof(grk_buf_t));
    if (!new_seg)
        return NULL;

    memset(new_seg, 0, sizeof(grk_buf_t));
    new_seg->buf = buf;
    new_seg->len = len;
	seg_buf->segments.push_back(new_seg);

    seg_buf->cur_seg_id = (int32_t)seg_buf->segments.size() - 1;
    seg_buf->data_len += len;
    return new_seg;
}

/*--------------------------------------------------------------------------*/

void opj_seg_buf_cleanup(opj_seg_buf_t* seg_buf)
{
    size_t i;
    if (!seg_buf)
        return;
    for (i = 0; i < seg_buf->segments.size(); ++i) {
        grk_buf_t* seg = seg_buf->segments[i];
        if (seg) {
            grk_buf_free(seg);
        }
    }
    seg_buf->segments.clear();
}

void opj_seg_buf_rewind(opj_seg_buf_t* seg_buf)
{
	size_t i;
    if (!seg_buf)
        return;
    for (i = 0; i < seg_buf->segments.size(); ++i) {
        grk_buf_t* seg = seg_buf->segments[i];
        if (seg) {
            seg->offset = 0;
        }
    }
    seg_buf->cur_seg_id = 0;
}


bool opj_seg_buf_push_back(opj_seg_buf_t* seg_buf, uint8_t* buf, size_t len)
{
    grk_buf_t* seg = NULL;
    if (!seg_buf || !buf || !len)
        return false;

	seg = opj_seg_buf_add_segment(seg_buf, buf, len);
	if (!seg)
        return false;
    seg->owns_data = false;
    return true;
}

bool opj_seg_buf_alloc_and_push_back(opj_seg_buf_t* seg_buf, size_t len)
{
    grk_buf_t* seg = NULL;
    uint8_t* buf = NULL;
    if (!seg_buf || !len)
        return false;

    buf = (uint8_t*)grk_malloc(len);
    if (!buf)
        return false;

	seg = opj_seg_buf_add_segment(seg_buf, buf, len);
	if (!seg) {
	    grk_free(buf);
        return false;
    }
    seg->owns_data = true;
    return true;
}

void opj_seg_buf_incr_cur_seg_offset(opj_seg_buf_t* seg_buf, uint64_t offset)
{
    grk_buf_t* cur_seg = NULL;
    if (!seg_buf)
        return;
    cur_seg = seg_buf->segments[seg_buf->cur_seg_id];
    grk_buf_incr_offset(cur_seg, offset);
    if ((size_t)cur_seg->offset == cur_seg->len) {
        opj_seg_buf_increment(seg_buf);
    }

}


/**
* Zero copy read of contiguous chunk from current segment.
* Returns false if unable to get a contiguous chunk, true otherwise
*/
bool opj_seg_buf_zero_copy_read(opj_seg_buf_t* seg_buf,
                                uint8_t** ptr,
                                size_t chunk_len)
{
    grk_buf_t* cur_seg = NULL;
    if (!seg_buf)
        return false;
    cur_seg = seg_buf->segments[seg_buf->cur_seg_id];
    if (!cur_seg)
        return false;

    if ((size_t)cur_seg->offset + chunk_len <= cur_seg->len) {
        *ptr = cur_seg->buf + cur_seg->offset;
        opj_seg_buf_read(seg_buf, NULL, chunk_len);
        return true;
    }
    return false;
}

bool opj_seg_buf_copy_to_contiguous_buffer(opj_seg_buf_t* seg_buf, uint8_t* buffer)
{
	size_t i = 0;
    size_t offset = 0;

    if (!buffer || !seg_buf)
        return false;

    for (i = 0; i < seg_buf->segments.size(); ++i) {
        grk_buf_t* seg = seg_buf->segments[i];
        if (seg->len)
            memcpy(buffer + offset, seg->buf, seg->len);
        offset += seg->len;
    }
    return true;

}

uint8_t* opj_seg_buf_get_global_ptr(opj_seg_buf_t* seg_buf)
{
    grk_buf_t* cur_seg = NULL;
    if (!seg_buf)
        return NULL;
    cur_seg = seg_buf->segments[seg_buf->cur_seg_id];
    return (cur_seg) ? (cur_seg->buf + cur_seg->offset) : NULL;
}

size_t opj_seg_buf_get_cur_seg_len(opj_seg_buf_t* seg_buf)
{
    grk_buf_t* cur_seg = NULL;
    if (!seg_buf)
        return 0;
    cur_seg = seg_buf->segments[seg_buf->cur_seg_id];
    return (cur_seg) ? (cur_seg->len - (size_t)cur_seg->offset) : 0;
}

int64_t opj_seg_buf_get_cur_seg_offset(opj_seg_buf_t* seg_buf)
{
    grk_buf_t* cur_seg = NULL;
    if (!seg_buf)
        return 0;
    cur_seg = seg_buf->segments[seg_buf->cur_seg_id];
    return (cur_seg) ? (int64_t)(cur_seg->offset) : 0;
}


int64_t opj_seg_buf_get_global_offset(opj_seg_buf_t* seg_buf)
{
    int32_t i = 0;
    int64_t offset = 0;

    if (!seg_buf)
        return 0;

    for (i = 0; i < seg_buf->cur_seg_id; ++i) {
        grk_buf_t* seg = seg_buf->segments[i];
        offset += (int64_t)seg->len;
    }
    return offset + opj_seg_buf_get_cur_seg_offset(seg_buf);
}


void grk_buf_incr_offset(grk_buf_t* buf, uint64_t off)
{
    if (!buf)
        return;
    /*  we allow the offset to move to one location beyond end of buffer segment*/
    if (buf->offset + off > (uint64_t)buf->len) {
#ifdef DEBUG_SEG_BUF
        printf("Warning: attempt to increment buffer offset out of bounds\n");
#endif
        buf->offset = (uint64_t)buf->len;
    }
    buf->offset += off;
}

void grk_buf_free(grk_buf_t* buffer)
{
    if (!buffer)
        return;
    if (buffer->buf && buffer->owns_data)
        grk_free(buffer->buf);
    grk_free(buffer);
}

