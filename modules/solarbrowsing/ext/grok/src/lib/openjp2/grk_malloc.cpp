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
*
*    This source code incorporates work covered by the following copyright and
*    permission notice:
*
 * The copyright in this software is being made available under the 2-clauses
 * BSD License, included below. This software may be subject to other third
 * party and contributor rights, including patent rights, and no such rights
 * are granted under this license.
 *
 * Copyright (c) 2015, Mathieu Malaterre <mathieu.malaterre@gmail.com>
 * Copyright (c) 2015, Matthieu Darbois
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS `AS IS'
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */
#define OPJ_SKIP_POISON
#include "grk_includes.h"

#if defined(OPJ_HAVE_MALLOC_H) && defined(OPJ_HAVE_MEMALIGN)
# include <malloc.h>
#endif

#ifndef SIZE_MAX
# define SIZE_MAX ((size_t) -1)
#endif

static inline void *grk_aligned_alloc_n(size_t alignment, size_t size)
{
    void* ptr;

    /* alignment shall be power of 2 */
    assert( (alignment != 0U) && ((alignment & (alignment - 1U)) == 0U));
    /* alignment shall be at least sizeof(void*) */
    assert( alignment >= sizeof(void*));

    if (size == 0U) { /* prevent implementation defined behavior of realloc */
        return NULL;
    }

#if defined(OPJ_HAVE_POSIX_MEMALIGN)
    /* aligned_alloc requires c11, restrict to posix_memalign for now. Quote:
     * This function was introduced in POSIX 1003.1d. Although this function is
     * superseded by aligned_alloc, it is more portable to older POSIX systems
     * that do not support ISO C11.  */
    if (posix_memalign (&ptr, alignment, size)) {
        ptr = NULL;
    }
    /* older linux */
#elif defined(OPJ_HAVE_MEMALIGN)
    ptr = memalign( alignment, size );
    /* _MSC_VER */
#elif defined(OPJ_HAVE__ALIGNED_MALLOC)
    ptr = _aligned_malloc(size, alignment);
#else
    /*
     * Generic aligned malloc implementation.
     * Uses size_t offset for the integer manipulation of the pointer,
     * as uintptr_t is not available in C89 to do
     * bitwise operations on the pointer itself.
     */
    alignment--;
    {
        size_t offset;
        uint8_t *mem;

        /* Room for padding and extra pointer stored in front of allocated area */
        size_t overhead = alignment + sizeof(void *);

        /* let's be extra careful */
        assert(alignment <= (SIZE_MAX - sizeof(void *)));

        /* Avoid integer overflow */
        if (size > (SIZE_MAX - overhead)) {
            return NULL;
        }

        mem = (uint8_t*)malloc(size + overhead);
        if (mem == NULL) {
            return mem;
        }
        /* offset = ((alignment + 1U) - ((size_t)(mem + sizeof(void*)) & alignment)) & alignment; */
        /* Use the fact that alignment + 1U is a power of 2 */
        offset = ((alignment ^ ((size_t)(mem + sizeof(void*)) & alignment)) + 1U) & alignment;
        ptr = (void *)(mem + sizeof(void*) + offset);
        ((void**) ptr)[-1] = mem;
    }
#endif
    return ptr;
}
static inline void *grk_aligned_realloc_n(void *ptr, size_t alignment, size_t new_size)
{
    void *r_ptr;

    /* alignment shall be power of 2 */
    assert( (alignment != 0U) && ((alignment & (alignment - 1U)) == 0U));
    /* alignment shall be at least sizeof(void*) */
    assert( alignment >= sizeof(void*));

    if (new_size == 0U) { /* prevent implementation defined behavior of realloc */
        return NULL;
    }

    /* no portable aligned realloc */
#if defined(OPJ_HAVE_POSIX_MEMALIGN) || defined(OPJ_HAVE_MEMALIGN)
    /* glibc doc states one can mix aligned malloc with realloc */
    r_ptr = realloc( ptr, new_size ); /* fast path */
    /* we simply use `size_t` to cast, since we are only interest in binary AND
     * operator */
    if( ((size_t)r_ptr & (alignment - 1U)) != 0U ) {
        /* this is non-trivial to implement a portable aligned realloc, so use a
         * simple approach where we do not need a function that return the size of an
         * allocated array (eg. _msize on Windows, malloc_size on MacOS,
         * malloc_usable_size on systems with glibc) */
        void *a_ptr = grk_aligned_alloc_n(alignment, new_size);
        if (a_ptr != NULL) {
            memcpy(a_ptr, r_ptr, new_size);
        }
        free( r_ptr );
        r_ptr = a_ptr;
    }
    /* _MSC_VER */
#elif defined(OPJ_HAVE__ALIGNED_MALLOC)
    r_ptr = _aligned_realloc( ptr, new_size, alignment );
#else
    if (ptr == NULL) {
        return grk_aligned_alloc_n(alignment, new_size);
    }
    alignment--;
    {
        void *oldmem;
        uint8_t *newmem;
        size_t overhead = alignment + sizeof(void *);

        /* let's be extra careful */
        assert(alignment <= (SIZE_MAX - sizeof(void *)));

        /* Avoid integer overflow */
        if (new_size > SIZE_MAX - overhead) {
            return NULL;
        }

        oldmem = ((void**) ptr)[-1];
        newmem = (uint8_t*)realloc(oldmem, new_size + overhead);
        if (newmem == NULL) {
            return newmem;
        }

        if (newmem == oldmem) {
            r_ptr = ptr;
        } else {
            size_t old_offset;
            size_t new_offset;

            /* realloc created a new copy, realign the copied memory block */
            old_offset = (size_t)((uint8_t*)ptr - (uint8_t*)oldmem);

            /* offset = ((alignment + 1U) - ((size_t)(mem + sizeof(void*)) & alignment)) & alignment; */
            /* Use the fact that alignment + 1U is a power of 2 */
            new_offset  = ((alignment ^ ((size_t)(newmem + sizeof(void*)) & alignment)) + 1U) & alignment;
            new_offset += sizeof(void*);
            r_ptr = (void *)(newmem + new_offset);

            if (new_offset != old_offset) {
                memmove(newmem + new_offset, newmem + old_offset, new_size);
            }
            ((void**) r_ptr)[-1] = newmem;
        }
    }
#endif
    return r_ptr;
}
void * grk_malloc(size_t size)
{
    if (size == 0U) { /* prevent implementation defined behavior of realloc */
        return NULL;
    }
    return malloc(size);
}
void * grk_calloc(size_t num, size_t size)
{
	if (num == 0 || size == 0) {
		/* prevent implementation defined behavior of realloc */
		return NULL;
	}
	return calloc(num, size);
}

void *grk_aligned_malloc(size_t size)
{
    return grk_aligned_alloc_n(16U, size);
}
void * grk_aligned_realloc(void *ptr, size_t size)
{
    return grk_aligned_realloc_n(ptr, 16U, size);
}

void grk_aligned_free(void* ptr)
{
#if defined(OPJ_HAVE_POSIX_MEMALIGN) || defined(OPJ_HAVE_MEMALIGN)
    free( ptr );
#elif defined(OPJ_HAVE__ALIGNED_MALLOC)
    _aligned_free( ptr );
#else
    /* Generic implementation has malloced pointer stored in front of used area */
    if (ptr != NULL) {
        free(((void**) ptr)[-1]);
    }
#endif
}

void * grk_realloc(void *ptr, size_t new_size)
{
    if (new_size == 0U) { /* prevent implementation defined behavior of realloc */
        return NULL;
    }
    return realloc(ptr, new_size);
}
void grk_free(void *ptr)
{
	if (ptr)
		free(ptr);
}
