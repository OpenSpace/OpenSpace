/**
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


#ifdef _WIN32
#include <windows.h>
#else /* _WIN32 */

#include <errno.h>

#include <stdarg.h>
#include <stdlib.h>
#include <sys/stat.h>
# include <unistd.h>
# include <sys/mman.h>

#endif

#include <fcntl.h>


#ifdef _WIN32
typedef void* grk_handle_t;
#else
typedef int32_t grk_handle_t;
#endif


typedef struct grk_buf_info {
    uint8_t *buf;
    int64_t off;
    size_t len;
    grk_handle_t fd;		// for file mapping
} grk_buf_info_t;

static void grk_free_buffer_info(void* user_data)
{
    if (user_data)
        grk_free(user_data);
}

static size_t grk_zero_copy_read_from_buffer(void ** p_buffer,
        size_t p_nb_bytes,
        grk_buf_info_t* p_source_buffer)
{
    size_t l_nb_read = 0;

    if (((size_t)p_source_buffer->off + p_nb_bytes) < p_source_buffer->len) {
        l_nb_read = p_nb_bytes;
    }

    *p_buffer = p_source_buffer->buf + p_source_buffer->off;
    p_source_buffer->off += (int64_t)l_nb_read;

    return l_nb_read ? l_nb_read : ((size_t)-1);
}

static size_t grk_read_from_buffer(void * p_buffer,
                                   size_t p_nb_bytes,
                                   grk_buf_info_t* p_source_buffer)
{
    size_t l_nb_read;

    if ((size_t)p_source_buffer->off + p_nb_bytes < p_source_buffer->len) {
        l_nb_read = p_nb_bytes;
    } else {
        l_nb_read = (p_source_buffer->len - (size_t)p_source_buffer->off);
    }
    memcpy(p_buffer, p_source_buffer->buf + p_source_buffer->off, l_nb_read);
    p_source_buffer->off += (int64_t)l_nb_read;

    return l_nb_read ? l_nb_read : ((size_t)-1);
}

static size_t grk_write_to_buffer(void * p_buffer,
                                    size_t p_nb_bytes,
                                    grk_buf_info_t* p_source_buffer)
{
    memcpy(p_source_buffer->buf + (size_t)p_source_buffer->off, p_buffer, p_nb_bytes);
    p_source_buffer->off += (int64_t)p_nb_bytes;
    return p_nb_bytes;
}

static int64_t grk_skip_from_buffer(int64_t p_nb_bytes,
                                    grk_buf_info_t * p_source_buffer)
{
    if (p_source_buffer->off + p_nb_bytes <  (int64_t)p_source_buffer->len) {
        p_source_buffer->off += p_nb_bytes;
    } else {
        p_source_buffer->off = (int64_t)p_source_buffer->len;
    }
    return p_nb_bytes;
}

static bool grk_seek_from_buffer(int64_t p_nb_bytes,
                                 grk_buf_info_t * p_source_buffer)
{
    if (p_nb_bytes <  (int64_t)p_source_buffer->len) {
        p_source_buffer->off = p_nb_bytes;
    } else {
        p_source_buffer->off = (int64_t)p_source_buffer->len;
    }
    return true;
}


static void grk_set_up_buffer_stream(opj_stream_t* l_stream, size_t len, bool p_is_read_stream)
{
    opj_stream_set_user_data_length(l_stream, len);

    if (p_is_read_stream) {
        opj_stream_set_read_function(l_stream, (opj_stream_read_fn)grk_read_from_buffer);
        opj_stream_set_zero_copy_read_function(l_stream, (opj_stream_zero_copy_read_fn)grk_zero_copy_read_from_buffer);
    } else
        opj_stream_set_write_function(l_stream, (opj_stream_write_fn)grk_write_to_buffer);
    opj_stream_set_skip_function(l_stream, (opj_stream_skip_fn)grk_skip_from_buffer);
    opj_stream_set_seek_function(l_stream, (opj_stream_seek_fn)grk_seek_from_buffer);
}

size_t grk_get_buffer_stream_offset(opj_stream_t* stream) {
	if (!stream)
		return 0;
	grk_stream_private_t * private_stream = (grk_stream_private_t*)stream;
	if (!private_stream->m_user_data)
		return 0;
	grk_buf_info_t* buf = (grk_buf_info_t*)private_stream->m_user_data;
	return buf->off;
}

opj_stream_t*  grk_create_buffer_stream(uint8_t *buf,
                                        size_t len,
                                        bool p_is_read_stream)
{
    opj_stream_t* l_stream;
    grk_buf_info_t* p_source_buffer = NULL;

    if (!buf || !len)
        return NULL;

    p_source_buffer = (grk_buf_info_t*)grk_malloc(sizeof(grk_buf_info_t));
    if (!p_source_buffer)
        return NULL;

    l_stream = opj_stream_create(0, p_is_read_stream);
    if (!l_stream) {
        grk_free(p_source_buffer);
        return NULL;

    }

    memset(p_source_buffer, 0, sizeof(grk_buf_info_t));
    p_source_buffer->buf = buf;
    p_source_buffer->off = 0;
    p_source_buffer->len = len;

    opj_stream_set_user_data(l_stream, p_source_buffer, grk_free_buffer_info);
    grk_set_up_buffer_stream(l_stream, p_source_buffer->len, p_is_read_stream);
    return l_stream;
}





int32_t grk_get_file_open_mode(const char* mode)
{
    int32_t m = -1;
    switch (mode[0]) {
    case 'r':
        m = O_RDONLY;
        if (mode[1] == '+')
            m = O_RDWR;
        break;
    case 'w':
    case 'a':
        m = O_RDWR | O_CREAT;
        if (mode[0] == 'w')
            m |= O_TRUNC;
        break;
    default:
        break;
    }
    return m;
}


#ifdef _WIN32

static uint64_t  grk_size_proc(grk_handle_t fd)
{
    ULARGE_INTEGER m;
    m.LowPart = GetFileSize(fd, &m.HighPart);
    return(m.QuadPart);
}


static void* grk_map(grk_handle_t fd, size_t len)
{
	(void)len;
    void* ptr = NULL;
    HANDLE hMapFile = NULL;

    if (!fd || !len)
        return NULL;

    /* Passing in 0 for the maximum file size indicates that we
    would like to create a file mapping object for the full file size */
    hMapFile = CreateFileMapping(fd, NULL, PAGE_READONLY, 0, 0, NULL);
    if (hMapFile == NULL) {
        return NULL;
    }
    ptr = MapViewOfFile(hMapFile, FILE_MAP_READ, 0, 0, 0);
    CloseHandle(hMapFile);
    return ptr;
}

static int32_t grk_unmap(void* ptr, size_t len)
{
    int32_t rc = -1;
    (void)len;
    if (ptr)
        rc = UnmapViewOfFile(ptr) ? 0 : -1;
    return rc;
}

static grk_handle_t grk_open_fd(const char* fname, const char* mode)
{
    void*	fd = NULL;
    int32_t m = -1;
    DWORD			dwMode = 0;

    if (!fname)
        return (grk_handle_t)-1;


    m = grk_get_file_open_mode(mode);
    switch (m) {
    case O_RDONLY:
        dwMode = OPEN_EXISTING;
        break;
    case O_RDWR:
        dwMode = OPEN_ALWAYS;
        break;
    case O_RDWR | O_CREAT:
        dwMode = OPEN_ALWAYS;
        break;
    case O_RDWR | O_TRUNC:
        dwMode = CREATE_ALWAYS;
        break;
    case O_RDWR | O_CREAT | O_TRUNC:
        dwMode = CREATE_ALWAYS;
        break;
    default:
        return NULL;
    }

    fd = (grk_handle_t)CreateFileA(fname,
                                   (m == O_RDONLY) ? GENERIC_READ : (GENERIC_READ | GENERIC_WRITE),
                                   FILE_SHARE_READ | FILE_SHARE_WRITE, NULL, dwMode,
                                   (m == O_RDONLY) ? FILE_ATTRIBUTE_READONLY : FILE_ATTRIBUTE_NORMAL,
                                   NULL);
    if (fd == INVALID_HANDLE_VALUE) {
        return (grk_handle_t)-1;
    }
    return fd;
}

static int32_t grk_close_fd(grk_handle_t fd)
{
    int32_t rc = -1;
    if (fd) {
        rc = CloseHandle(fd) ? 0 : -1;
    }
    return rc;
}

#else

static uint64_t grk_size_proc(grk_handle_t fd)
{
    struct stat sb;
    if (!fd)
        return 0;

    if (fstat(fd, &sb)<0)
        return(0);
    else
        return((uint64_t)sb.st_size);
}

static void* grk_map(grk_handle_t fd, size_t len)
{
	(void)len;
    void* ptr = NULL;
    uint64_t		size64 = 0;

    if (!fd)
        return NULL;

    size64 = grk_size_proc(fd);
    ptr = (void*)mmap(0, (size_t)size64, PROT_READ, MAP_SHARED, fd, 0);
    return ptr == (void*)-1 ? NULL : ptr;
}

static int32_t grk_unmap(void* ptr, size_t len)
{
    if (ptr)
        munmap(ptr, len);
    return 0;
}

static grk_handle_t grk_open_fd(const char* fname, const char* mode)
{
    grk_handle_t	fd = 0;
    int32_t m = -1;
    if (!fname) {
        return (grk_handle_t)-1;
    }
    m = grk_get_file_open_mode(mode);
    fd = open(fname, m, 0666);
    if (fd < 0) {
#ifdef DEBUG_ERRNO
        if (errno > 0 && strerror(errno) != NULL) {
            printf("%s: %s", fname, strerror(errno));
        } else {
            printf("%s: Cannot open", fname);
        }
#endif
        return (grk_handle_t)-1;
    }
    return fd;
}

static int32_t grk_close_fd(grk_handle_t fd)
{
    if (!fd)
        return 0;
    return  (close(fd));
}

#endif



static void grk_mem_map_free(void* user_data)
{
    if (user_data) {
        grk_buf_info_t* buffer_info = (grk_buf_info_t*)user_data;
        grk_unmap(buffer_info->buf, buffer_info->len);
        grk_close_fd(buffer_info->fd);
        grk_free(buffer_info);
    }
}

/*
Currently, only read streams are supported for memory mapped files.
*/
opj_stream_t* grk_create_mapped_file_read_stream(const char *fname)
{
    opj_stream_t*	l_stream = NULL;
    grk_buf_info_t* buffer_info = NULL;
    void*			mapped_view = NULL;
    bool p_is_read_stream = true;

    grk_handle_t	fd = grk_open_fd(fname, p_is_read_stream ? "r" : "w");
    if (fd == (grk_handle_t)-1)
        return NULL;

    buffer_info = (grk_buf_info_t*)grk_malloc(sizeof(grk_buf_info_t));
    memset(buffer_info, 0, sizeof(grk_buf_info_t));
    buffer_info->fd = fd;
    buffer_info->len = (size_t)grk_size_proc(fd);

    l_stream = opj_stream_create(0, p_is_read_stream);
    if (!l_stream) {
        grk_mem_map_free(buffer_info);
        return NULL;
    }

    mapped_view = grk_map(fd, buffer_info->len);
    if (!mapped_view) {
        opj_stream_destroy(l_stream);
        grk_mem_map_free(buffer_info);
        return NULL;
    }

    buffer_info->buf = (uint8_t*)mapped_view;
    buffer_info->off = 0;

    opj_stream_set_user_data(l_stream, buffer_info, (opj_stream_free_user_data_fn)grk_mem_map_free);
    grk_set_up_buffer_stream(l_stream, buffer_info->len, p_is_read_stream);


    return l_stream;
}
