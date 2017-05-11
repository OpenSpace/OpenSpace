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
 * Copyright (c) 2002-2014, Universite catholique de Louvain (UCL), Belgium
 * Copyright (c) 2002-2014, Professor Benoit Macq
 * Copyright (c) 2001-2003, David Janssens
 * Copyright (c) 2002-2003, Yannick Verschueren
 * Copyright (c) 2003-2007, Francois-Olivier Devaux
 * Copyright (c) 2003-2014, Antonin Descampe
 * Copyright (c) 2005, Herve Drolon, FreeImage Team
 * Copyright (c) 2008, 2011-2012, Centre National d'Etudes Spatiales (CNES), FR
 * Copyright (c) 2012, CS Systemes d'Information, France
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


#pragma once

/**
@file cio.h
@brief Implementation of a byte input-output process (CIO)

The functions in CIO.C have for goal to realize a byte input / output process.
*/

/** @defgroup CIO CIO - byte input-output stream */
/*@{*/

#include "opj_config_private.h"

/* ----------------------------------------------------------------------- */

#if defined(OPJ_BIG_ENDIAN)
#define grk_write_bytes		grk_write_bytes_BE
#define grk_read_bytes		grk_read_bytes_BE
#define grk_write_double	grk_write_double_BE
#define grk_read_double		grk_read_double_BE
#define grk_write_float		grk_write_float_BE
#define grk_read_float		grk_read_float_BE
#else
#define grk_write_bytes		grk_write_bytes_LE
#define grk_read_bytes		grk_read_bytes_LE
#define grk_write_double	grk_write_double_LE
#define grk_read_double		grk_read_double_LE
#define grk_write_float		grk_write_float_LE
#define grk_read_float		grk_read_float_LE
#endif


#define OPJ_STREAM_STATUS_OUTPUT  0x1U
#define OPJ_STREAM_STATUS_INPUT   0x2U
#define OPJ_STREAM_STATUS_END     0x4U
#define OPJ_STREAM_STATUS_ERROR   0x8U

/**
Byte input-output stream.
*/
struct grk_stream_private_t {
    /**
     * User data, be it files, ... The actual data depends on the type of the stream.
     */
    void *					m_user_data;

    /**
     * Pointer to function to free m_user_data (NULL at initialization)
     * when destroying the stream. If pointer is NULL the function is not
     * called and the m_user_data is not freed (even if non-NULL).
     */
    opj_stream_free_user_data_fn		m_free_user_data_fn;

    /**
     * User data length
     */
    uint64_t 				m_user_data_length;

    /**
     * Pointer to actual read function (NULL at the initialization of the cio).
     */
    opj_stream_read_fn		m_read_fn;

    /**
    * Pointer to actual zero copy read function (NULL at the initialization of the cio).
    */
    opj_stream_zero_copy_read_fn		m_zero_copy_read_fn;


    /**
     * Pointer to actual write function (NULL at the initialization of the cio.
     */
    opj_stream_write_fn		m_write_fn;

    /**
     * Pointer to actual skip function (NULL at the initialization of the cio.
     * There is no seek function to prevent from back and forth slow procedures.
     */
    opj_stream_skip_fn		m_skip_fn;

    /**
     * Pointer to actual seek function (if available).
     */
    opj_stream_seek_fn		m_seek_fn;

    /**
     * Actual data stored into the stream if read from. Data is read by chunk of fixed size.
     * you should never access this data directly.
     */
    uint8_t *					m_stored_data;

    /**
     * Pointer to the current read data.
     */
    uint8_t *					m_current_data;

    /**
    * FIXME DOC.
    */
    bool (* m_opj_skip)(grk_stream_private_t * ,
							int64_t ,
							grk_event_mgr_t *);

    /**
    * FIXME DOC.
    */
    bool (* m_opj_seek) (grk_stream_private_t * , 
						int64_t ,
						grk_event_mgr_t *);

    /**
     * number of bytes containing in the buffer.
     */
    size_t			m_bytes_in_buffer;

    /**
     * The number of bytes read/written from the beginning of the stream
     */
    int64_t			m_byte_offset;

    /**
     * The size of the buffer.
     */
    size_t			m_buffer_size;

    /**
     * Flags to tell the status of the stream.
     * Used with OPJ_STREAM_STATUS_* defines.
     */
    uint32_t m_status;

};


/** @name Exported functions (see also openjpeg.h) */
/*@{*/
/* ----------------------------------------------------------------------- */
/**
 * Write some bytes to the given data buffer, this function is used in Big Endian cpus.
 * @param p_buffer		pointer the data buffer to write data to.
 * @param p_value		the value to write
 * @param p_nb_bytes	the number of bytes to write
*/
void grk_write_bytes_BE (uint8_t * p_buffer, uint32_t p_value, uint32_t p_nb_bytes);

/**
 * Reads some bytes from the given data buffer, this function is used in Big Endian cpus.
 * @param p_buffer		pointer the data buffer to read data from.
 * @param p_value		pointer to the value that will store the data.
 * @param p_nb_bytes	the nb bytes to read.
 * @return				the number of bytes read or -1 if an error occurred.
 */
void grk_read_bytes_BE(const uint8_t * p_buffer, uint32_t * p_value, uint32_t p_nb_bytes);

/**
 * Write some bytes to the given data buffer, this function is used in Little Endian cpus.
 * @param p_buffer		pointer the data buffer to write data to.
 * @param p_value		the value to write
 * @param p_nb_bytes	the number of bytes to write
 * @return				the number of bytes written or -1 if an error occurred
*/
void grk_write_bytes_LE (uint8_t * p_buffer, uint32_t p_value, uint32_t p_nb_bytes);

/**
 * Reads some bytes from the given data buffer, this function is used in Little Endian cpus.
 * @param p_buffer		pointer the data buffer to read data from.
 * @param p_value		pointer to the value that will store the data.
 * @param p_nb_bytes	the nb bytes to read.
 * @return				the number of bytes read or -1 if an error occurred.
 */
void grk_read_bytes_LE(const uint8_t * p_buffer, uint32_t * p_value, uint32_t p_nb_bytes);


/**
 * Write some bytes to the given data buffer, this function is used in Little Endian cpus.
 * @param p_buffer		pointer the data buffer to write data to.
 * @param p_value		the value to write
 */
void grk_write_double_LE(uint8_t * p_buffer, double p_value);

/***
 * Write some bytes to the given data buffer, this function is used in Big Endian cpus.
 * @param p_buffer		pointer the data buffer to write data to.
 * @param p_value		the value to write
 */
void grk_write_double_BE(uint8_t * p_buffer, double p_value);

/**
 * Reads some bytes from the given data buffer, this function is used in Little Endian cpus.
 * @param p_buffer		pointer the data buffer to read data from.
 * @param p_value		pointer to the value that will store the data.
 */
void grk_read_double_LE(const uint8_t * p_buffer, double * p_value);

/**
 * Reads some bytes from the given data buffer, this function is used in Big Endian cpus.
 * @param p_buffer		pointer the data buffer to read data from.
 * @param p_value		pointer to the value that will store the data.
 */
void grk_read_double_BE(const uint8_t * p_buffer, double * p_value);

/**
 * Reads some bytes from the given data buffer, this function is used in Little Endian cpus.
 * @param p_buffer		pointer the data buffer to read data from.
 * @param p_value		pointer to the value that will store the data.
 */
void grk_read_float_LE(const uint8_t * p_buffer, float * p_value);

/**
 * Reads some bytes from the given data buffer, this function is used in Big Endian cpus.
 * @param p_buffer		pointer the data buffer to read data from.
 * @param p_value		pointer to the value that will store the data.
 */
void grk_read_float_BE(const uint8_t * p_buffer, float * p_value);

/**
 * Write some bytes to the given data buffer, this function is used in Little Endian cpus.
 * @param p_buffer		pointer the data buffer to write data to.
 * @param p_value		the value to write
 */
void grk_write_float_LE(uint8_t * p_buffer, float p_value);

/***
 * Write some bytes to the given data buffer, this function is used in Big Endian cpus.
 * @param p_buffer		pointer the data buffer to write data to.
 * @param p_value		the value to write
 */
void grk_write_float_BE(uint8_t * p_buffer, float p_value);

/**
 * Reads some bytes from the stream.
 * @param		p_stream	the stream to read data from.
 * @param		p_buffer	pointer to the data buffer that will receive the data.
 * @param		p_size		number of bytes to read.
 * @param		p_event_mgr	the user event manager to be notified of special events.
 * @return		the number of bytes read, or -1 if an error occurred or if the stream is at the end.
 */
size_t grk_stream_read_data (grk_stream_private_t * p_stream, uint8_t * p_buffer, size_t p_size, grk_event_mgr_t * p_event_mgr);

size_t grk_stream_read_data_zero_copy(grk_stream_private_t * p_stream, uint8_t ** p_buffer, size_t p_size, grk_event_mgr_t * p_event_mgr);

/**
 * Writes some bytes to the stream.
 * @param		p_stream	the stream to write data to.
 * @param		p_buffer	pointer to the data buffer holds the data to be written.
 * @param		p_size		number of bytes to write.
 * @param		p_event_mgr	the user event manager to be notified of special events.
 * @return		the number of bytes written, or -1 if an error occurred.
 */
size_t grk_stream_write_data (grk_stream_private_t * p_stream,
								const uint8_t * p_buffer,
								size_t p_size, 
								grk_event_mgr_t * p_event_mgr);

/**
 * Writes the content of the stream buffer to the stream.
 * @param		p_stream	the stream to write data to.
 * @param		p_event_mgr	the user event manager to be notified of special events.
 * @return		true if the data could be flushed, false else.
 */
bool grk_stream_flush (grk_stream_private_t * p_stream, 
						 grk_event_mgr_t * p_event_mgr);

/**
 * Skips a number of bytes from the stream.
 * @param		p_stream	the stream to skip data from.
 * @param		p_size		the number of bytes to skip.
 * @param		p_event_mgr	the user event manager to be notified of special events.
 * @return		the number of bytes skipped, or -1 if an error occurred.
 */
bool grk_stream_skip (grk_stream_private_t * p_stream,
						int64_t p_size, 
						grk_event_mgr_t * p_event_mgr);

/**
 * Tells the byte offset on the stream (similar to ftell).
 *
 * @param		p_stream	the stream to get the information from.
 *
 * @return		the current position o fthe stream.
 */
int64_t grk_stream_tell (const grk_stream_private_t * p_stream);


/**
 * Get the number of bytes left before the end of the stream (similar to cio_numbytesleft).
 *
 * @param		p_stream	the stream to get the information from.
 *
 * @return		Number of bytes left before the end of the stream.
 */
int64_t grk_stream_get_number_byte_left (const grk_stream_private_t * p_stream);

/**
 * Skips a number of bytes from the stream.
 * @param		p_stream	the stream to skip data from.
 * @param		p_size		the number of bytes to skip.
 * @param		p_event_mgr	the user event manager to be notified of special events.
 * @return		the number of bytes skipped, or -1 if an error occurred.
 */
bool grk_stream_write_skip (grk_stream_private_t * p_stream,
								int64_t p_size, 
								grk_event_mgr_t * p_event_mgr);

/**
 * Skips a number of bytes from the stream.
 * @param		p_stream	the stream to skip data from.
 * @param		p_size		the number of bytes to skip.
 * @param		p_event_mgr	the user event manager to be notified of special events.
 * @return		the number of bytes skipped, or -1 if an error occurred.
 */
bool grk_stream_read_skip (grk_stream_private_t * p_stream,
								int64_t p_size,
								grk_event_mgr_t * p_event_mgr);

/**
 * Skips a number of bytes from the stream.
 * @param		p_stream	the stream to skip data from.
 * @param		p_size		the number of bytes to skip.
 * @param		p_event_mgr	the user event manager to be notified of special events.
 * @return		true if success, or false if an error occurred.
 */
bool grk_stream_read_seek (grk_stream_private_t * p_stream, 
							int64_t p_size, 
							grk_event_mgr_t * p_event_mgr);

/**
 * Skips a number of bytes from the stream.
 * @param		p_stream	the stream to skip data from.
 * @param		p_size		the number of bytes to skip.
 * @param		p_event_mgr	the user event manager to be notified of special events.
 * @return		the number of bytes skipped, or -1 if an error occurred.
 */
bool grk_stream_write_seek (grk_stream_private_t * p_stream, 
							int64_t p_size,
							grk_event_mgr_t * p_event_mgr);

/**
 * Seeks a number of bytes from the stream.
 * @param		p_stream	the stream to skip data from.
 * @param		p_size		the number of bytes to skip.
 * @param		p_event_mgr	the user event manager to be notified of special events.
 * @return		true if the stream is seekable.
 */
bool grk_stream_seek (grk_stream_private_t * p_stream,
						int64_t p_size,
						grk_event_mgr_t * p_event_mgr);

/**
 * Tells if the given stream is seekable.
 */
bool grk_stream_has_seek (const grk_stream_private_t * p_stream);

/**
 * FIXME DOC.
 */
size_t grk_stream_default_read (void * p_buffer, 
								size_t p_nb_bytes,
								void * p_user_data);

/**
 * FIXME DOC.
 */
size_t grk_stream_default_write (void * p_buffer,
								size_t p_nb_bytes,
								void * p_user_data);

/**
 * FIXME DOC.
 */
int64_t grk_stream_default_skip (int64_t p_nb_bytes, void * p_user_data);

/**
 * FIXME DOC.
 */
bool grk_stream_default_seek (int64_t p_nb_bytes, void * p_user_data);

/* ----------------------------------------------------------------------- */
/*@}*/

/*@}*/



