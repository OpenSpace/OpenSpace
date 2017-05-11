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
#include "grk_includes.h"

static bool is_initialized = false;
bool OPJ_CALLCONV opj_initialize(const char* plugin_path)
{
    if (!is_initialized) {
		opj_plugin_load_info_t info;
		info.plugin_path = plugin_path;
        is_initialized = opj_plugin_load(info);
    }
    return is_initialized;
}

OPJ_API void OPJ_CALLCONV opj_cleanup() {
	opj_plugin_cleanup();
}

/* ---------------------------------------------------------------------- */
/* Functions to set the message handlers */

bool OPJ_CALLCONV opj_set_info_handler(	opj_codec_t * p_codec,
                                        opj_msg_callback p_callback,
                                        void * p_user_data)
{
    grk_codec_private_t * l_codec = (grk_codec_private_t *) p_codec;
    if(! l_codec) {
        return false;
    }

    l_codec->m_event_mgr.info_handler = p_callback;
    l_codec->m_event_mgr.m_info_data = p_user_data;

    return true;
}

bool OPJ_CALLCONV opj_set_warning_handler(	opj_codec_t * p_codec,
        opj_msg_callback p_callback,
        void * p_user_data)
{
    grk_codec_private_t * l_codec = (grk_codec_private_t *) p_codec;
    if (! l_codec) {
        return false;
    }

    l_codec->m_event_mgr.warning_handler = p_callback;
    l_codec->m_event_mgr.m_warning_data = p_user_data;

    return true;
}

bool OPJ_CALLCONV opj_set_error_handler(opj_codec_t * p_codec,
                                        opj_msg_callback p_callback,
                                        void * p_user_data)
{
    grk_codec_private_t * l_codec = (grk_codec_private_t *) p_codec;
    if (! l_codec) {
        return false;
    }

    l_codec->m_event_mgr.error_handler = p_callback;
    l_codec->m_event_mgr.m_error_data = p_user_data;

    return true;
}

/* ---------------------------------------------------------------------- */

static size_t grk_read_from_file (void * p_buffer, size_t p_nb_bytes, FILE * p_file)
{
    size_t l_nb_read = fread(p_buffer,1,p_nb_bytes,p_file);
    return l_nb_read ? l_nb_read : (size_t)-1;
}

static uint64_t opj_get_data_length_from_file (FILE * p_file)
{
    int64_t file_length = 0;

    OPJ_FSEEK(p_file, 0, SEEK_END);
    file_length = (int64_t)OPJ_FTELL(p_file);
    OPJ_FSEEK(p_file, 0, SEEK_SET);

    return (uint64_t)file_length;
}

static size_t grk_write_from_file (void * p_buffer, size_t p_nb_bytes, FILE * p_file)
{
    return fwrite(p_buffer,1,p_nb_bytes,p_file);
}

static int64_t opj_skip_from_file (int64_t p_nb_bytes, FILE * p_user_data)
{
    if (OPJ_FSEEK(p_user_data,p_nb_bytes,SEEK_CUR)) {
        return -1;
    }

    return p_nb_bytes;
}

static bool opj_seek_from_file (int64_t p_nb_bytes, FILE * p_user_data)
{
    if (OPJ_FSEEK(p_user_data,p_nb_bytes,SEEK_SET)) {
        return false;
    }

    return true;
}

/* ---------------------------------------------------------------------- */

/* ---------------------------------------------------------------------- */



#ifdef _WIN32
#ifndef OPJ_STATIC
BOOL APIENTRY
DllMain(HINSTANCE hModule, DWORD ul_reason_for_call, LPVOID lpReserved)
{

    OPJ_ARG_NOT_USED(lpReserved);
    OPJ_ARG_NOT_USED(hModule);

    switch (ul_reason_for_call) {
    case DLL_PROCESS_ATTACH :
        break;
    case DLL_PROCESS_DETACH :
        break;
    case DLL_THREAD_ATTACH :
    case DLL_THREAD_DETACH :
        break;
    }

    return TRUE;
}
#endif /* OPJ_STATIC */
#endif /* _WIN32 */

/* ---------------------------------------------------------------------- */

const char* OPJ_CALLCONV opj_version(void)
{
    return OPJ_PACKAGE_VERSION;
}

/* ---------------------------------------------------------------------- */
/* DECOMPRESSION FUNCTIONS*/

opj_codec_t* OPJ_CALLCONV opj_create_decompress(OPJ_CODEC_FORMAT p_format)
{
    grk_codec_private_t *l_codec = nullptr;

    l_codec = (grk_codec_private_t*) grk_calloc(1, sizeof(grk_codec_private_t));
    if (!l_codec) {
        return nullptr;
    }

    l_codec->is_decompressor = 1;

    switch (p_format) {
    case OPJ_CODEC_J2K:
        l_codec->opj_dump_codec = (void (*) (void*, int32_t, FILE*)) j2k_dump;

        l_codec->grk_get_codec_info = (opj_codestream_info_v2_t* (*) (void*) ) j2k_get_cstr_info;

        l_codec->opj_get_codec_index = (opj_codestream_index_t* (*) (void*) ) j2k_get_cstr_index;

        l_codec->m_codec_data.m_decompression.opj_decode =
            (bool (*) (	void *, 
						opj_plugin_tile_t*,
                        grk_stream_private_t *,
                        opj_image_t*, grk_event_mgr_t * )) grk_j2k_decode;

        l_codec->m_codec_data.m_decompression.opj_end_decompress =
            (bool (*) (	void *,
                        grk_stream_private_t *,
                        grk_event_mgr_t *)) grk_j2k_end_decompress;

        l_codec->m_codec_data.m_decompression.opj_read_header =
            (bool (*) (	grk_stream_private_t *,
                        void *,
						opj_header_info_t* header_info,
                        opj_image_t **,
                        grk_event_mgr_t * )) grk_j2k_read_header;

        l_codec->m_codec_data.m_decompression.opj_destroy =
            (void (*) (void *))grk_j2k_destroy;

        l_codec->m_codec_data.m_decompression.opj_setup_decoder =
            (void (*) (void * , opj_dparameters_t * )) grk_j2k_setup_decoder;

        l_codec->m_codec_data.m_decompression.opj_read_tile_header =
            (bool (*) (	void *,
                        uint32_t*,
                        uint64_t*,
                        uint32_t*, uint32_t*,
                        uint32_t*, uint32_t*,
                        uint32_t*,
                        bool*,
                        grk_stream_private_t *,
                        grk_event_mgr_t * )) grk_j2k_read_tile_header;

        l_codec->m_codec_data.m_decompression.opj_decode_tile_data =
            (bool (*) ( void *,
                        uint32_t,
                        uint8_t*,
                        uint64_t,
                        grk_stream_private_t *,
                        grk_event_mgr_t *)) grk_j2k_decode_tile;

        l_codec->m_codec_data.m_decompression.opj_set_decode_area =
            (bool (*) ( void *,
                        opj_image_t*,
                        uint32_t, uint32_t, uint32_t, uint32_t,
                        grk_event_mgr_t *)) grk_j2k_set_decode_area;

        l_codec->m_codec_data.m_decompression.opj_get_decoded_tile =
            (bool (*) ( void *p_codec,
                        grk_stream_private_t *p_cio,
                        opj_image_t *p_image,
                        grk_event_mgr_t * p_manager,
                        uint32_t tile_index)) grk_j2k_get_tile;

        l_codec->m_codec_data.m_decompression.opj_set_decoded_resolution_factor =
            (bool (*) ( void * p_codec,
                        uint32_t res_factor,
                        grk_event_mgr_t * p_manager)) grk_j2k_set_decoded_resolution_factor;

        l_codec->m_codec = grk_j2k_create_decompress();

        if (! l_codec->m_codec) {
            grk_free(l_codec);
            return NULL;
        }

        break;

    case OPJ_CODEC_JP2:
        /* get a JP2 decoder handle */
        l_codec->opj_dump_codec = (void (*) (void*, int32_t, FILE*)) jp2_dump;

        l_codec->grk_get_codec_info = (opj_codestream_info_v2_t* (*) (void*) ) jp2_get_cstr_info;

        l_codec->opj_get_codec_index = (opj_codestream_index_t* (*) (void*) ) jp2_get_cstr_index;

        l_codec->m_codec_data.m_decompression.opj_decode =
            (bool (*) (	void *, 
						opj_plugin_tile_t*,
                        grk_stream_private_t *,
                        opj_image_t*,
                        grk_event_mgr_t * )) grk_jp2_decode;

        l_codec->m_codec_data.m_decompression.opj_end_decompress =
            (bool (*) ( void *,
                        grk_stream_private_t *,
                        grk_event_mgr_t *)) grk_jp2_end_decompress;

        l_codec->m_codec_data.m_decompression.opj_read_header =
            (bool (*) ( grk_stream_private_t *,
                        void *,
						opj_header_info_t* header_info,
                        opj_image_t **,
                        grk_event_mgr_t * )) grk_jp2_read_header;

        l_codec->m_codec_data.m_decompression.opj_read_tile_header =
            (bool (*) ( void *,
                        uint32_t*,
                        uint64_t*,
                        uint32_t*,
                        uint32_t*,
                        uint32_t * ,
                        uint32_t * ,
                        uint32_t * ,
                        bool *,
                        grk_stream_private_t *,
                        grk_event_mgr_t * )) grk_jp2_read_tile_header;

        l_codec->m_codec_data.m_decompression.opj_decode_tile_data =
            (bool (*) ( void *,
                        uint32_t,
						uint8_t*,
						uint64_t,
                        grk_stream_private_t *,
                        grk_event_mgr_t * )) grk_jp2_decode_tile;

        l_codec->m_codec_data.m_decompression.opj_destroy = (void (*) (void *))grk_jp2_destroy;

        l_codec->m_codec_data.m_decompression.opj_setup_decoder =
            (void (*) (void * ,opj_dparameters_t * )) grk_jp2_setup_decoder;

        l_codec->m_codec_data.m_decompression.opj_set_decode_area =
            (bool (*) ( void *,
                        opj_image_t*,
                        uint32_t,uint32_t,uint32_t,uint32_t,
                        grk_event_mgr_t * )) grk_jp2_set_decode_area;

        l_codec->m_codec_data.m_decompression.opj_get_decoded_tile =
            (bool (*) ( void *p_codec,
                        grk_stream_private_t *p_cio,
                        opj_image_t *p_image,
                        grk_event_mgr_t * p_manager,
                        uint32_t tile_index)) grk_jp2_get_tile;

        l_codec->m_codec_data.m_decompression.opj_set_decoded_resolution_factor =
            (bool (*) ( void * p_codec,
                        uint32_t res_factor,
                        grk_event_mgr_t * p_manager)) grk_jp2_set_decoded_resolution_factor;

        l_codec->m_codec = grk_jp2_create(true);

        if (! l_codec->m_codec) {
            grk_free(l_codec);
            return nullptr;
        }

        break;
    case OPJ_CODEC_UNKNOWN:
    case OPJ_CODEC_JPT:
    default:
        grk_free(l_codec);
        return nullptr;
    }

    grk_set_default_event_handler(&(l_codec->m_event_mgr));
    return (opj_codec_t*) l_codec;
}

void OPJ_CALLCONV opj_set_default_decoder_parameters(opj_dparameters_t *parameters)
{
    if(parameters) {
        memset(parameters, 0, sizeof(opj_dparameters_t));
        /* default decoding parameters */
        parameters->cp_layer = 0;
        parameters->cp_reduce = 0;

        parameters->decod_format = -1;
        parameters->cod_format = -1;
        parameters->flags = 0;
		parameters->numThreads = 8;
    }
}

bool OPJ_CALLCONV opj_setup_decoder(opj_codec_t *p_codec,
                                    opj_dparameters_t *parameters
                                   )
{
    if (p_codec && parameters) {
        grk_codec_private_t * l_codec = (grk_codec_private_t *) p_codec;

        if (! l_codec->is_decompressor) {
            grk_event_msg(&(l_codec->m_event_mgr), EVT_ERROR,
                          "Codec provided to the opj_setup_decoder function is not a decompressor handler.\n");
            return false;
        }

        l_codec->m_codec_data.m_decompression.opj_setup_decoder(l_codec->m_codec,
                parameters);
        return true;
    }
    return false;
}

bool OPJ_CALLCONV opj_read_header(opj_stream_t *p_stream,
	opj_codec_t *p_codec,
	opj_image_t **p_image) {

	return opj_read_header_ex(p_stream, p_codec, NULL, p_image);
}

bool OPJ_CALLCONV opj_read_header_ex (	opj_stream_t *p_stream,
                                    opj_codec_t *p_codec,
									opj_header_info_t* header_info,
                                    opj_image_t **p_image )
{
    if (p_codec && p_stream) {
        grk_codec_private_t* l_codec = (grk_codec_private_t*) p_codec;
        grk_stream_private_t* l_stream = (grk_stream_private_t*) p_stream;

        if(! l_codec->is_decompressor) {
            grk_event_msg(&(l_codec->m_event_mgr), EVT_ERROR,
                          "Codec provided to the grk_read_header function is not a decompressor handler.\n");
            return false;
        }

        return l_codec->m_codec_data.m_decompression.opj_read_header(	l_stream,
                l_codec->m_codec,
				header_info,
                p_image,
                &(l_codec->m_event_mgr) );
    }

    return false;
}

bool OPJ_CALLCONV opj_decode(opj_codec_t *p_codec,
	opj_stream_t *p_stream,
	opj_image_t* p_image) {
	return opj_decode_ex(p_codec, NULL, p_stream, p_image);
}

bool OPJ_CALLCONV opj_decode_ex(   opj_codec_t *p_codec,
								opj_plugin_tile_t* tile,
                                opj_stream_t *p_stream,
                                opj_image_t* p_image)
{
    if (p_codec && p_stream) {
        grk_codec_private_t * l_codec = (grk_codec_private_t *) p_codec;
        grk_stream_private_t * l_stream = (grk_stream_private_t *) p_stream;

        if (! l_codec->is_decompressor) {
            return false;
        }

        return l_codec->m_codec_data.m_decompression.opj_decode(l_codec->m_codec,
				tile,
                l_stream,
                p_image,
                &(l_codec->m_event_mgr) );
    }

    return false;
}

bool OPJ_CALLCONV opj_set_decode_area(	opj_codec_t *p_codec,
                                        opj_image_t* p_image,
                                        uint32_t p_start_x, uint32_t p_start_y,
                                        uint32_t p_end_x, uint32_t p_end_y
                                     )
{
    if (p_codec) {
        grk_codec_private_t * l_codec = (grk_codec_private_t *) p_codec;

        if (! l_codec->is_decompressor) {
            return false;
        }

        return  l_codec->m_codec_data.m_decompression.opj_set_decode_area(	l_codec->m_codec,
                p_image,
                p_start_x, p_start_y,
                p_end_x, p_end_y,
                &(l_codec->m_event_mgr) );
    }
    return false;
}

bool OPJ_CALLCONV opj_read_tile_header(	opj_codec_t *p_codec,
                                        opj_stream_t * p_stream,
                                        uint32_t * p_tile_index,
                                        uint64_t * p_data_size,
                                        uint32_t * p_tile_x0,
										uint32_t * p_tile_y0,
                                        uint32_t * p_tile_x1,
										uint32_t * p_tile_y1,
                                        uint32_t * p_nb_comps,
                                        bool * p_should_go_on)
{
    if (p_codec && p_stream && p_data_size && p_tile_index) {
        grk_codec_private_t * l_codec = (grk_codec_private_t *) p_codec;
        grk_stream_private_t * l_stream = (grk_stream_private_t *) p_stream;

        if (! l_codec->is_decompressor) {
            return false;
        }

        return l_codec->m_codec_data.m_decompression.opj_read_tile_header(	l_codec->m_codec,
                p_tile_index,
                p_data_size,
                p_tile_x0, p_tile_y0,
                p_tile_x1, p_tile_y1,
                p_nb_comps,
                p_should_go_on,
                l_stream,
                &(l_codec->m_event_mgr));
    }
    return false;
}

bool OPJ_CALLCONV opj_decode_tile_data(	opj_codec_t *p_codec,
                                        uint32_t p_tile_index,
                                        uint8_t * p_data,
                                        uint64_t p_data_size,
                                        opj_stream_t *p_stream
                                      )
{
    if (p_codec && p_data && p_stream) {
        grk_codec_private_t * l_codec = (grk_codec_private_t *) p_codec;
        grk_stream_private_t * l_stream = (grk_stream_private_t *) p_stream;

        if (! l_codec->is_decompressor) {
            return false;
        }

        return l_codec->m_codec_data.m_decompression.opj_decode_tile_data(	l_codec->m_codec,
                p_tile_index,
                p_data,
                p_data_size,
                l_stream,
                &(l_codec->m_event_mgr) );
    }
    return false;
}

bool OPJ_CALLCONV opj_get_decoded_tile(	opj_codec_t *p_codec,
                                        opj_stream_t *p_stream,
                                        opj_image_t *p_image,
                                        uint32_t tile_index)
{
    if (p_codec && p_stream) {
        grk_codec_private_t * l_codec = (grk_codec_private_t *) p_codec;
        grk_stream_private_t * l_stream = (grk_stream_private_t *) p_stream;

        if (! l_codec->is_decompressor) {
            return false;
        }

        return l_codec->m_codec_data.m_decompression.opj_get_decoded_tile(	l_codec->m_codec,
                l_stream,
                p_image,
                &(l_codec->m_event_mgr),
                tile_index);
    }

    return false;
}

bool OPJ_CALLCONV opj_set_decoded_resolution_factor(opj_codec_t *p_codec,
        uint32_t res_factor )
{
    grk_codec_private_t * l_codec = (grk_codec_private_t *) p_codec;

    if ( !l_codec ) {
        return false;
    }

    return l_codec->m_codec_data.m_decompression.opj_set_decoded_resolution_factor(l_codec->m_codec,
            res_factor,
            &(l_codec->m_event_mgr) );
}

/* ---------------------------------------------------------------------- */
/* COMPRESSION FUNCTIONS*/

opj_codec_t* OPJ_CALLCONV opj_create_compress(OPJ_CODEC_FORMAT p_format)
{
    grk_codec_private_t *l_codec = nullptr;

    l_codec = (grk_codec_private_t*)grk_calloc(1, sizeof(grk_codec_private_t));
    if (!l_codec) {
        return nullptr;
    }

    l_codec->is_decompressor = 0;

    switch(p_format) {
    case OPJ_CODEC_J2K:
        l_codec->m_codec_data.m_compression.opj_encode = (bool (*) (void *,
				opj_plugin_tile_t*,
                grk_stream_private_t *,
                grk_event_mgr_t * )) grk_j2k_encode;

        l_codec->m_codec_data.m_compression.opj_end_compress = (bool (*) (	void *,
                grk_stream_private_t *,
                grk_event_mgr_t *)) grk_j2k_end_compress;

        l_codec->m_codec_data.m_compression.opj_start_compress = (bool (*) (void *,
                grk_stream_private_t *,
                struct opj_image * ,
                grk_event_mgr_t *)) grk_j2k_start_compress;

        l_codec->m_codec_data.m_compression.opj_write_tile = (bool (*) (void *,
                uint32_t,
                uint8_t*,
                uint64_t,
                grk_stream_private_t *,
                grk_event_mgr_t *) ) grk_j2k_write_tile;

        l_codec->m_codec_data.m_compression.opj_destroy = (void (*) (void *)) grk_j2k_destroy;

        l_codec->m_codec_data.m_compression.opj_setup_encoder = (bool (*) (	void *,
                opj_cparameters_t *,
                opj_image_t *,
                grk_event_mgr_t * )) grk_j2k_setup_encoder;

        l_codec->m_codec = grk_j2k_create_compress();
        if (! l_codec->m_codec) {
            grk_free(l_codec);
            return nullptr;
        }

        break;

    case OPJ_CODEC_JP2:
        /* get a JP2 decoder handle */
        l_codec->m_codec_data.m_compression.opj_encode = (bool (*) (void *,
				opj_plugin_tile_t*,
                grk_stream_private_t *,
                grk_event_mgr_t * )) grk_jp2_encode;

        l_codec->m_codec_data.m_compression.opj_end_compress = (bool (*) (	void *,
                grk_stream_private_t *,
                grk_event_mgr_t *)) grk_jp2_end_compress;

        l_codec->m_codec_data.m_compression.opj_start_compress = (bool (*) (void *,
                grk_stream_private_t *,
                struct opj_image * ,
                grk_event_mgr_t *))  grk_jp2_start_compress;

        l_codec->m_codec_data.m_compression.opj_write_tile = (bool (*) (void *,
                uint32_t,
                uint8_t*,
                uint64_t,
                grk_stream_private_t *,
                grk_event_mgr_t *)) grk_jp2_write_tile;

        l_codec->m_codec_data.m_compression.opj_destroy = (void (*) (void *)) grk_jp2_destroy;

        l_codec->m_codec_data.m_compression.opj_setup_encoder = (bool (*) (	void *,
                opj_cparameters_t *,
                opj_image_t *,
                grk_event_mgr_t * )) grk_jp2_setup_encoder;

        l_codec->m_codec = grk_jp2_create(false);
        if (! l_codec->m_codec) {
            grk_free(l_codec);
            return nullptr;
        }

        break;

    case OPJ_CODEC_UNKNOWN:
    case OPJ_CODEC_JPT:
    default:
        grk_free(l_codec);
        return nullptr;
    }

    grk_set_default_event_handler(&(l_codec->m_event_mgr));
    return (opj_codec_t*) l_codec;
}

void OPJ_CALLCONV opj_set_default_encoder_parameters(opj_cparameters_t *parameters)
{
    if(parameters) {
        memset(parameters, 0, sizeof(opj_cparameters_t));
        /* default coding parameters */
        parameters->rsiz = OPJ_PROFILE_NONE;
        parameters->max_comp_size = 0;
        parameters->numresolution = 6;
        parameters->cblockw_init = 64;
        parameters->cblockh_init = 64;
        parameters->prog_order = OPJ_LRCP;
        parameters->roi_compno = -1;		/* no ROI */
        parameters->subsampling_dx = 1;
        parameters->subsampling_dy = 1;
        parameters->tp_on = 0;
        parameters->decod_format = -1;
        parameters->cod_format = -1;
        parameters->tcp_rates[0] = 0;
        parameters->tcp_numlayers = 0;
        parameters->cp_disto_alloc = 0;
        parameters->cp_fixed_quality = 0;
		parameters->numThreads = 8;
		parameters->deviceId = 0;
		parameters->repeats = 1;
    }
}

bool OPJ_CALLCONV opj_setup_encoder(opj_codec_t *p_codec,
                                    opj_cparameters_t *parameters,
                                    opj_image_t *p_image)
{
    if (p_codec && parameters && p_image) {
        grk_codec_private_t * l_codec = (grk_codec_private_t *) p_codec;

        if (! l_codec->is_decompressor) {
            return l_codec->m_codec_data.m_compression.opj_setup_encoder(	l_codec->m_codec,
                    parameters,
                    p_image,
                    &(l_codec->m_event_mgr) );
        }
    }

    return false;
}

bool OPJ_CALLCONV opj_start_compress (	opj_codec_t *p_codec,
                                        opj_image_t * p_image,
                                        opj_stream_t *p_stream)
{
    if (p_codec && p_stream) {
        grk_codec_private_t * l_codec = (grk_codec_private_t *) p_codec;
        grk_stream_private_t * l_stream = (grk_stream_private_t *) p_stream;

        if (! l_codec->is_decompressor) {
            return l_codec->m_codec_data.m_compression.opj_start_compress(	l_codec->m_codec,
                    l_stream,
                    p_image,
                    &(l_codec->m_event_mgr));
        }
    }

    return false;
}


bool OPJ_CALLCONV opj_encode(opj_codec_t *p_info, opj_stream_t *p_stream)
{
	return opj_encode_with_plugin(p_info, NULL, p_stream);
}

bool OPJ_CALLCONV opj_encode_with_plugin(opj_codec_t *p_info, opj_plugin_tile_t* tile, opj_stream_t *p_stream)
{
    if (p_info && p_stream) {
        grk_codec_private_t * l_codec = (grk_codec_private_t *) p_info;
        grk_stream_private_t * l_stream = (grk_stream_private_t *) p_stream;

        if (! l_codec->is_decompressor) {
            return l_codec->m_codec_data.m_compression.opj_encode(	l_codec->m_codec,
					tile,
                    l_stream,
                    &(l_codec->m_event_mgr));
        }
    }

    return false;

}

bool OPJ_CALLCONV opj_end_compress (opj_codec_t *p_codec,
                                    opj_stream_t *p_stream)
{
    if (p_codec && p_stream) {
        grk_codec_private_t * l_codec = (grk_codec_private_t *) p_codec;
        grk_stream_private_t * l_stream = (grk_stream_private_t *) p_stream;

        if (! l_codec->is_decompressor) {
            return l_codec->m_codec_data.m_compression.opj_end_compress(l_codec->m_codec,
                    l_stream,
                    &(l_codec->m_event_mgr));
        }
    }
    return false;

}

bool OPJ_CALLCONV opj_end_decompress (	opj_codec_t *p_codec,
                                        opj_stream_t *p_stream)
{
    if (p_codec && p_stream) {
        grk_codec_private_t * l_codec = (grk_codec_private_t *) p_codec;
        grk_stream_private_t * l_stream = (grk_stream_private_t *) p_stream;

        if (! l_codec->is_decompressor) {
            return false;
        }

        return l_codec->m_codec_data.m_decompression.opj_end_decompress(l_codec->m_codec,
                l_stream,
                &(l_codec->m_event_mgr) );
    }

    return false;
}

bool OPJ_CALLCONV opj_set_MCT(opj_cparameters_t *parameters,
                              float * pEncodingMatrix,
                              int32_t * p_dc_shift,uint32_t pNbComp)
{
    uint32_t l_matrix_size = pNbComp * pNbComp * (uint32_t)sizeof(float);
    uint32_t l_dc_shift_size = pNbComp * (uint32_t)sizeof(int32_t);
    uint32_t l_mct_total_size = l_matrix_size + l_dc_shift_size;

    /* add MCT capability */
    if (OPJ_IS_PART2(parameters->rsiz)) {
        parameters->rsiz |= OPJ_EXTENSION_MCT;
    } else {
        parameters->rsiz = ((OPJ_PROFILE_PART2) | (OPJ_EXTENSION_MCT));
    }
    parameters->irreversible = 1;

    /* use array based MCT */
    parameters->tcp_mct = 2;
    parameters->mct_data = grk_malloc(l_mct_total_size);
    if (! parameters->mct_data) {
        return false;
    }

    memcpy(parameters->mct_data,pEncodingMatrix,l_matrix_size);
    memcpy(((uint8_t *) parameters->mct_data) +  l_matrix_size,p_dc_shift,l_dc_shift_size);

    return true;
}

bool OPJ_CALLCONV opj_write_tile (	opj_codec_t *p_codec,
                                    uint32_t p_tile_index,
                                    uint8_t * p_data,
                                    uint64_t p_data_size,
                                    opj_stream_t *p_stream )
{
    if (p_codec && p_stream && p_data) {
        grk_codec_private_t * l_codec = (grk_codec_private_t *) p_codec;
        grk_stream_private_t * l_stream = (grk_stream_private_t *) p_stream;

        if (l_codec->is_decompressor) {
            return false;
        }

        return l_codec->m_codec_data.m_compression.opj_write_tile(	l_codec->m_codec,
                p_tile_index,
                p_data,
                p_data_size,
                l_stream,
                &(l_codec->m_event_mgr) );
    }

    return false;
}

/* ---------------------------------------------------------------------- */

void OPJ_CALLCONV opj_destroy_codec(opj_codec_t *p_codec)
{
    if (p_codec) {
        grk_codec_private_t * l_codec = (grk_codec_private_t *) p_codec;

        if (l_codec->is_decompressor) {
            l_codec->m_codec_data.m_decompression.opj_destroy(l_codec->m_codec);
        } else {
            l_codec->m_codec_data.m_compression.opj_destroy(l_codec->m_codec);
        }

        l_codec->m_codec = nullptr;
        grk_free(l_codec);
    }
}

/* ---------------------------------------------------------------------- */

void OPJ_CALLCONV opj_dump_codec(	opj_codec_t *p_codec,
                                    int32_t info_flag,
                                    FILE* output_stream)
{
    if (p_codec) {
        grk_codec_private_t* l_codec = (grk_codec_private_t*) p_codec;

        l_codec->opj_dump_codec(l_codec->m_codec, info_flag, output_stream);
        return;
    }

    /* TODO return error */
    /* fprintf(stderr, "[ERROR] Input parameter of the dump_codec function are incorrect.\n"); */
    return;
}

opj_codestream_info_v2_t* OPJ_CALLCONV opj_get_cstr_info(opj_codec_t *p_codec)
{
    if (p_codec) {
        grk_codec_private_t* l_codec = (grk_codec_private_t*) p_codec;

        return l_codec->grk_get_codec_info(l_codec->m_codec);
    }

    return NULL;
}

void OPJ_CALLCONV opj_destroy_cstr_info(opj_codestream_info_v2_t **cstr_info)
{
    if (cstr_info) {

        if ((*cstr_info)->m_default_tile_info.tccp_info) {
            grk_free((*cstr_info)->m_default_tile_info.tccp_info);
        }

        if ((*cstr_info)->tile_info) {
            /* FIXME not used for the moment*/
        }

        grk_free((*cstr_info));
        (*cstr_info) = NULL;
    }
}

opj_codestream_index_t * OPJ_CALLCONV opj_get_cstr_index(opj_codec_t *p_codec)
{
    if (p_codec) {
        grk_codec_private_t* l_codec = (grk_codec_private_t*) p_codec;

        return l_codec->opj_get_codec_index(l_codec->m_codec);
    }

    return NULL;
}

void OPJ_CALLCONV opj_destroy_cstr_index(opj_codestream_index_t **p_cstr_index)
{
    if (*p_cstr_index) {
        j2k_destroy_cstr_index(*p_cstr_index);
        (*p_cstr_index) = NULL;
    }
}

/* ---------------------------------------------------------------------- */

opj_stream_t* OPJ_CALLCONV opj_stream_create_default_file_stream (const char *fname, bool p_is_read_stream)
{
    return opj_stream_create_file_stream(fname, OPJ_J2K_STREAM_CHUNK_SIZE, p_is_read_stream);
}

opj_stream_t* OPJ_CALLCONV opj_stream_create_file_stream (
    const char *fname,
    size_t p_size,
    bool p_is_read_stream)
{
    opj_stream_t* l_stream = nullptr;
    FILE *p_file;
    const char *mode;

    if (! fname) {
        return NULL;
    }

    if(p_is_read_stream) mode = "rb";
    else mode = "wb";

    p_file = fopen(fname, mode);

    if (! p_file) {
        return NULL;
    }

    l_stream = opj_stream_create(p_size,p_is_read_stream);
    if (! l_stream) {
        fclose(p_file);
        return NULL;
    }

    opj_stream_set_user_data(l_stream, p_file, (opj_stream_free_user_data_fn) fclose);
    opj_stream_set_user_data_length(l_stream, opj_get_data_length_from_file(p_file));
    opj_stream_set_read_function(l_stream, (opj_stream_read_fn) grk_read_from_file);
    opj_stream_set_write_function(l_stream, (opj_stream_write_fn) grk_write_from_file);
    opj_stream_set_skip_function(l_stream, (opj_stream_skip_fn) opj_skip_from_file);
    opj_stream_set_seek_function(l_stream, (opj_stream_seek_fn) opj_seek_from_file);

    return l_stream;
}


/* ---------------------------------------------------------------------- */
OPJ_API size_t OPJ_CALLCONV opj_stream_get_write_buffer_stream_length(opj_stream_t* stream) {
	if (!stream)
		return 0;
	return grk_get_buffer_stream_offset(stream);

}


opj_stream_t* OPJ_CALLCONV opj_stream_create_buffer_stream(uint8_t *buf,
        size_t len,
        bool p_is_read_stream)
{
    return grk_create_buffer_stream(buf, len, p_is_read_stream);

}

opj_stream_t* OPJ_CALLCONV opj_stream_create_mapped_file_read_stream(const char *fname)
{
    return grk_create_mapped_file_read_stream(fname);
}


/* ---------------------------------------------------------------------- */

void OPJ_CALLCONV opj_image_all_components_data_free(opj_image_t* image)
{
    uint32_t i;
    if (!image || !image->comps)
        return;
    for (i = 0; i < image->numcomps; ++i) {
        opj_image_single_component_data_free(image->comps + i);
    }
}


bool OPJ_CALLCONV opj_image_single_component_data_alloc(opj_image_comp_t* comp)
{
    int32_t* data = NULL;
    if (!comp)
        return false;

    data = (int32_t*)grk_aligned_malloc(comp->w * comp->h * sizeof(uint32_t));
    if (!data)
        return false;
    opj_image_single_component_data_free(comp);
    comp->data = data;
    return true;
}

void OPJ_CALLCONV opj_image_single_component_data_free(opj_image_comp_t* comp)
{
    if (!comp)
        return;
    if (comp->data) {
        grk_aligned_free(comp->data);
        comp->data = NULL;
    }
}


/**********************************************************************
Plugin interface implementation
***********************************************************************/

static const char* get_path_separator() {
#ifdef _WIN32
	return "\\";
#else
	return "/";
#endif
}


bool pluginLoaded = false;
bool OPJ_CALLCONV opj_plugin_load(opj_plugin_load_info_t info)
{
	// form plugin name
	std::string pluginName = "";
#if !defined(_WIN32)
	pluginName += "lib";
#endif
	pluginName += std::string(GROK_PLUGIN_NAME) + "." + minpf_get_dynamic_library_extension();

	// form absolute plugin path
	auto pluginPath = std::string(info.plugin_path) + get_path_separator() + pluginName;
	int32_t rc = minpf_load_from_path(pluginPath.c_str(), NULL);

	// if fails, try local path
	if (rc) {
		std::string localPlugin = std::string(".") + get_path_separator() + pluginName;
		rc = minpf_load_from_path(localPlugin.c_str(), NULL);

	}
	pluginLoaded = !rc;
	if (!pluginLoaded)
		minpf_cleanup_plugin_manager();
	return pluginLoaded;
}


uint32_t OPJ_CALLCONV opj_plugin_get_debug_state()
{
    minpf_plugin_manager* mgr = NULL;
    PLUGIN_GET_DEBUG_STATE func = NULL;
    uint32_t rc = OPJ_PLUGIN_STATE_NO_DEBUG;

    if (!pluginLoaded)
        return rc;
    mgr = minpf_get_plugin_manager();
    if (mgr && mgr->num_libraries > 0) {
        func = (PLUGIN_GET_DEBUG_STATE)minpf_get_symbol(mgr->dynamic_libraries[0], plugin_get_debug_state_method_name);
        if (func) {
            rc = func();
        }
    }
    return rc;
}

void OPJ_CALLCONV opj_plugin_cleanup(void)
{
    minpf_cleanup_plugin_manager();
    pluginLoaded = false;
}

OPJ_API bool OPJ_CALLCONV opj_plugin_init(opj_plugin_init_info_t initInfo) {
	minpf_plugin_manager* mgr = NULL;
	PLUGIN_INIT func = NULL;
	if (!pluginLoaded)
		return false;

	mgr = minpf_get_plugin_manager();
	if (mgr && mgr->num_libraries > 0) {
		func = (PLUGIN_INIT)minpf_get_symbol(mgr->dynamic_libraries[0], plugin_init_method_name);
		if (func) {
			return func(initInfo);
		}
	}
	return false;
}


/*******************
Encode Implementation
********************/

OPJ_PLUGIN_ENCODE_USER_CALLBACK userEncodeCallback = 0;

/* wrapper for user's encode callback */
void opj_plugin_internal_encode_callback(plugin_encode_user_callback_info_t* info)
{
    /* set code block data etc on code object */
    opj_plugin_encode_user_callback_info_t opjInfo;
    memset(&opjInfo, 0, sizeof(opj_plugin_encode_user_callback_info_t));
    opjInfo.input_file_name = info->input_file_name;
	opjInfo.outputFileNameIsRelative = info->outputFileNameIsRelative;
    opjInfo.output_file_name = info->output_file_name;
    opjInfo.encoder_parameters = (opj_cparameters_t*)info->encoder_parameters;
    opjInfo.image = (opj_image_t*)info->image;
    opjInfo.tile = (opj_plugin_tile_t*)info->tile;
    if (userEncodeCallback)
        userEncodeCallback(&opjInfo);
}

int32_t OPJ_CALLCONV opj_plugin_encode(opj_cparameters_t* encode_parameters,
                                       OPJ_PLUGIN_ENCODE_USER_CALLBACK callback)
{
    minpf_plugin_manager* mgr = NULL;
    PLUGIN_ENCODE func = NULL;
    if (!pluginLoaded)
        return -1;

    userEncodeCallback = callback;
    mgr = minpf_get_plugin_manager();
    if (mgr && mgr->num_libraries > 0) {
        func = (PLUGIN_ENCODE)minpf_get_symbol(mgr->dynamic_libraries[0], plugin_encode_method_name);
        if (func) {
            return func((opj_cparameters_t*)encode_parameters, opj_plugin_internal_encode_callback);
        }
    }
    return -1;
}


int32_t OPJ_CALLCONV opj_plugin_batch_encode(const char* input_dir,
        const char* output_dir,
        opj_cparameters_t* encode_parameters,
        OPJ_PLUGIN_ENCODE_USER_CALLBACK callback)
{
    minpf_plugin_manager* mgr = NULL;
    PLUGIN_BATCH_ENCODE func = NULL;
    if (!pluginLoaded)
        return -1;

    userEncodeCallback = callback;
    mgr = minpf_get_plugin_manager();
    if (mgr && mgr->num_libraries > 0) {
        func = (PLUGIN_BATCH_ENCODE)minpf_get_symbol(mgr->dynamic_libraries[0], plugin_batch_encode_method_name);
        if (func) {
            return  func(input_dir, output_dir, (opj_cparameters_t*)encode_parameters, opj_plugin_internal_encode_callback);
        }
    }
    return -1;
}

PLUGIN_IS_BATCH_ENCODE_COMPLETE funcPluginIsBatchEncodeComplete = NULL;
OPJ_API bool OPJ_CALLCONV opj_plugin_is_batch_encode_complete(void) {
	minpf_plugin_manager* mgr = NULL;
	if (!pluginLoaded)
		return true;

	mgr = minpf_get_plugin_manager();
	if (mgr && mgr->num_libraries > 0) {
		if (!funcPluginIsBatchEncodeComplete)
			funcPluginIsBatchEncodeComplete = (PLUGIN_IS_BATCH_ENCODE_COMPLETE)minpf_get_symbol(mgr->dynamic_libraries[0], plugin_is_batch_encode_complete_method_name);
		if (funcPluginIsBatchEncodeComplete) {
			return  funcPluginIsBatchEncodeComplete();
		}
	}
	return true;
}

void OPJ_CALLCONV opj_plugin_stop_batch_encode(void)
{
    minpf_plugin_manager* mgr = NULL;
    PLUGIN_STOP_BATCH_DECODE func = NULL;
    if (!pluginLoaded)
        return;

    mgr = minpf_get_plugin_manager();
    if (mgr && mgr->num_libraries > 0) {
        func = (PLUGIN_STOP_BATCH_DECODE)minpf_get_symbol(mgr->dynamic_libraries[0], plugin_stop_batch_encode_method_name);
        if (func) {
            func();
        }
    }
}



/*******************
Decode Implementation
********************/

opj_plugin_decode_callback userPreDecodeCallback = 0;
opj_plugin_decode_callback userPostDecodeCallback = 0;

/* wrapper for user's decode callback */
void opj_plugin_internal_decode_callback(plugin_decode_callback_info_t* info)
{
    /* set code block data etc on code object */
    opj_plugin_decode_callback_info_t opjInfo;
    memset(&opjInfo, 0, sizeof(opj_plugin_decode_callback_info_t));
    opjInfo.generate_tile_func = (OPJ_GENERATE_TILE)info->generate_tile_func;
    opjInfo.input_file_name = info->input_file_name;
    opjInfo.output_file_name = info->output_file_name;
    opjInfo.decoder_parameters = (opj_decompress_parameters*)info->decoder_parameters;
    opjInfo.image = (opj_image_t*)info->image;
    opjInfo.tile = (opj_plugin_tile_t*)info->tile;
    if (!opjInfo.image) {
		if (userPreDecodeCallback) {
			userPreDecodeCallback(&opjInfo);
			info->image = opjInfo.image;
			info->tile = opjInfo.tile;
			info->l_stream = opjInfo.l_stream;
			info->l_codec = opjInfo.l_codec;
		}
    } else {
        if (userPostDecodeCallback)
            userPostDecodeCallback(&opjInfo);
    }
}

int32_t OPJ_CALLCONV opj_plugin_decode(opj_decompress_parameters* decode_parameters,
                                       opj_plugin_decode_callback preDecode,
                                       opj_plugin_decode_callback postDecode)
{
    minpf_plugin_manager* mgr = NULL;
    PLUGIN_DECODE func = NULL;
    if (!pluginLoaded)
        return -1;

    userPreDecodeCallback = preDecode;
    userPostDecodeCallback = postDecode;
    mgr = minpf_get_plugin_manager();
    func = NULL;
    if (mgr && mgr->num_libraries > 0) {
        func = (PLUGIN_DECODE)minpf_get_symbol(mgr->dynamic_libraries[0], plugin_decode_method_name);
        if (func) {
            return func((opj_decompress_parameters*)decode_parameters, opj_plugin_internal_decode_callback);
        }
    }
    return -1;
}


int32_t OPJ_CALLCONV opj_plugin_batch_decode(const char* input_dir,
        const char* output_dir,
        opj_decompress_parameters* decode_parameters,
        opj_plugin_decode_callback preDecode,
        opj_plugin_decode_callback postDecode)
{
    minpf_plugin_manager* mgr = NULL;
    PLUGIN_BATCH_DECODE func = NULL;
    if (!pluginLoaded)
        return -1;

    userPreDecodeCallback = preDecode;
    userPostDecodeCallback = postDecode;
    mgr = minpf_get_plugin_manager();
    if (mgr && mgr->num_libraries > 0) {
        func = (PLUGIN_BATCH_DECODE)minpf_get_symbol(mgr->dynamic_libraries[0], plugin_batch_decode_method_name);
        if (func) {
            return  func(input_dir, output_dir, (opj_decompress_parameters*)decode_parameters, opj_plugin_internal_decode_callback);
        }
    }
    return -1;
}

void OPJ_CALLCONV opj_plugin_stop_batch_decode(void)
{
    minpf_plugin_manager* mgr = NULL;
    PLUGIN_STOP_BATCH_DECODE func = NULL;
    if (!pluginLoaded)
        return;

    mgr = minpf_get_plugin_manager();
    if (mgr && mgr->num_libraries > 0) {
        func = (PLUGIN_STOP_BATCH_DECODE)minpf_get_symbol(mgr->dynamic_libraries[0], plugin_stop_batch_decode_method_name);
        if (func) {
            func();
        }
    }
}



/*****************************************************************************************/




