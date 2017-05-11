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


#pragma once

#include "openjpeg.h"
#include "minpf_plugin.h"

/////////////////////
// Debug Interface
/////////////////////

#define DEBUG_CONTEXT_CACHE_SIZE 3

// debugging variables
struct plugin_debug_mqc_t {
	uint32_t	debug_state;
	uint8_t		context_number;			
	uint32_t*	contextStream;			
	uint32_t	contextStreamByteCount;	
	uint8_t		contextCache[DEBUG_CONTEXT_CACHE_SIZE];
	uint32_t	contextCacheCount;
};


static const char* plugin_get_debug_state_method_name = "plugin_get_debug_state";
typedef uint32_t(*PLUGIN_GET_DEBUG_STATE)(void);


static const char* plugin_debug_mqc_next_cxd_method_name = "plugin_debug_mqc_next_cxd";
typedef void (*PLUGIN_DEBUG_MQC_NEXT_CXD)(plugin_debug_mqc_t *mqc, uint32_t d);


static const char* plugin_debug_mqc_next_plane_method_name = "plugin_debug_mqc_next_plane";
typedef void  (*PLUGIN_DEBUG_MQC_NEXT_PLANE)(plugin_debug_mqc_t *mqc);


/////////////////////
// encoder interface
/////////////////////

typedef struct plugin_encode_user_callback_info {
    const char* input_file_name;
	bool	outputFileNameIsRelative;
    const char* output_file_name;
    opj_cparameters_t* encoder_parameters;
    opj_image_t* image;
    opj_plugin_tile_t* tile;
    int32_t	error_code;
} plugin_encode_user_callback_info_t;

typedef void(*PLUGIN_ENCODE_USER_CALLBACK)(plugin_encode_user_callback_info_t* info);

static const char* plugin_init_method_name = "plugin_init";
typedef bool(*PLUGIN_INIT)(opj_plugin_init_info_t initInfo);


static const char* plugin_encode_method_name = "plugin_encode";
typedef int32_t (*PLUGIN_ENCODE)( opj_cparameters_t* encoding_parameters, PLUGIN_ENCODE_USER_CALLBACK callback);


static const char* plugin_batch_encode_method_name = "plugin_batch_encode";
typedef int32_t (*PLUGIN_BATCH_ENCODE)(const char* input_dir,
                                       const char* output_dir,
                                       opj_cparameters_t* encoding_parameters,
                                       PLUGIN_ENCODE_USER_CALLBACK userCallback);

static const char* plugin_stop_batch_encode_method_name = "plugin_stop_batch_encode";
typedef void (*PLUGIN_STOP_BATCH_ENCODE)(void);


static const char* plugin_is_batch_encode_complete_method_name = "plugin_is_batch_encode_complete";
typedef bool (*PLUGIN_IS_BATCH_ENCODE_COMPLETE)(void);



////////////////////
// decoder interface
////////////////////

typedef opj_plugin_tile_t*(*GENERATE_TILE)(size_t deviceId,
                                    size_t compressed_tile_id,
                                    opj_cparameters_t* encoder_parameters,
                                    opj_image_t* image);

typedef struct plugin_decode_callback_info {
    size_t deviceId;
    size_t compressed_tile_id;
    GENERATE_TILE generate_tile_func;
    const char* input_file_name;
    const char* output_file_name;
	opj_stream_t*				l_stream;
	opj_codec_t*				l_codec;
    opj_decompress_parameters* decoder_parameters;
    opj_image_t* image;
    opj_plugin_tile_t* tile;
    int32_t	error_code;
} plugin_decode_callback_info_t;

typedef void(*PLUGIN_DECODE_USER_CALLBACK)(plugin_decode_callback_info_t* info);


static const char* plugin_decode_method_name = "plugin_decode";
typedef int32_t (*PLUGIN_DECODE)(opj_decompress_parameters* decoding_parameters,
                                 PLUGIN_DECODE_USER_CALLBACK userCallback);


static const char* plugin_batch_decode_method_name = "plugin_batch_decode";
typedef int32_t (*PLUGIN_BATCH_DECODE)(const char* input_dir,
                                       const char* output_dir,
                                       opj_decompress_parameters* decoding_parameters,
                                       PLUGIN_DECODE_USER_CALLBACK userCallback);

static const char* plugin_stop_batch_decode_method_name = "plugin_stop_batch_decode";
typedef void(*PLUGIN_STOP_BATCH_DECODE)(void);


