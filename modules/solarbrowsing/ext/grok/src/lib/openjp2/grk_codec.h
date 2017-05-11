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
 * Main codec handler used for compression or decompression.
 */
typedef struct grk_codec_private {
    /** FIXME DOC */
    union {
        /**
         * Decompression handler.
         */
        struct opj_decompression {
            /** Main header reading function handler */
            bool (*opj_read_header) ( grk_stream_private_t * cio,
                                      void * p_codec,
									  opj_header_info_t* header_info,
                                      opj_image_t **p_image,
                                      grk_event_mgr_t * p_manager);

            /** Decoding function */
            bool (*opj_decode) ( void * p_codec,
								  opj_plugin_tile_t* tile,
                                 grk_stream_private_t * p_cio,
                                 opj_image_t * p_image,
                                 grk_event_mgr_t * p_manager);

            /** FIXME DOC */
            bool (*opj_read_tile_header)( void * p_codec,
                                          uint32_t * p_tile_index,
                                          uint64_t * p_data_size,
                                          uint32_t * p_tile_x0,
                                          uint32_t * p_tile_y0,
                                          uint32_t * p_tile_x1,
                                          uint32_t * p_tile_y1,
                                          uint32_t * p_nb_comps,
                                          bool * p_should_go_on,
                                          grk_stream_private_t * p_cio,
                                          grk_event_mgr_t * p_manager);

            /** FIXME DOC */
            bool (*opj_decode_tile_data)( void * p_codec,
                                          uint32_t p_tile_index,
                                          uint8_t * p_data,
                                          uint64_t p_data_size,
                                          grk_stream_private_t * p_cio,
                                          grk_event_mgr_t * p_manager);

            /** Reading function used after codestream if necessary */
            bool (* opj_end_decompress) ( void *p_codec,
                                          grk_stream_private_t * cio,
                                          grk_event_mgr_t * p_manager);

            /** Codec destroy function handler */
            void (*opj_destroy) (void * p_codec);

            /** Setup decoder function handler */
            void (*opj_setup_decoder) ( void * p_codec, opj_dparameters_t * p_param);

            /** Set decode area function handler */
            bool (*opj_set_decode_area) ( void * p_codec,
                                          opj_image_t * p_image,
                                          uint32_t p_start_x,
                                          uint32_t p_end_x,
                                          uint32_t p_start_y,
                                          uint32_t p_end_y,
                                          grk_event_mgr_t * p_manager);

            /** Get tile function */
            bool (*opj_get_decoded_tile) ( void *p_codec,
                                           grk_stream_private_t * p_cio,
                                           opj_image_t *p_image,
                                           grk_event_mgr_t * p_manager,
                                           uint32_t tile_index);

            /** Set the decoded resolution factor */
            bool (*opj_set_decoded_resolution_factor) ( void * p_codec,
                    uint32_t res_factor,
                    grk_event_mgr_t * p_manager);
        } m_decompression;

        /**
         * Compression handler. FIXME DOC
         */
        struct opj_compression {
            bool (* opj_start_compress) ( void *p_codec,
                                          grk_stream_private_t * cio,
                                          struct opj_image * p_image,
                                          grk_event_mgr_t * p_manager);

            bool (* opj_encode) ( void * p_codec,
									opj_plugin_tile_t*,
                                  grk_stream_private_t *p_cio,
                                  grk_event_mgr_t * p_manager);

            bool (* opj_write_tile) ( void * p_codec,
                                      uint32_t p_tile_index,
                                      uint8_t * p_data,
                                      uint64_t p_data_size,
                                      grk_stream_private_t * p_cio,
                                      grk_event_mgr_t * p_manager);

            bool (* opj_end_compress) (	void * p_codec,
                                        grk_stream_private_t * p_cio,
                                        grk_event_mgr_t * p_manager);

            void (* opj_destroy) (void * p_codec);

            bool (* opj_setup_encoder) ( void * p_codec,
                                         opj_cparameters_t * p_param,
                                         struct opj_image * p_image,
                                         grk_event_mgr_t * p_manager);
        } m_compression;
    } m_codec_data;
    /** FIXME DOC*/
    void * m_codec;
    /** Event handler */
    grk_event_mgr_t m_event_mgr;
    /** Flag to indicate if the codec is used to decode or encode*/
    bool is_decompressor;
    void (*opj_dump_codec) (void * p_codec, int32_t info_flag, FILE* output_stream);
    opj_codestream_info_v2_t* (*grk_get_codec_info)(void* p_codec);
    opj_codestream_index_t* (*opj_get_codec_index)(void* p_codec);
}
grk_codec_private_t;




