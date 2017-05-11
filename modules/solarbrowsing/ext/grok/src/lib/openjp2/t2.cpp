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

#include "grk_includes.h"
#include "testing.h"
#include <memory>

/** @defgroup T2 T2 - Implementation of a tier-2 coding */
/*@{*/

/** @name Local static functions */
/*@{*/

static void grk_t2_putcommacode(BitIO *bio, int32_t n);

static uint32_t grk_t2_getcommacode(BitIO *bio);
/**
Variable length code for signaling delta Zil (truncation point)
@param bio  Bit Input/Output component
@param n    delta Zil
*/
static void grk_t2_putnumpasses(BitIO *bio, uint32_t n);
static uint32_t grk_t2_getnumpasses(BitIO *bio);

/**
Encode a packet of a tile to a destination buffer
@param tileno Number of the tile encoded
@param tile Tile for which to write the packets
@param tcp Tile coding parameters
@param pi Packet identity
@param dest Destination buffer
@param p_data_written   FIXME DOC
@param len Length of the destination buffer
@param cstr_info Codestream information structure
@return
*/
static bool grk_t2_encode_packet(grk_t2_t* p_t2,
									uint32_t tileno,
                                    grk_tcd_tile_t *tile,
                                    grk_tcp_t *tcp,
                                    grk_pi_iterator_t *pi,
                                    uint8_t *dest,
                                    uint64_t * p_data_written,
                                    uint64_t len,
                                    opj_codestream_info_t *cstr_info,
									grk_event_mgr_t * p_manager);

/**
Encode a packet of a tile to a destination buffer
@param tileno Number of the tile encoded
@param tile Tile for which to write the packets
@param tcp Tile coding parameters
@param pi Packet identity
@param dest Destination buffer
@param p_data_written   FIXME DOC
@param len Length of the destination buffer
@param cstr_info Codestream information structure
@return
*/
static bool grk_t2_encode_packet_simulate(grk_tcd_tile_t *tile,
                                        grk_tcp_t *tcp,
                                        grk_pi_iterator_t *pi,
                                        uint64_t * p_data_written,
                                        uint64_t len);



/**
Decode a packet of a tile from a source buffer
@param t2 T2 handle
@param tile Tile for which to write the packets
@param tcp Tile coding parameters
@param pi Packet identity
@param src Source buffer
@param data_read   FIXME DOC
@param max_length  FIXME DOC
@param pack_info Packet information

@return  FIXME DOC
*/
static bool grk_t2_decode_packet(   grk_t2_t* t2,
									grk_tcd_resolution_t* l_res,
                                    grk_tcp_t *tcp,
                                    grk_pi_iterator_t *pi,
                                    opj_seg_buf_t* src_buf,
                                    uint64_t * data_read,
                                    grk_event_mgr_t *p_manager);

static bool grk_t2_skip_packet( grk_t2_t* p_t2,
                                grk_tcd_tile_t *p_tile,
                                grk_tcp_t *p_tcp,
                                grk_pi_iterator_t *p_pi,
                                opj_seg_buf_t* src_buf,
                                uint64_t * p_data_read,
                                grk_event_mgr_t *p_manager);

static bool grk_t2_read_packet_header(  grk_t2_t* p_t2,
										grk_tcd_resolution_t* l_res,
                                        grk_tcp_t *p_tcp,
                                        grk_pi_iterator_t *p_pi,
                                        bool * p_is_data_present,
                                        opj_seg_buf_t* src_buf,
                                        uint64_t * p_data_read,
                                        grk_event_mgr_t *p_manager);

static bool grk_t2_read_packet_data(grk_tcd_resolution_t* l_res,
                                    grk_pi_iterator_t *p_pi,
                                    opj_seg_buf_t* src_buf,
                                    uint64_t * p_data_read,
                                    grk_event_mgr_t *p_manager);

static bool grk_t2_skip_packet_data(grk_tcd_resolution_t* l_res,
                                    grk_pi_iterator_t *p_pi,
                                    uint64_t * p_data_read,
                                    uint64_t p_max_length,
                                    grk_event_mgr_t *p_manager);

/**
@param cblk
@param index
@param cblksty
@param first
*/
static bool grk_t2_init_seg(    grk_tcd_cblk_dec_t* cblk,
                                uint32_t index,
                                uint32_t cblksty,
                                uint32_t first);

static bool t2_empty_of_code_blocks(grk_tcd_band_t* band) {
	return ((band->x1 - band->x0 == 0) || (band->y1 - band->y0 == 0));
}

/*@}*/

/*@}*/

/* ----------------------------------------------------------------------- */

/* #define RESTART 0x04 */
static void grk_t2_putcommacode(BitIO *bio, int32_t n)
{
    while (--n >= 0) {
        bio->write( 1, 1);
    }
    bio->write( 0, 1);
}

static uint32_t grk_t2_getcommacode(BitIO *bio)
{
    uint32_t n = 0;
    while (bio->read( 1)) {
        ++n;
    }
    return n;
}

static void grk_t2_putnumpasses(BitIO *bio, uint32_t n)
{
    if (n == 1) {
        bio->write( 0, 1);
    } else if (n == 2) {
        bio->write( 2, 2);
    } else if (n <= 5) {
        bio->write( 0xc | (n - 3), 4);
    } else if (n <= 36) {
        bio->write( 0x1e0 | (n - 6), 9);
    } else if (n <= 164) {
        bio->write( 0xff80 | (n - 37), 16);
    }
}

static uint32_t grk_t2_getnumpasses(BitIO *bio)
{
    uint32_t n;
    if (!bio->read( 1))
        return 1;
    if (!bio->read( 1))
        return 2;
    if ((n = bio->read( 2)) != 3)
        return (3 + n);
    if ((n = bio->read( 5)) != 31)
        return (6 + n);
    return (37 + bio->read( 7));
}

/* ----------------------------------------------------------------------- */

bool grk_t2_encode_packets( grk_t2_t* p_t2,
                            uint32_t p_tile_no,
                            grk_tcd_tile_t *p_tile,
                            uint32_t p_maxlayers,
                            uint8_t *p_dest,
                            uint64_t * p_data_written,
                            uint64_t p_max_len,
                            opj_codestream_info_t *cstr_info,
                            uint32_t p_tp_num,
                            uint32_t p_tp_pos,
                            uint32_t p_pino,
							grk_event_mgr_t * p_manager)
{
    uint8_t *l_current_data = p_dest;
    uint64_t l_nb_bytes = 0;
    grk_pi_iterator_t *l_pi = nullptr;
    grk_pi_iterator_t *l_current_pi = nullptr;
    opj_image_t *l_image = p_t2->image;
    opj_cp_t *l_cp = p_t2->cp;
    grk_tcp_t *l_tcp = &l_cp->tcps[p_tile_no];
    uint32_t l_nb_pocs = l_tcp->numpocs + 1;

    l_pi = grk_pi_initialise_encode(l_image, l_cp, p_tile_no, FINAL_PASS);
    if (!l_pi) {
        return false;
    }

    * p_data_written = 0;

    grk_pi_init_encode(l_pi, l_cp,p_tile_no,p_pino,p_tp_num,p_tp_pos, FINAL_PASS);

    l_current_pi = &l_pi[p_pino];
    if (l_current_pi->poc.prg == OPJ_PROG_UNKNOWN) {
        /* TODO ADE : add an error */
        grk_pi_destroy(l_pi, l_nb_pocs);
        return false;
    }
    while (grk_pi_next(l_current_pi)) {
        if (l_current_pi->layno < p_maxlayers) {
            l_nb_bytes=0;

            if (! grk_t2_encode_packet(p_t2,
										p_tile_no,
										p_tile, 
										l_tcp,
										l_current_pi,
										l_current_data, 
										&l_nb_bytes, 
										p_max_len,
										cstr_info, 
										p_manager)) {
                grk_pi_destroy(l_pi, l_nb_pocs);
                return false;
            }

            l_current_data += l_nb_bytes;
            p_max_len -= l_nb_bytes;

            * p_data_written += l_nb_bytes;

            /* INDEX >> */
            if(cstr_info) {
                if(cstr_info->index_write) {
                    opj_tile_info_t *info_TL = &cstr_info->tile[p_tile_no];
                    opj_packet_info_t *info_PK = &info_TL->packet[cstr_info->packno];
                    if (!cstr_info->packno) {
                        info_PK->start_pos = info_TL->end_header + 1;
                    } else {
                        info_PK->start_pos = ((l_cp->m_specific_param.m_enc.m_tp_on | l_tcp->POC)&& info_PK->start_pos) ? info_PK->start_pos : info_TL->packet[cstr_info->packno - 1].end_pos + 1;
                    }
                    info_PK->end_pos = info_PK->start_pos + l_nb_bytes - 1;
                    info_PK->end_ph_pos += info_PK->start_pos - 1;  /* End of packet header which now only represents the distance
                                                                                                                                                                                                                                                to start of packet is incremented by value of start of packet*/
                }

                cstr_info->packno++;
            }
            /* << INDEX */
            ++p_tile->packno;
        }
    }

    grk_pi_destroy(l_pi, l_nb_pocs);

    return true;
}


bool grk_t2_encode_packets_simulate(grk_t2_t* p_t2,
                                  uint32_t p_tile_no,
                                  grk_tcd_tile_t *p_tile,
                                  uint32_t p_maxlayers,
                                  uint64_t * p_data_written,
                                  uint64_t p_max_len,
                                  uint32_t p_tp_pos)
{
    opj_image_t *l_image = p_t2->image;
    opj_cp_t *l_cp = p_t2->cp;
    grk_tcp_t *l_tcp = l_cp->tcps + p_tile_no;
    uint32_t pocno = (l_cp->rsiz == OPJ_PROFILE_CINEMA_4K) ? 2 : 1;
    uint32_t l_max_comp = l_cp->m_specific_param.m_enc.m_max_comp_size > 0 ? l_image->numcomps : 1;
    uint32_t l_nb_pocs = l_tcp->numpocs + 1;

	if (!p_data_written)
		return false;

	grk_pi_iterator_t* l_pi = grk_pi_initialise_encode(l_image, l_cp, p_tile_no, THRESH_CALC);
    if (!l_pi) {
        return false;
    }
    *p_data_written = 0;
	grk_pi_iterator_t * l_current_pi = l_pi;

    for (uint32_t compno = 0; compno < l_max_comp; ++compno) {
        uint64_t l_comp_len = 0;
        l_current_pi = l_pi;

        for (uint32_t poc = 0; poc < pocno; ++poc) {
            uint32_t l_tp_num = compno;
            grk_pi_init_encode(l_pi, l_cp, p_tile_no, poc, l_tp_num, p_tp_pos, THRESH_CALC);

            if (l_current_pi->poc.prg == OPJ_PROG_UNKNOWN) {
                /* TODO ADE : add an error */
                grk_pi_destroy(l_pi, l_nb_pocs);
                return false;
            }
            while (grk_pi_next(l_current_pi)) {
                if (l_current_pi->layno < p_maxlayers) {
					uint64_t bytesInPacket = 0;
                    if (!grk_t2_encode_packet_simulate(p_tile, l_tcp, l_current_pi, &bytesInPacket, p_max_len)) {
                        grk_pi_destroy(l_pi, l_nb_pocs);
                        return false;
                    }

                    l_comp_len += bytesInPacket;
                    p_max_len -= bytesInPacket;
                    *p_data_written += bytesInPacket;
                }
            }

            if (l_cp->m_specific_param.m_enc.m_max_comp_size) {
                if (l_comp_len > l_cp->m_specific_param.m_enc.m_max_comp_size) {
                    grk_pi_destroy(l_pi, l_nb_pocs);
                    return false;
                }
            }

            ++l_current_pi;
        }
    }
    grk_pi_destroy(l_pi, l_nb_pocs);
    return true;
}

/* see issue 80 */
#if 0
#define JAS_FPRINTF fprintf
#else
/* issue 290 */
static void opj_null_jas_fprintf(FILE* file, const char * format, ...)
{
    (void)file;
    (void)format;
}
#define JAS_FPRINTF opj_null_jas_fprintf
#endif

bool grk_t2_decode_packets( grk_t2_t *p_t2,
                            uint32_t p_tile_no,
                            grk_tcd_tile_t *p_tile,
                            opj_seg_buf_t* src_buf,
                            uint64_t * p_data_read,
                            grk_event_mgr_t *p_manager)
{
    grk_pi_iterator_t *l_pi = nullptr;
    uint32_t pino;
    opj_image_t *l_image = p_t2->image;
    opj_cp_t *l_cp = p_t2->cp;
    grk_tcp_t *l_tcp = p_t2->cp->tcps + p_tile_no;
    uint64_t l_nb_bytes_read;
    uint32_t l_nb_pocs = l_tcp->numpocs + 1;
    grk_pi_iterator_t *l_current_pi = nullptr;
    opj_image_comp_t* l_img_comp = nullptr;

    /* create a packet iterator */
    l_pi = grk_pi_create_decode(l_image, l_cp, p_tile_no);
    if (!l_pi) {
        return false;
    }


    l_current_pi = l_pi;

    for     (pino = 0; pino <= l_tcp->numpocs; ++pino) {

        /* if the resolution needed is too low, one dim of the tilec could be equal to zero
         * and no packets are used to decode this resolution and
         * l_current_pi->resno is always >= p_tile->comps[l_current_pi->compno].minimum_num_resolutions
         * and no l_img_comp->resno_decoded are computed
         */

        if (l_current_pi->poc.prg == OPJ_PROG_UNKNOWN) {
            /* TODO ADE : add an error */
            grk_pi_destroy(l_pi, l_nb_pocs);
            return false;
        }
        while (grk_pi_next(l_current_pi)) {
            bool skip_precinct = false;
            grk_tcd_tilecomp_t* tilec = p_tile->comps + l_current_pi->compno;
            bool skip_layer_or_res = l_current_pi->layno >= l_tcp->num_layers_to_decode ||
                                     l_current_pi->resno >= tilec->minimum_num_resolutions;

            l_img_comp = l_image->comps + l_current_pi->compno;

            JAS_FPRINTF( stderr,
                         "packet offset=00000166 prg=%d cmptno=%02d rlvlno=%02d prcno=%03d lyrno=%02d\n\n",
                         l_current_pi->poc.prg1,
                         l_current_pi->compno,
                         l_current_pi->resno,
                         l_current_pi->precno,
                         l_current_pi->layno );



            if (!skip_layer_or_res) {
                grk_tcd_resolution_t* res = tilec->resolutions + l_current_pi->resno;
                uint32_t bandno;
                skip_precinct = true;
                for (bandno = 0; bandno < res->numbands && skip_precinct; ++bandno) {
                    grk_tcd_band_t* band = res->bands + bandno;
					auto num_precincts = band->numPrecincts();
                    uint32_t precno;
                    for (precno = 0; precno < num_precincts && skip_precinct; ++precno) {
                        rect_t prec_rect;
                        grk_tcd_precinct_t* prec = band->precincts + precno;
						prec_rect= rect_t(prec->x0, prec->y0, prec->x1, prec->y1);
                        if (grk_tile_buf_hit_test(tilec->buf,&prec_rect)) {
                            skip_precinct = false;
                            break;
                        }
                    }
                }
            }

            if (!skip_layer_or_res && !skip_precinct) {
                l_nb_bytes_read = 0;
                if (! grk_t2_decode_packet(p_t2,
											&p_tile->comps[l_current_pi->compno].resolutions[l_current_pi->resno],
                                           l_tcp,
                                           l_current_pi,
                                           src_buf,
                                           &l_nb_bytes_read,
                                           p_manager)) {
                    grk_pi_destroy(l_pi,l_nb_pocs);
                    return false;
                }
            } else {
                l_nb_bytes_read = 0;
                if (! grk_t2_skip_packet(p_t2,
                                         p_tile,
                                         l_tcp,l_current_pi,
                                         src_buf,
                                         &l_nb_bytes_read,
										p_manager)) {
                    grk_pi_destroy(l_pi,l_nb_pocs);
                    return false;
                }
            }

            if (!skip_layer_or_res)
                l_img_comp->resno_decoded = grk_max<uint32_t>(l_current_pi->resno, l_img_comp->resno_decoded);
            *p_data_read += l_nb_bytes_read;
        }
        ++l_current_pi;
    }
    grk_pi_destroy(l_pi,l_nb_pocs);
    return true;
}

/* ----------------------------------------------------------------------- */

/**
 * Creates a Tier 2 handle
 *
 * @param       p_image         Source or destination image
 * @param       p_cp            Image coding parameters.
 * @return              a new T2 handle if successful, NULL otherwise.
*/
grk_t2_t* grk_t2_create(opj_image_t *p_image, opj_cp_t *p_cp)
{
    /* create the t2 structure */
    grk_t2_t *l_t2 = (grk_t2_t*)grk_calloc(1,sizeof(grk_t2_t));
    if (!l_t2) {
        return NULL;
    }
    l_t2->image = p_image;
    l_t2->cp = p_cp;
    return l_t2;
}

void grk_t2_destroy(grk_t2_t *t2)
{
    if(t2) {
        grk_free(t2);
    }
}

static bool grk_t2_decode_packet(  grk_t2_t* p_t2,
									grk_tcd_resolution_t* l_res,
                                   grk_tcp_t *p_tcp,
                                   grk_pi_iterator_t *p_pi,
                                   opj_seg_buf_t* src_buf,
                                   uint64_t * p_data_read,
                                   grk_event_mgr_t *p_manager)
{
    bool l_read_data;
    uint64_t l_nb_bytes_read = 0;
    uint64_t l_nb_total_bytes_read = 0;
    *p_data_read = 0;

    if (! grk_t2_read_packet_header(p_t2,
									l_res,
									p_tcp,
									p_pi,
									&l_read_data,
                                    src_buf,
                                    &l_nb_bytes_read,
                                    p_manager)) {
        return false;
    }
    l_nb_total_bytes_read += l_nb_bytes_read;

    /* we should read data for the packet */
    if (l_read_data) {
        l_nb_bytes_read = 0;
        if (! grk_t2_read_packet_data(l_res,
										p_pi,
										src_buf,
										&l_nb_bytes_read,
										p_manager)) {
            return false;
        }
        l_nb_total_bytes_read += l_nb_bytes_read;
    }
    *p_data_read = l_nb_total_bytes_read;
    return true;
}

static bool grk_t2_read_packet_header(grk_t2_t* p_t2,
									grk_tcd_resolution_t* l_res,
									grk_tcp_t *p_tcp,
									grk_pi_iterator_t *p_pi,
									bool * p_is_data_present,
									opj_seg_buf_t* src_buf,
									uint64_t * p_data_read,
									grk_event_mgr_t *p_manager)

{
	uint8_t *p_src_data = opj_seg_buf_get_global_ptr(src_buf);
	uint32_t p_max_length = (uint32_t)opj_seg_buf_get_cur_seg_len(src_buf);

	uint32_t bandno, cblkno;
	uint32_t l_nb_code_blocks;
	uint32_t l_remaining_length;
	uint32_t l_header_length;
	uint32_t * l_modified_length_ptr = nullptr;
	uint8_t *l_current_data = p_src_data;
	opj_cp_t *l_cp = p_t2->cp;
	grk_tcd_band_t *l_band = nullptr;
	grk_tcd_cblk_dec_t* l_cblk = nullptr;

	uint8_t *l_header_data = nullptr;
	uint8_t **l_header_data_start = nullptr;

	uint32_t l_present;

	if (p_pi->layno == 0) {
		/* reset tagtrees */
		for (bandno = 0; bandno < l_res->numbands; ++bandno) {
			l_band = l_res->bands + bandno;
			if (t2_empty_of_code_blocks(l_band))
				continue;
			grk_tcd_precinct_t *l_prc = &l_band->precincts[p_pi->precno];
			if (!(p_pi->precno < (l_band->numPrecincts()))) {
				grk_event_msg(p_manager, EVT_ERROR, "Invalid precinct\n");
				return false;
			}
			if (l_prc->incltree)
				l_prc->incltree->reset();
			if (l_prc->imsbtree)
				l_prc->imsbtree->reset();
			l_nb_code_blocks = l_prc->cw * l_prc->ch;
			for (cblkno = 0; cblkno < l_nb_code_blocks; ++cblkno) {
				l_cblk = l_prc->cblks.dec + cblkno;
				l_cblk->numSegments = 0;
			}
		}
	}

	/* SOP markers */
	if (p_tcp->csty & J2K_CP_CSTY_SOP) {
		if (p_max_length < 6) {
			grk_event_msg(p_manager, EVT_WARNING, "Not enough space for expected SOP marker\n");
		}
		else if ((*l_current_data) != 0xff || (*(l_current_data + 1) != 0x91)) {
			grk_event_msg(p_manager, EVT_WARNING, "Expected SOP marker\n");
		}
		else {
			l_current_data += 6;
		}
		/** TODO : check the Nsop value */
	}

	/*
	When the marker PPT/PPM is used the packet header are store in PPT/PPM marker
	This part deal with this characteristic
	step 1: Read packet header in the saved structure
	step 2: Return to codestream for decoding
	*/
	std::unique_ptr<BitIO> l_bio(new BitIO());

	if (l_cp->ppm == 1) { /* PPM */
		l_header_data_start = &l_cp->ppm_data;
		l_header_data = *l_header_data_start;
		l_modified_length_ptr = &(l_cp->ppm_len);

	}
	else if (p_tcp->ppt == 1) { /* PPT */
		l_header_data_start = &(p_tcp->ppt_data);
		l_header_data = *l_header_data_start;
		l_modified_length_ptr = &(p_tcp->ppt_len);
	}
	else { /* Normal Case */
		l_header_data_start = &(l_current_data);
		l_header_data = *l_header_data_start;
		l_remaining_length = (uint32_t)(p_src_data + p_max_length - l_header_data);
		l_modified_length_ptr = &(l_remaining_length);
	}

	l_bio->init_dec(l_header_data, *l_modified_length_ptr);

	l_present = l_bio->read(1);
	JAS_FPRINTF(stderr, "present=%d \n", l_present);
	if (!l_present) {
		if (!l_bio->inalign())
			return false;
		l_header_data += l_bio->numbytes();

		/* EPH markers */
		if (p_tcp->csty & J2K_CP_CSTY_EPH) {
			if ((*l_modified_length_ptr - (uint32_t)(l_header_data - *l_header_data_start)) < 2U) {
				grk_event_msg(p_manager, EVT_WARNING, "Not enough space for expected EPH marker\n");
			}
			else if ((*l_header_data) != 0xff || (*(l_header_data + 1) != 0x92)) {
				grk_event_msg(p_manager, EVT_WARNING, "Expected EPH marker\n");
			}
			else {
				l_header_data += 2;
			}
		}

		l_header_length = (uint32_t)(l_header_data - *l_header_data_start);
		*l_modified_length_ptr -= l_header_length;
		*l_header_data_start += l_header_length;

		*p_is_data_present = false;
		*p_data_read = (uint32_t)(l_current_data - p_src_data);
		opj_seg_buf_incr_cur_seg_offset(src_buf, *p_data_read);
		return true;
	}

	for (bandno = 0; bandno < l_res->numbands; ++bandno) {
		l_band = l_res->bands + bandno;
		if (t2_empty_of_code_blocks(l_band)) {
			continue;
		}

		grk_tcd_precinct_t *l_prc = l_band->precincts + p_pi->precno;
		l_nb_code_blocks = l_prc->cw * l_prc->ch;
		for (cblkno = 0; cblkno < l_nb_code_blocks; cblkno++) {
			uint32_t l_included, l_increment, l_segno;
			int32_t n;
			l_cblk = l_prc->cblks.dec + cblkno;

			/* if cblk not yet included before --> inclusion tagtree */
			if (!l_cblk->numSegments) {
				auto value = l_prc->incltree->decodeValue(l_bio.get(), cblkno, (int32_t)(p_pi->layno + 1));
				if (value != tag_tree_uninitialized_node_value && value != p_pi->layno) {
					grk_event_msg(p_manager, EVT_WARNING, "Illegal inclusion tag tree found when decoding packet header\n");
				}
				l_included = (value <= (int32_t)p_pi->layno) ? 1 : 0;
			}
			/* else one bit */
			else {
				l_included = l_bio->read(1);
			}

			/* if cblk not included */
			if (!l_included) {
				l_cblk->numPassesInPacket = 0;
				JAS_FPRINTF(stderr, "included=%d \n", l_included);
				continue;
			}

			/* if cblk not yet included --> zero-bitplane tagtree */
			if (!l_cblk->numSegments) {
				uint32_t i = 0;
				while (!l_prc->imsbtree->decode(l_bio.get(), cblkno, (int32_t)i)) {
					++i;
				}

				l_cblk->numbps = l_band->numbps + 1 - i;
				// BIBO analysis gives upper limit on number of bit planes
				if (l_cblk->numbps > OPJ_MAX_PRECISION + OPJ_J2K_MAXRLVLS * 5) {
					grk_event_msg(p_manager, EVT_WARNING, "Number of bit planes %u is impossibly large.\n", l_cblk->numbps);
				}
				l_cblk->numlenbits = 3;
			}

			/* number of coding passes */
			l_cblk->numPassesInPacket = grk_t2_getnumpasses(l_bio.get());
			l_increment = grk_t2_getcommacode(l_bio.get());

			/* length indicator increment */
			l_cblk->numlenbits += l_increment;
			l_segno = 0;

			if (!l_cblk->numSegments) {
				if (!grk_t2_init_seg(l_cblk, l_segno, p_tcp->tccps[p_pi->compno].cblksty, 1)) {
					return false;
				}
			}
			else {
				l_segno = l_cblk->numSegments - 1;
				if (l_cblk->segs[l_segno].numpasses == l_cblk->segs[l_segno].maxpasses) {
					++l_segno;
					if (!grk_t2_init_seg(l_cblk, l_segno, p_tcp->tccps[p_pi->compno].cblksty, 0)) {
						return false;
					}
				}
			}
			n = (int32_t)l_cblk->numPassesInPacket;
			do {
				auto l_seg = l_cblk->segs + l_segno;
				l_seg->numPassesInPacket = (uint32_t)grk_min<int32_t>((int32_t)(l_seg->maxpasses - l_seg->numpasses), n);
				l_seg->newlen = l_bio->read(l_cblk->numlenbits + grk_uint_floorlog2(l_seg->numPassesInPacket));
				JAS_FPRINTF(stderr, "included=%d numPassesInPacket=%d increment=%d len=%d \n", l_included, l_seg->numPassesInPacket, l_increment, l_seg->newlen);

				size_t offset = (size_t)opj_seg_buf_get_global_offset(src_buf);
				size_t len = src_buf->data_len;
				// Check possible overflow on segment length
				if (((offset + l_seg->newlen) >	len)) {
					grk_event_msg(p_manager, EVT_ERROR, "read packet: segment length (%u) plus segment offset %u is greater than total length of all segments (%u) for codeblock %d (p=%d, b=%d, r=%d, c=%d)\n",
						l_seg->newlen, offset, len, cblkno, p_pi->precno, bandno, p_pi->resno, p_pi->compno);
					return false;
				}

				n -= (int32_t)l_cblk->segs[l_segno].numPassesInPacket;
				if (n > 0) {
					++l_segno;

					if (!grk_t2_init_seg(l_cblk, l_segno, p_tcp->tccps[p_pi->compno].cblksty, 0)) {
						return false;
					}
				}
			} while (n > 0);
		}
	}

	if (!l_bio->inalign()) {
		return false;
	}

	l_header_data += l_bio->numbytes();

	/* EPH markers */
	if (p_tcp->csty & J2K_CP_CSTY_EPH) {
		if ((*l_modified_length_ptr - (uint32_t)(l_header_data - *l_header_data_start)) < 2U) {
			grk_event_msg(p_manager, EVT_WARNING, "Not enough space for expected EPH marker\n");
		}
		else if ((*l_header_data) != 0xff || (*(l_header_data + 1) != 0x92)) {
			grk_event_msg(p_manager, EVT_WARNING, "Expected EPH marker\n");
		}
		else {
			l_header_data += 2;
		}
	}

	l_header_length = (uint32_t)(l_header_data - *l_header_data_start);
	JAS_FPRINTF(stderr, "hdrlen=%d \n", l_header_length);
	JAS_FPRINTF(stderr, "packet body\n");
	*l_modified_length_ptr -= l_header_length;
	*l_header_data_start += l_header_length;

	*p_is_data_present = true;
	*p_data_read = (uint32_t)(l_current_data - p_src_data);
	opj_seg_buf_incr_cur_seg_offset(src_buf, *p_data_read);

	return true;
}

static bool grk_t2_read_packet_data(grk_tcd_resolution_t* l_res,
									grk_pi_iterator_t *p_pi,
									opj_seg_buf_t* src_buf,
									uint64_t * p_data_read,
									grk_event_mgr_t* p_manager)
{
	uint32_t bandno, cblkno;
	uint32_t l_nb_code_blocks;
	grk_tcd_band_t *l_band = nullptr;
	grk_tcd_cblk_dec_t* l_cblk = nullptr;
	//grk_tcd_resolution_t* l_res = 

	l_band = l_res->bands;
	for (bandno = 0; bandno < l_res->numbands; ++bandno) {
		grk_tcd_precinct_t *l_prc = &l_band->precincts[p_pi->precno];
		l_nb_code_blocks = l_prc->cw * l_prc->ch;
		l_cblk = l_prc->cblks.dec;

		for (cblkno = 0; cblkno < l_nb_code_blocks; ++cblkno) {
			grk_tcd_seg_t *l_seg = nullptr;

			if (!l_cblk->numPassesInPacket) {
				/* nothing to do */
				++l_cblk;
				continue;
			}

			if (!l_cblk->numSegments) {
				l_seg = l_cblk->segs;
				++l_cblk->numSegments;
				l_cblk->dataSize = 0;
			}
			else {
				l_seg = &l_cblk->segs[l_cblk->numSegments - 1];

				if (l_seg->numpasses == l_seg->maxpasses) {
					++l_seg;
					++l_cblk->numSegments;
				}
			}

			do {
				size_t offset	= (size_t)opj_seg_buf_get_global_offset(src_buf);
				size_t len		= src_buf->data_len;
				// Check possible overflow on segment length
				if (((offset + l_seg->newlen) >	len)) {
					grk_event_msg(p_manager, EVT_ERROR, "read packet: segment length (%u) plus segment offset %u is greater than total length of all segments (%u) for codeblock %d (p=%d, b=%d, r=%d, c=%d)\n",
						l_seg->newlen, offset, len, cblkno, p_pi->precno, bandno, p_pi->resno, p_pi->compno);
					return false;
				}

				if (l_seg->numpasses == 0) {
					l_seg->dataindex = l_cblk->dataSize;
				}

				grk_min_buf_vec_push_back(&l_cblk->seg_buffers, opj_seg_buf_get_global_ptr(src_buf), (uint16_t)l_seg->newlen);

				*(p_data_read)				+= l_seg->newlen;
				opj_seg_buf_incr_cur_seg_offset(src_buf, l_seg->newlen);
				l_seg->numpasses			+= l_seg->numPassesInPacket;
				l_cblk->numPassesInPacket	-= l_seg->numPassesInPacket;

				l_cblk->dataSize			+= l_seg->newlen;
				l_seg->len					+= l_seg->newlen;

				if (l_cblk->numPassesInPacket > 0) {
					++l_seg;
					++l_cblk->numSegments;
				}
			} while (l_cblk->numPassesInPacket > 0);
			++l_cblk;
		} /* next code_block */

		++l_band;
	}
	return true;
}
//--------------------------------------------------------------------------------------------------

static bool grk_t2_encode_packet(grk_t2_t* p_t2,
									uint32_t tileno,
                                   grk_tcd_tile_t * tile,
                                   grk_tcp_t * tcp,
                                   grk_pi_iterator_t *pi,
                                   uint8_t *dest,
                                   uint64_t * p_data_written,
                                   uint64_t length,
                                   opj_codestream_info_t *cstr_info,
									grk_event_mgr_t * p_manager)
{
    uint32_t bandno, cblkno;
    uint8_t* c = dest;
    uint64_t l_nb_bytes;
    uint32_t compno = pi->compno;    
    uint32_t resno  = pi->resno;      
    uint32_t precno = pi->precno;     
    uint32_t layno  = pi->layno;     
    uint32_t l_nb_blocks;
    grk_tcd_band_t *band = nullptr;
    grk_tcd_cblk_enc_t* cblk = nullptr;
    grk_tcd_pass_t *pass = nullptr;

    grk_tcd_tilecomp_t *tilec = &tile->comps[compno];
    grk_tcd_resolution_t *res = &tilec->resolutions[resno];

    // SOP marker
    if (tcp->csty & J2K_CP_CSTY_SOP) {
        c[0] = 255;
        c[1] = 145;
        c[2] = 0;
        c[3] = 4;
        c[4] = (tile->packno >> 8) & 0xff; /* packno is uint32_t, in big endian format */
        c[5] = tile->packno & 0xff;
        c += 6;
        length -= 6;
    }

	// initialize precinct and code blocks if this is the first layer
    if (!layno) {
        band = res->bands;
        for(bandno = 0; bandno < res->numbands; ++bandno) {
			if (t2_empty_of_code_blocks(band)) {
				band++;
				continue;
			}
            grk_tcd_precinct_t *prc = &band->precincts[precno];
			if (prc->incltree)
				prc->incltree->reset();
			if (prc->imsbtree)
				prc->imsbtree->reset();

            l_nb_blocks = prc->cw * prc->ch;
            for  (cblkno = 0; cblkno < l_nb_blocks; ++cblkno) {
                cblk = &prc->cblks.enc[cblkno];
                cblk->num_passes_included_in_current_layer = 0;
				prc->imsbtree->setvalue( cblkno, band->numbps - (int32_t)cblk->numbps);
            }
            ++band;
        }
    }

	auto bio = new BitIO();
    if (!bio) {
        return false;
    }
    bio->init_enc(c, length);
    bio->write( 1, 1);           // Empty header bit. Grok always sets this to 1,
								 // even though there is also an option to set it to zero.

    /* Writing Packet header */
    band = res->bands;
    for (bandno = 0; bandno < res->numbands; ++bandno)      {
		if (t2_empty_of_code_blocks(band)) {
			band++;
			continue;
		}
        auto prc = &band->precincts[precno];
        l_nb_blocks = prc->cw * prc->ch;
        cblk = prc->cblks.enc;

        for (cblkno = 0; cblkno < l_nb_blocks; ++cblkno) {
            grk_tcd_layer_t *layer = &cblk->layers[layno];
            if (!cblk->num_passes_included_in_current_layer && layer->numpasses) {
				prc->incltree->setvalue( cblkno, (int32_t)layno);
            }
            ++cblk;
        }

        cblk = prc->cblks.enc;
        for (cblkno = 0; cblkno < l_nb_blocks; cblkno++) {
            grk_tcd_layer_t *layer = &cblk->layers[layno];
            uint32_t increment = 0;
            uint32_t nump = 0;
            uint32_t len = 0, passno;
            uint32_t l_nb_passes;

            /* cblk inclusion bits */
            if (!cblk->num_passes_included_in_current_layer) {
				prc->incltree->encode(bio,  cblkno, (int32_t)(layno + 1));
            } else {
                bio->write(layer->numpasses != 0, 1);
            }

            /* if cblk not included, go to next cblk  */
            if (!layer->numpasses) {
                ++cblk;
                continue;
            }

            /* if first instance of cblk --> zero bit-planes information */
            if (!cblk->num_passes_included_in_current_layer) {
                cblk->numlenbits = 3;
				prc->imsbtree->encode(bio, cblkno, tag_tree_uninitialized_node_value);
            }

#ifdef DEBUG_LOSSLESS_T2
			if (resno == 1 && precno == 0 && band->bandno == 2 && cblkno == 0) {
				int k = 0;
			}
#endif
            /* number of coding passes included */
            grk_t2_putnumpasses(bio, layer->numpasses);
            l_nb_passes = cblk->num_passes_included_in_current_layer + layer->numpasses;
            pass		= cblk->passes +  cblk->num_passes_included_in_current_layer;

            /* computation of the increase of the length indicator and insertion in the header     */
            for (passno = cblk->num_passes_included_in_current_layer; passno < l_nb_passes; ++passno) {
                ++nump;
                len += pass->len;

                if (pass->term || passno == (cblk->num_passes_included_in_current_layer + layer->numpasses) - 1) {
                    increment = (uint32_t)grk_max<int32_t>((int32_t)increment, grk_int_floorlog2((int32_t)len) + 1
                                                      - ((int32_t)cblk->numlenbits + grk_int_floorlog2((int32_t)nump)));
                    len = 0;
                    nump = 0;
                }
                ++pass;
            }
            grk_t2_putcommacode(bio, (int32_t)increment);

            /* computation of the new Length indicator */
            cblk->numlenbits += increment;

            pass = cblk->passes +  cblk->num_passes_included_in_current_layer;
            /* insertion of the codeword segment length */
            for (passno = cblk->num_passes_included_in_current_layer; passno < l_nb_passes; ++passno) {
                nump++;
                len += pass->len;

                if (pass->term || passno == (cblk->num_passes_included_in_current_layer + layer->numpasses) - 1) {
                    bio->write( len, cblk->numlenbits + (uint32_t)grk_int_floorlog2((int32_t)nump));
                    len = 0;
                    nump = 0;
                }
                ++pass;
            }
            ++cblk;
        }
        ++band;
    }

    if (!bio->flush()) {
		grk_event_msg(p_manager, EVT_ERROR, "Bit IO flush failed while encoding packet\n");
        delete bio;
        return false;            
    }

    l_nb_bytes = (uint64_t)bio->numbytes();
    c += l_nb_bytes;
    length -= l_nb_bytes;

    delete bio;

    // EPH marker
    if (tcp->csty & J2K_CP_CSTY_EPH) {
        c[0] = 255;
        c[1] = 146;
        c += 2;
        length -= 2;
    }


    /* << INDEX */
    /* End of packet header position. Currently only represents the distance to start of packet
       Will be updated later by incrementing with packet start value*/
    if(cstr_info && cstr_info->index_write) {
        opj_packet_info_t *info_PK = &cstr_info->tile[tileno].packet[cstr_info->packno];
        info_PK->end_ph_pos = (int32_t)(c - dest);
    }
    /* INDEX >> */

    /* Writing the packet body */
    band = res->bands;
    for (bandno = 0; bandno < res->numbands; bandno++) {
		if (t2_empty_of_code_blocks(band)) {
			band++;
			continue;
		}
        grk_tcd_precinct_t *prc = &band->precincts[precno];
        l_nb_blocks = prc->cw * prc->ch;
        cblk = prc->cblks.enc;

        for (cblkno = 0; cblkno < l_nb_blocks; ++cblkno) {
            grk_tcd_layer_t *cblk_layer = &cblk->layers[layno];

            if (!cblk_layer->numpasses) {
                ++cblk;
                continue;
            }

            if (cblk_layer->len > length) {
				grk_event_msg(p_manager, EVT_ERROR, "Code block layer size exceeds size of tile buffer\n");
				return false;
            }

            memcpy(c, cblk_layer->data, cblk_layer->len);
            cblk->num_passes_included_in_current_layer += cblk_layer->numpasses;
            c += cblk_layer->len;
            length -= cblk_layer->len;
            if(cstr_info && cstr_info->index_write) {
                opj_packet_info_t *info_PK = &cstr_info->tile[tileno].packet[cstr_info->packno];
                info_PK->disto += cblk_layer->disto;
                if (cstr_info->D_max < info_PK->disto) {
                    cstr_info->D_max = info_PK->disto;
                }
            }
            ++cblk;
        }
        ++band;
    }
    * p_data_written += (uint32_t)(c - dest);

#ifdef DEBUG_LOSSLESS_T2
	grk_tcd_resolution_t decodeRes = *res;

	// allocate
	for (uint32_t bandno = 0; bandno < res->numbands; ++bandno) {
		auto band = res->bands+bandno;
		auto decodeBand = decodeRes.bands + bandno;
		if (!band->precincts)
			continue;
		decodeBand->precincts = new grk_tcd_precinct_t[band->numPrecincts()];
		decodeBand->precincts_data_size = (uint32_t)(band->numPrecincts() * sizeof(grk_tcd_precinct_t));
		for (size_t precno = 0; precno < band->numPrecincts(); ++precno) {
			auto prec = band->precincts + precno;
			auto decodePrec = decodeBand->precincts + precno;
			*decodePrec = grk_tcd_precinct_t(*prec);
			if (prec->cblks.enc && prec->cw && prec->ch) {
				decodePrec->cw = prec->cw;
				decodePrec->ch = prec->ch;
				decodePrec->initTagTrees(p_manager);
				decodePrec->cblks.dec = new grk_tcd_cblk_dec_t[prec->cw * prec->ch];
				for (uint32_t cblkno = 0; cblkno < prec->cw * prec->ch; ++cblkno) {
					auto cblk = prec->cblks.enc + cblkno;
					auto decodeCblk = decodePrec->cblks.dec + cblkno;
					*decodeCblk = grk_tcd_cblk_dec_t(*cblk);
					decodeCblk->numbps = 0;
					decodeCblk->numlenbits = 0;
					decodeCblk->alloc();
				}
			}
		}
	}

	size_t l_nb_bytes_read = 0;
	opj_seg_buf_t* src_buf= new opj_seg_buf_t();
	opj_seg_buf_push_back(src_buf, dest, *p_data_written);
	
	if (grk_t2_decode_packet(p_t2,
							&decodeRes,
							tcp,
							pi,
							src_buf,
							&l_nb_bytes_read,
							p_manager)) {


		//do comparison
		for (uint32_t bandno = 0; bandno < res->numbands; ++bandno) {
			auto band = res->bands + bandno;
			auto decodeBand = decodeRes.bands + bandno;
			if (!band->precincts)
				continue;
			for (size_t precno = 0; precno < band->numPrecincts(); ++precno) {
				auto prec = band->precincts + precno;
				auto decodePrec = decodeBand->precincts + precno;
				for (uint32_t cblkno = 0; cblkno < prec->cw * prec->ch; ++cblkno) {
					auto cblk = prec->cblks.enc + cblkno;
					auto decodeCblk = decodePrec->cblks.dec + cblkno;
					auto encodeRate = cblk->passes[cblk->num_passes_encoded - 1].rate;
					assert(decodeCblk->numSegments == 1);
					assert(decodeCblk->segs[0].len == encodeRate);
					assert(decodeCblk->numbps == cblk->numbps);
					assert(decodeCblk->numlenbits == cblk->numlenbits);
					grk_min_buf_t* seg = (grk_min_buf_t*)decodeCblk->seg_buffers.get(0);
					for (uint32_t i = 0; i < decodeCblk->segs[0].len; ++i) {
						assert(seg->buf[i] == cblk->data[i]);
					}
					auto diff = abs((int)decodeCblk->segs[0].numpasses - (int)cblk->num_passes_encoded);
					if (diff > 1) {
						if (resno == 1 &&
							band->bandno == 2 &&
							precno == 0 &&
							cblkno == 0) {

							int k = 0;
						}
						assert(decodeCblk->segs[0].numpasses < cblk->num_passes_encoded);
						assert(cblk->passes[decodeCblk->segs[0].numpasses-1].rate == encodeRate);
						printf("diff %d at resolution %d\n", diff, pi->resno);
					}
				}
			}
		}
	}
	else {
		assert(0);
	}
	
	for (uint32_t bandno = 0; bandno < res->numbands; ++bandno) {
		auto decodeBand = decodeRes.bands + bandno;
		for (size_t precno = 0; precno < decodeBand->numPrecincts(); ++precno) {
			auto decodePrec = decodeBand->precincts + precno;
			decodePrec->cleanupDecodeBlocks();
		}
	}

#endif
    return true;
}


static bool grk_t2_encode_packet_simulate(grk_tcd_tile_t * tile,
                                        grk_tcp_t * tcp,
                                        grk_pi_iterator_t *pi,
                                        uint64_t * p_data_written,
                                        uint64_t length)
{
    uint32_t bandno, cblkno;
    uint64_t l_nb_bytes;
    uint32_t compno = pi->compno;    
    uint32_t resno = pi->resno;     
    uint32_t precno = pi->precno;     
    uint32_t layno = pi->layno;     
    uint32_t l_nb_blocks;
    grk_tcd_band_t *band = nullptr;
    grk_tcd_cblk_enc_t* cblk = nullptr;
    grk_tcd_pass_t *pass = nullptr;

    grk_tcd_tilecomp_t *tilec = tile->comps + compno;
    grk_tcd_resolution_t *res = tilec->resolutions + resno;

    BitIO *bio = nullptr;    
    uint64_t packet_bytes_written = 0;

    /* <SOP 0xff91> */
    if (tcp->csty & J2K_CP_CSTY_SOP) {
        length -= 6;
        packet_bytes_written += 6;
    }
    /* </SOP> */

    if (!layno) {
        band = res->bands;

        for (bandno = 0; bandno < res->numbands; ++bandno) {
            grk_tcd_precinct_t *prc = band->precincts + precno;

			if (prc->incltree)
				prc->incltree->reset();
			if (prc->imsbtree)
				prc->imsbtree->reset();

            l_nb_blocks = prc->cw * prc->ch;
            for (cblkno = 0; cblkno < l_nb_blocks; ++cblkno) {
                cblk = prc->cblks.enc+cblkno;

                cblk->num_passes_included_in_current_layer = 0;
				prc->imsbtree->setvalue(cblkno, band->numbps - (int32_t)cblk->numbps);
            }
            ++band;
        }
    }

	bio = new BitIO();
    if (!bio) {
        /* FIXME event manager error callback */
        return false;
    }
    bio->init_enc(0, length);
	bio->simulateOutput(true);
	bio->write( 1, 1);           /* Empty header bit */

    /* Writing Packet header */
    band = res->bands;
    for (bandno = 0; bandno < res->numbands; ++bandno) {
        grk_tcd_precinct_t *prc = band->precincts + precno;

        l_nb_blocks = prc->cw * prc->ch;
        cblk = prc->cblks.enc;

        for (cblkno = 0; cblkno < l_nb_blocks; ++cblkno) {
            grk_tcd_layer_t *layer = cblk->layers + layno;

            if (!cblk->num_passes_included_in_current_layer && layer->numpasses) {
				prc->incltree->setvalue( cblkno, (int32_t)layno);
            }

            ++cblk;
        }

        cblk = prc->cblks.enc;
        for (cblkno = 0; cblkno < l_nb_blocks; cblkno++) {
            grk_tcd_layer_t *layer = cblk->layers + layno;
            uint32_t increment = 0;
            uint32_t nump = 0;
            uint32_t len = 0, passno;
            uint32_t l_nb_passes;

            /* cblk inclusion bits */
            if (!cblk->num_passes_included_in_current_layer) {
				prc->incltree->encode(bio, cblkno, (int32_t)(layno + 1));
            } else {
                bio->write( layer->numpasses != 0, 1);
            }

            /* if cblk not included, go to the next cblk  */
            if (!layer->numpasses) {
                ++cblk;
                continue;
            }

            /* if first instance of cblk --> zero bit-planes information */
            if (!cblk->num_passes_included_in_current_layer) {
                cblk->numlenbits = 3;
				prc->imsbtree->encode(bio, cblkno, tag_tree_uninitialized_node_value);
            }

            /* number of coding passes included */
            grk_t2_putnumpasses(bio, layer->numpasses);
            l_nb_passes = cblk->num_passes_included_in_current_layer + layer->numpasses;
            pass = cblk->passes + cblk->num_passes_included_in_current_layer;

            /* computation of the increase of the length indicator and insertion in the header     */
            for (passno = cblk->num_passes_included_in_current_layer; passno < l_nb_passes; ++passno) {
                ++nump;
                len += pass->len;

                if (pass->term || passno == (cblk->num_passes_included_in_current_layer + layer->numpasses) - 1) {
                    increment = (uint32_t)grk_max<int32_t>((int32_t)increment, grk_int_floorlog2((int32_t)len) + 1
                                                      - ((int32_t)cblk->numlenbits + grk_int_floorlog2((int32_t)nump)));
                    len = 0;
                    nump = 0;
                }

                ++pass;
            }
            grk_t2_putcommacode(bio, (int32_t)increment);

            /* computation of the new Length indicator */
            cblk->numlenbits += increment;

            pass = cblk->passes + cblk->num_passes_included_in_current_layer;
            /* insertion of the codeword segment length */
            for (passno = cblk->num_passes_included_in_current_layer; passno < l_nb_passes; ++passno) {
                nump++;
                len += pass->len;

                if (pass->term || passno == (cblk->num_passes_included_in_current_layer + layer->numpasses) - 1) {
                    bio->write( len, cblk->numlenbits + (uint32_t)grk_int_floorlog2((int32_t)nump));
                    len = 0;
                    nump = 0;
                }
                ++pass;
            }

            ++cblk;
        }

        ++band;
    }

    if (!bio->flush()) {
        delete bio;
        return false;
    }

    l_nb_bytes = (uint64_t)bio->numbytes();
    packet_bytes_written += l_nb_bytes;
    length -= l_nb_bytes;

    delete bio;

    /* <EPH 0xff92> */
    if (tcp->csty & J2K_CP_CSTY_EPH) {
        length -= 2;
        packet_bytes_written += 2;
    }
    /* </EPH> */


    /* Writing the packet body */
    band = res->bands;
    for (bandno = 0; bandno < res->numbands; bandno++) {
        grk_tcd_precinct_t *prc = band->precincts + precno;

        l_nb_blocks = prc->cw * prc->ch;
        cblk = prc->cblks.enc;

        for (cblkno = 0; cblkno < l_nb_blocks; ++cblkno) {
            grk_tcd_layer_t *layer = cblk->layers + layno;

            if (!layer->numpasses) {
                ++cblk;
                continue;
            }

            if (layer->len > length) {
                return false;
            }

            cblk->num_passes_included_in_current_layer += layer->numpasses;
            packet_bytes_written += layer->len;
            length -= layer->len;
            ++cblk;
        }
        ++band;
    }
    *p_data_written += packet_bytes_written;

    return true;
}
static bool grk_t2_skip_packet( grk_t2_t* p_t2,
                                grk_tcd_tile_t *p_tile,
                                grk_tcp_t *p_tcp,
                                grk_pi_iterator_t *p_pi,
                                opj_seg_buf_t* src_buf,
                                uint64_t * p_data_read,
                                grk_event_mgr_t *p_manager)
{
    bool l_read_data;
    uint64_t l_nb_bytes_read = 0;
    uint64_t l_nb_total_bytes_read = 0;
    uint64_t p_max_length = (uint64_t)opj_seg_buf_get_cur_seg_len(src_buf);

    *p_data_read = 0;

    if (! grk_t2_read_packet_header(p_t2,
									&p_tile->comps[p_pi->compno].resolutions[p_pi->resno],
									p_tcp,
									p_pi,
									&l_read_data,
                                    src_buf,
                                    &l_nb_bytes_read,
                                    p_manager)) {
        return false;
    }

    l_nb_total_bytes_read += l_nb_bytes_read;
    p_max_length -= l_nb_bytes_read;

    /* we should read data for the packet */
    if (l_read_data) {
        l_nb_bytes_read = 0;

        if (! grk_t2_skip_packet_data(&p_tile->comps[p_pi->compno].resolutions[p_pi->resno],
										p_pi,
										&l_nb_bytes_read,
										p_max_length,
										p_manager)) {
            return false;
        }
        opj_seg_buf_incr_cur_seg_offset(src_buf, l_nb_bytes_read);
        l_nb_total_bytes_read += l_nb_bytes_read;
    }
    *p_data_read = l_nb_total_bytes_read;


    return true;
}


static bool grk_t2_skip_packet_data(grk_tcd_resolution_t* l_res,
                                       grk_pi_iterator_t *p_pi,
                                       uint64_t * p_data_read,
                                       uint64_t p_max_length,
                                       grk_event_mgr_t *p_manager)
{
    uint32_t bandno, cblkno;
    uint32_t l_nb_code_blocks;
    grk_tcd_band_t *l_band = nullptr;
    grk_tcd_cblk_dec_t* l_cblk = nullptr;

    *p_data_read = 0;
    l_band = l_res->bands;

    for (bandno = 0; bandno < l_res->numbands; ++bandno) {
        grk_tcd_precinct_t *l_prc = &l_band->precincts[p_pi->precno];

        if ((l_band->x1-l_band->x0 == 0)||(l_band->y1-l_band->y0 == 0)) {
            ++l_band;
            continue;
        }

        l_nb_code_blocks = l_prc->cw * l_prc->ch;
        l_cblk = l_prc->cblks.dec;

        for (cblkno = 0; cblkno < l_nb_code_blocks; ++cblkno) {
            grk_tcd_seg_t *l_seg = nullptr;

            if (!l_cblk->numPassesInPacket) {
                /* nothing to do */
                ++l_cblk;
                continue;
            }

            if (!l_cblk->numSegments) {
                l_seg = l_cblk->segs;
                ++l_cblk->numSegments;
                l_cblk->dataSize = 0;
            } else {
                l_seg = &l_cblk->segs[l_cblk->numSegments - 1];

                if (l_seg->numpasses == l_seg->maxpasses) {
                    ++l_seg;
                    ++l_cblk->numSegments;
                }
            }

            do {
                /* Check possible overflow then size */
                if (((*p_data_read + l_seg->newlen) < (*p_data_read)) || ((*p_data_read + l_seg->newlen) > p_max_length)) {
                    grk_event_msg(p_manager, EVT_ERROR, "skip: segment too long (%d) with max (%d) for codeblock %d (p=%d, b=%d, r=%d, c=%d)\n",
                                  l_seg->newlen, p_max_length, cblkno, p_pi->precno, bandno, p_pi->resno, p_pi->compno);
                    return false;
                }

                JAS_FPRINTF(stderr, "p_data_read (%d) newlen (%d) \n", *p_data_read, l_seg->newlen );
                *(p_data_read) += l_seg->newlen;

                l_seg->numpasses += l_seg->numPassesInPacket;
                l_cblk->numPassesInPacket -= l_seg->numPassesInPacket;
                if (l_cblk->numPassesInPacket > 0) {
                    ++l_seg;
                    ++l_cblk->numSegments;
                }
            } while (l_cblk->numPassesInPacket > 0);

            ++l_cblk;
        }

        ++l_band;
    }

    return true;
}


static bool grk_t2_init_seg(   grk_tcd_cblk_dec_t* cblk,
                               uint32_t index,
                               uint32_t cblksty,
                               uint32_t first)
{
    grk_tcd_seg_t* seg = nullptr;
    uint32_t l_nb_segs = index + 1;

    if (l_nb_segs > cblk->numSegmentsAllocated) {
        grk_tcd_seg_t* new_segs;
        cblk->numSegmentsAllocated += OPJ_J2K_DEFAULT_NB_SEGS;

        new_segs = (grk_tcd_seg_t*) grk_realloc(cblk->segs, cblk->numSegmentsAllocated * sizeof(grk_tcd_seg_t));
        if(! new_segs) {
            grk_free(cblk->segs);
            cblk->segs = NULL;
            cblk->numSegmentsAllocated = 0;
            /* grk_event_msg(p_manager, EVT_ERROR, "Not enough memory to initialize segment %d\n", l_nb_segs); */
            return false;
        }
        cblk->segs = new_segs;
    }

    seg = &cblk->segs[index];
    memset(seg,0,sizeof(grk_tcd_seg_t));

    if (cblksty & J2K_CCP_CBLKSTY_TERMALL) {
        seg->maxpasses = 1;
    } else if (cblksty & J2K_CCP_CBLKSTY_LAZY) {
        if (first) {
            seg->maxpasses = 10;
        } else {
            seg->maxpasses = (((seg - 1)->maxpasses == 1) || ((seg - 1)->maxpasses == 10)) ? 2 : 1;
        }
    } else {
        seg->maxpasses = 109;
    }

    return true;
}
