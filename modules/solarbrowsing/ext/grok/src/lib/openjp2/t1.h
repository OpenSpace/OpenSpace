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

#include <vector>
#include "testing.h"


struct decodeBlockInfo {
	decodeBlockInfo() : tilec(NULL),
		tiledp(NULL),
		cblk(NULL),
		resno(0),
		bandno(0),
		stepsize(0),
		roishift(0),
		cblksty(0),
		qmfbid(0),
		x(0),
		y(0)
	{  }
	grk_tcd_tilecomp_t* tilec;
	int32_t* tiledp;
	grk_tcd_cblk_dec_t* cblk;
	uint32_t resno;
	uint32_t bandno;
	float stepsize;
	uint32_t roishift;
	uint32_t cblksty;
	uint32_t qmfbid;
	uint32_t x, y;		/* relative code block offset */
};



struct encodeBlockInfo {
	encodeBlockInfo() : tiledp(NULL),
						cblk(NULL),
						compno(0),
						resno(0),
						bandno(0),
						precno(0),
						cblkno(0),
						bandconst(0),
						stepsize(0),
						cblksty(0),
						qmfbid(0),
						x(0),
						y(0),
						mct_norms(NULL),
#ifdef DEBUG_LOSSLESS_T1
						unencodedData(nullptr),
#endif
						mct_numcomps(0)
	{  }
	int32_t* tiledp;
	grk_tcd_cblk_enc_t* cblk;
	uint32_t compno;
	uint32_t resno;
	uint32_t bandno;
	uint32_t precno;
	uint32_t cblkno;
	int32_t bandconst;
	float stepsize;
	uint32_t cblksty;
	uint32_t qmfbid;
	uint32_t x, y;		/* relative code block offset */
	const double * mct_norms;
	uint32_t mct_numcomps;
#ifdef DEBUG_LOSSLESS_T1
	int32_t* unencodedData;
#endif
};


/**
@file t1.h
@brief Implementation of the tier-1 coding (coding of code-block coefficients) (T1)

The functions in T1.C have for goal to realize the tier-1 coding operation. The functions
in T1.C are used by some function in TCD.C.
*/

/** @defgroup T1 T1 - Implementation of the tier-1 coding */
/*@{*/

/* ----------------------------------------------------------------------- */
#define T1_NMSEDEC_BITS 7

#define T1_SIG_NE 0x0001	/**< Context orientation : North-East direction */
#define T1_SIG_SE 0x0002	/**< Context orientation : South-East direction */
#define T1_SIG_SW 0x0004	/**< Context orientation : South-West direction */
#define T1_SIG_NW 0x0008	/**< Context orientation : North-West direction */
#define T1_SIG_N 0x0010		/**< Context orientation : North direction */
#define T1_SIG_E 0x0020		/**< Context orientation : East direction */
#define T1_SIG_S 0x0040		/**< Context orientation : South direction */
#define T1_SIG_W 0x0080		/**< Context orientation : West direction */
#define T1_SIG_OTH (T1_SIG_N|T1_SIG_NE|T1_SIG_E|T1_SIG_SE|T1_SIG_S|T1_SIG_SW|T1_SIG_W|T1_SIG_NW)
#define T1_SIG_PRIM (T1_SIG_N|T1_SIG_E|T1_SIG_S|T1_SIG_W)

#define T1_SGN_N 0x0100
#define T1_SGN_E 0x0200
#define T1_SGN_S 0x0400
#define T1_SGN_W 0x0800
#define T1_SGN (T1_SGN_N|T1_SGN_E|T1_SGN_S|T1_SGN_W)

#define T1_SIG 0x1000
#define T1_REFINE 0x2000
#define T1_VISIT 0x4000

#define T1_NUMCTXS_ZC 9
#define T1_NUMCTXS_SC 5
#define T1_NUMCTXS_MAG 3
#define T1_NUMCTXS_AGG 1
#define T1_NUMCTXS_UNI 1

#define T1_CTXNO_ZC 0
#define T1_CTXNO_SC (T1_CTXNO_ZC+T1_NUMCTXS_ZC)
#define T1_CTXNO_MAG (T1_CTXNO_SC+T1_NUMCTXS_SC)
#define T1_CTXNO_AGG (T1_CTXNO_MAG+T1_NUMCTXS_MAG)
#define T1_CTXNO_UNI (T1_CTXNO_AGG+T1_NUMCTXS_AGG)
#define T1_NUMCTXS (T1_CTXNO_UNI+T1_NUMCTXS_UNI)

#define T1_NMSEDEC_FRACBITS (T1_NMSEDEC_BITS-1)

#define T1_TYPE_MQ 0	/**< Normal coding using entropy coder */
#define T1_TYPE_RAW 1	/**< No encoding the information is store under raw format in codestream (mode switch RAW)*/

/* ----------------------------------------------------------------------- */




#define MACRO_t1_flags(x,y) t1->flags[((x)*(t1->flags_stride))+(y)]

/** @name Exported functions */
/*@{*/
/* ----------------------------------------------------------------------- */


typedef uint16_t opj_flag_t;

typedef struct grk_t1 {
	uint8_t* compressed_block;
	size_t compressed_block_size;
	/** MQC component */
	grk_mqc_t *mqc;
	/** RAW component */
	grk_raw_t *raw;

	int32_t  *data;
	opj_flag_t *flags;
	uint32_t w;
	uint32_t h;
	uint32_t datasize;
	uint32_t flagssize;
	uint32_t flags_stride;
	uint32_t data_stride;
	bool   encoder;
} grk_t1_t;

bool grk_t1_allocate_buffers(grk_t1_t *t1,
	uint32_t w,
	uint32_t h);


/**
Encode the code-blocks of a tile
@param t1 T1 handle
@param tile The tile to encode
@param tcp Tile coding parameters
@param mct_norms  FIXME DOC
@param mct_numcomps Number of components used for MCT
*/
bool grk_t1_encode_cblks(   grk_tcd_tile_t *tile,
                            grk_tcp_t *tcp,
                            const double * mct_norms,
                            uint32_t mct_numcomps,
							uint32_t numThreads);


double grk_t1_encode_cblk(grk_t1_t *t1,
						grk_tcd_cblk_enc_t* cblk,
						uint32_t orient,
						uint32_t compno,
						uint32_t level,
						uint32_t qmfbid,
						double stepsize,
						uint32_t cblksty,
						uint32_t numcomps,
						const double * mct_norms,
						uint32_t mct_numcomps);



/**
Decode the code-blocks of a tile
@param tilec The tile to decode
@param tccp Tile coding parameters
*/
bool grk_t1_prepare_decode_cblks(   grk_tcd_tilecomp_t* tilec,
                            opj_tccp_t* tccp,
							std::vector<decodeBlockInfo*>* blocks,
                            grk_event_mgr_t * p_manager);


/**
Decode 1 code-block
@param t1 T1 handle
@param cblk Code-block coding parameters
@param orient
@param roishift Region of interest shifting value
@param cblksty Code-block style
*/
bool grk_t1_decode_cblk(grk_t1_t *t1,
	grk_tcd_cblk_dec_t* cblk,
	uint32_t orient,
	uint32_t roishift,
	uint32_t cblksty);




double grk_t1_getwmsedec(
    int32_t nmsedec,
    uint32_t compno,
    uint32_t level,
    uint32_t orient,
    int32_t bpno,
    uint32_t qmfbid,
    double stepsize,
    uint32_t numcomps,
    const double * mct_norms,
    uint32_t mct_numcomps);


int16_t grk_t1_getnmsedec_sig(uint32_t x, uint32_t bitpos);
int16_t grk_t1_getnmsedec_ref(uint32_t x, uint32_t bitpos);

/**
* Creates a new Tier 1 handle
* and initializes the look-up tables of the Tier-1 coder/decoder
* @return a new T1 handle if successful, returns NULL otherwise
*/
grk_t1_t* grk_t1_create(bool isEncoder, uint16_t code_block_width, uint16_t code_block_height);

/**
* Destroys a previously created T1 handle
*
* @param p_t1 Tier 1 handle to destroy
*/
void grk_t1_destroy(grk_t1_t *p_t1);



/* ----------------------------------------------------------------------- */
/*@}*/

/*@}*/



