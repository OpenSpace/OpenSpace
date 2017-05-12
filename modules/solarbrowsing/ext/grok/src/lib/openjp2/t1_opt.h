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

/*********************/
/*   STATE FLAGS     */
/*********************/


/** We hold the state of individual data points for the T1 encoder using
*  a single 32-bit flags word to hold the state of 4 data points.  This corresponds
*  to the 4-point-high columns that the data is processed in.
*
*  These #defines declare the layout of a 32-bit flags word.
*
*  This is currently done for encoding only.
*/

/* T1_SIGMA_XXX is significance flag for stripe column and neighbouring locations: 18 locations in total */

#define T1_SIGMA_0  (1U << 0)
#define T1_SIGMA_1  (1U << 1)
#define T1_SIGMA_2  (1U << 2)
#define T1_SIGMA_3  (1U << 3)
#define T1_SIGMA_4  (1U << 4)
#define T1_SIGMA_5  (1U << 5)
#define T1_SIGMA_6  (1U << 6)
#define T1_SIGMA_7  (1U << 7)
#define T1_SIGMA_8  (1U << 8)
#define T1_SIGMA_9  (1U << 9)
#define T1_SIGMA_10 (1U << 10)
#define T1_SIGMA_11 (1U << 11)
#define T1_SIGMA_12 (1U << 12)
#define T1_SIGMA_13 (1U << 13)
#define T1_SIGMA_14 (1U << 14)
#define T1_SIGMA_15 (1U << 15)
#define T1_SIGMA_16 (1U << 16)
#define T1_SIGMA_17 (1U << 17)


/*
*
* T1_CHI_X is the sign flag for the (X+1)th location in the stripe column.
* T1_PI_X  indicates whether Xth location was coded in significance propagation pass
* T1_MU_X  indicates whether Xth location belongs to the magnitude refinement pass

*/

#define T1_CHI_0_I  18
#define T1_CHI_1_I  19
#define T1_CHI_0    (1U << T1_CHI_0_I)
#define T1_CHI_1    (1U << T1_CHI_1_I)
#define T1_MU_0     (1U << 20)
#define T1_PI_0     (1U << 21)
#define T1_CHI_2_I  22
#define T1_CHI_2    (1U << T1_CHI_2_I)
#define T1_MU_1     (1U << 23)
#define T1_PI_1     (1U << 24)
#define T1_CHI_3    (1U << 25)
#define T1_MU_2     (1U << 26)
#define T1_PI_2     (1U << 27)
#define T1_CHI_4    (1U << 28)
#define T1_MU_3     (1U << 29)
#define T1_PI_3     (1U << 30)
#define T1_CHI_5    (1U << 31)



/** As an example, the bits T1_SIGMA_3, T1_SIGMA_4 and T1_SIGMA_5
*  indicate the significance state of the west neighbour of data point zero
*  of our four, the point itself, and its east neighbour respectively.
*  Many of the bits are arranged so that given a flags word, you can
*  look at the values for the data point 0, then shift the flags
*  word right by 3 bits and look at the same bit positions to see the
*  values for data point 1.
*
*  The #defines below are convenience flags; say you have a flags word
*  f, you can do things like
*
*  (f & T1_SIGMA_THIS)
*
*  to see the significance bit of data point 0, then do
*
*  ((f >> 3) & T1_SIGMA_THIS)
*
*  to see the significance bit of data point 1.
*/

#define T1_SIGMA_NW   T1_SIGMA_0
#define T1_SIGMA_N    T1_SIGMA_1
#define T1_SIGMA_NE   T1_SIGMA_2
#define T1_SIGMA_W    T1_SIGMA_3
#define T1_SIGMA_THIS T1_SIGMA_4
#define T1_SIGMA_E    T1_SIGMA_5
#define T1_SIGMA_SW   T1_SIGMA_6
#define T1_SIGMA_S    T1_SIGMA_7
#define T1_SIGMA_SE   T1_SIGMA_8
#define T1_SIGMA_NEIGHBOURS (T1_SIGMA_NW | T1_SIGMA_N | T1_SIGMA_NE | T1_SIGMA_W | T1_SIGMA_E | T1_SIGMA_SW | T1_SIGMA_S | T1_SIGMA_SE)



#define T1_CHI_THIS   T1_CHI_1
#define T1_CHI_THIS_I T1_CHI_1_I
#define T1_MU_THIS    T1_MU_0
#define T1_PI_THIS    T1_PI_0

#define T1_LUT_SGN_W (1U << 0)
#define T1_LUT_SIG_N (1U << 1)
#define T1_LUT_SGN_E (1U << 2)
#define T1_LUT_SIG_W (1U << 3)
#define T1_LUT_SGN_N (1U << 4)
#define T1_LUT_SIG_E (1U << 5)
#define T1_LUT_SGN_S (1U << 6)
#define T1_LUT_SIG_S (1U << 7)

// sign bit is stored at this location in 32 bit coefficient
#define T1_DATA_SIGN_BIT_INDEX 31

typedef uint32_t opj_flag_opt_t;

/**
Tier-1 coding (coding of code-block coefficients)
*/
typedef struct grk_t1_opt {
	grk_mqc_t *mqc;
	uint32_t  *data;
	opj_flag_opt_t *flags;
	uint32_t w;
	uint32_t h;
	uint32_t flags_stride;
	bool   encoder;
} grk_t1_opt_t;

/** @name Exported functions */
/*@{*/
/* ----------------------------------------------------------------------- */

/**
* Creates a new Tier 1 handle
* and initializes the look-up tables of the Tier-1 coder/decoder
* @return a new T1 handle if successful, returns NULL otherwise
*/
grk_t1_opt_t* grk_t1_opt_create(bool isEncoder);

/**
* Destroys a previously created T1 handle
*
* @param p_t1 Tier 1 handle to destroy
*/
void grk_t1_opt_destroy(grk_t1_opt_t *p_t1);


bool grk_t1_opt_allocate_buffers(grk_t1_opt_t *t1,
	uint32_t cblkw,
	uint32_t cblkh);

void grk_t1_opt_init_buffers(grk_t1_opt_t *t1,
	uint32_t w,
	uint32_t h);


double grk_t1_opt_encode_cblk(grk_t1_opt_t *t1,
	grk_tcd_cblk_enc_t* cblk,
	uint32_t orient,
	uint32_t compno,
	uint32_t level,
	uint32_t qmfbid,
	double stepsize,
	uint32_t cblksty,
	uint32_t numcomps,
	const double * mct_norms,
	uint32_t mct_numcomps,
	uint32_t max);



/* ----------------------------------------------------------------------- */
/*@}*/

/*@}*/


