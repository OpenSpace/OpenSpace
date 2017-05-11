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
 * Copyright (c) 2007, Jonathan Ballard <dzonatas@dzonux.net>
 * Copyright (c) 2007, Callum Lerwick <seg@haxxed.com>
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

#ifdef __SSE__
#include <xmmintrin.h>
#endif

#include "grk_includes.h"
#include "Barrier.h"
#include "T1Decoder.h"
#include <atomic>
#include "testing.h"




/** @defgroup DWT DWT - Implementation of a discrete wavelet transform */
/*@{*/

/** @name Local data structures */
/*@{*/

typedef struct dwt_local {
    int32_t* mem;
    int32_t dn;
    int32_t sn;
    int32_t cas;
} grk_dwt_t;

typedef union {
    float	f[4];
} opj_v4_t;

typedef struct v4dwt_local {
    opj_v4_t*	wavelet ;
    int32_t		dn ;
    int32_t		sn ;
    int32_t		cas ;
} grk_v4dwt_t ;

static const float grk_dwt_alpha =  1.586134342f; /*  12994 */
static const float grk_dwt_beta  =  0.052980118f; /*    434 */
static const float grk_dwt_gamma = -0.882911075f; /*  -7233 */
static const float grk_dwt_delta = -0.443506852f; /*  -3633 */

static const float opj_K      = 1.230174105f; /*  10078 */
static const float opj_c13318 = 1.625732422f;

/*@}*/

/** @name Local static functions */
/*@{*/

/**
Forward lazy transform (horizontal)
*/
static void grk_dwt_deinterleave_h(int32_t *a, int32_t *b, int32_t dn, int32_t sn, int32_t cas);
/**
Forward lazy transform (vertical)
*/
static void grk_dwt_deinterleave_v(int32_t *a, int32_t *b, int32_t dn, int32_t sn, int32_t x, int32_t cas);
/**
Inverse lazy transform (horizontal)
*/
static void grk_dwt_interleave_h(grk_dwt_t* h, int32_t *a);
/**
Inverse lazy transform (vertical)
*/
static void grk_dwt_interleave_v(grk_dwt_t* v, int32_t *a, int32_t x);
/**
Forward 5-3 wavelet transform in 1-D
*/
static void grk_dwt_encode_line_53(int32_t *a, int32_t dn, int32_t sn, int32_t cas);
/**
Inverse 5-3 wavelet transform in 1-D
*/
static void grk_dwt_decode_line_53(grk_dwt_t *v);
/**
Forward 9-7 wavelet transform in 1-D
*/
static void grk_dwt_encode_line_97(int32_t *a, int32_t dn, int32_t sn, int32_t cas);
/**
Explicit calculation of the Quantization Stepsizes
*/
static void grk_dwt_encode_stepsize(int32_t stepsize, int32_t numbps, opj_stepsize_t *bandno_stepsize);

/* <summary>                             */
/* Inverse 9-7 wavelet transform in 1-D. */
/* </summary>                            */
static void grk_v4dwt_decode(grk_v4dwt_t* restrict dwt);

static void grk_v4dwt_interleave_h(grk_v4dwt_t* restrict w, float* restrict a, int32_t x, int32_t size);

static void grk_v4dwt_interleave_v(grk_v4dwt_t* restrict v , float* restrict a , int32_t x, int32_t nb_elts_read);

#ifdef __SSE__
static void grk_v4dwt_decode_step1_sse(opj_v4_t* w, int32_t count, const __m128 c);

static void grk_v4dwt_decode_step2_sse(opj_v4_t* l, opj_v4_t* w, int32_t k, int32_t m, __m128 c);

#else
static void grk_v4dwt_decode_step1(opj_v4_t* w, int32_t count, const float c);

static void grk_v4dwt_decode_step2(opj_v4_t* l, opj_v4_t* w, int32_t k, int32_t m, float c);

#endif

/*@}*/

/*@}*/

#define OPJ_S(i) a[(i)*2]
#define OPJ_D(i) a[(1+(i)*2)]
#define OPJ_S_(i) ((i)<0?OPJ_S(0):((i)>=sn?OPJ_S(sn-1):OPJ_S(i)))
#define OPJ_D_(i) ((i)<0?OPJ_D(0):((i)>=dn?OPJ_D(dn-1):OPJ_D(i)))
/* new */
#define OPJ_SS_(i) ((i)<0?OPJ_S(0):((i)>=dn?OPJ_S(dn-1):OPJ_S(i)))
#define OPJ_DD_(i) ((i)<0?OPJ_D(0):((i)>=sn?OPJ_D(sn-1):OPJ_D(i)))

/* <summary>                                                              */
/* This table contains the norms of the 5-3 wavelets for different bands. */
/* </summary>                                                             */
static const double grk_dwt_norms[4][10] = {
    {1.000, 1.500, 2.750, 5.375, 10.68, 21.34, 42.67, 85.33, 170.7, 341.3},
    {1.038, 1.592, 2.919, 5.703, 11.33, 22.64, 45.25, 90.48, 180.9},
    {1.038, 1.592, 2.919, 5.703, 11.33, 22.64, 45.25, 90.48, 180.9},
    {.7186, .9218, 1.586, 3.043, 6.019, 12.01, 24.00, 47.97, 95.93}
};

/* <summary>                                                              */
/* This table contains the norms of the 9-7 wavelets for different bands. */
/* </summary>                                                             */
static const double grk_dwt_norms_real[4][10] = {
    {1.000, 1.965, 4.177, 8.403, 16.90, 33.84, 67.69, 135.3, 270.6, 540.9},
    {2.022, 3.989, 8.355, 17.04, 34.27, 68.63, 137.3, 274.6, 549.0},
    {2.022, 3.989, 8.355, 17.04, 34.27, 68.63, 137.3, 274.6, 549.0},
    {2.080, 3.865, 8.307, 17.18, 34.71, 69.59, 139.3, 278.6, 557.2}
};

/*
==========================================================
   local functions
==========================================================
*/

/* <summary>			                 */
/* Forward lazy transform (horizontal).  */
/* </summary>                            */
static void grk_dwt_deinterleave_h(int32_t *a, int32_t *b, int32_t dn, int32_t sn, int32_t cas)
{
    int32_t i;
    int32_t * l_dest = b;
    int32_t * l_src = a+cas;

    for (i=0; i<sn; ++i) {
        *l_dest++ = *l_src;
        l_src += 2;
    }

    l_dest = b + sn;
    l_src = a + 1 - cas;

    for	(i=0; i<dn; ++i)  {
        *l_dest++=*l_src;
        l_src += 2;
    }
}

/* <summary>                             */
/* Forward lazy transform (vertical).    */
/* </summary>                            */
static void grk_dwt_deinterleave_v(int32_t *a, int32_t *b, int32_t dn, int32_t sn, int32_t x, int32_t cas)
{
    int32_t i = sn;
    int32_t * l_dest = b;
    int32_t * l_src = a+cas;

    while (i--) {
        *l_dest = *l_src;
        l_dest += x;
        l_src += 2;
    } /* b[i*x]=a[2*i+cas]; */

    l_dest = b + sn * x;
    l_src = a + 1 - cas;

    i = dn;
    while (i--) {
        *l_dest = *l_src;
        l_dest += x;
        l_src += 2;
    } /*b[(sn+i)*x]=a[(2*i+1-cas)];*/
}

/* <summary>                             */
/* Inverse lazy transform (horizontal).  */
/* </summary>                            */
static void grk_dwt_interleave_h(grk_dwt_t* h, int32_t *a)
{
    int32_t *ai = a;
    int32_t *bi = h->mem + h->cas;
    int32_t  i	= h->sn;
    while( i-- ) {
        *bi = *(ai++);
        bi += 2;
    }
    ai	= a + h->sn;
    bi	= h->mem + 1 - h->cas;
    i	= h->dn ;
    while( i-- ) {
        *bi = *(ai++);
        bi += 2;
    }
}

/* <summary>                             */
/* Inverse lazy transform (vertical).    */
/* </summary>                            */
static void grk_dwt_interleave_v(grk_dwt_t* v, int32_t *a, int32_t x)
{
    int32_t *ai = a;
    int32_t *bi = v->mem + v->cas;
    int32_t  i = v->sn;
    while( i-- ) {
        *bi = *ai;
        bi += 2;
        ai += x;
    }
    ai = a + (v->sn * x);
    bi = v->mem + 1 - v->cas;
    i = v->dn ;
    while( i-- ) {
        *bi = *ai;
        bi += 2;
        ai += x;
    }
}


/* <summary>                            */
/* Forward 5-3 wavelet transform in 1-D. */
/* </summary>                           */
static void grk_dwt_encode_line_53(int32_t *a, int32_t dn, int32_t sn, int32_t cas)
{
    int32_t i;

    if (!cas) {
        if ((dn > 0) || (sn > 1)) {	/* NEW :  CASE ONE ELEMENT */
            for (i = 0; i < dn; i++) OPJ_D(i) -= (OPJ_S_(i) + OPJ_S_(i + 1)) >> 1;
            for (i = 0; i < sn; i++) OPJ_S(i) += (OPJ_D_(i - 1) + OPJ_D_(i) + 2) >> 2;
        }
    } else {
        if (!sn && dn == 1)		    /* NEW :  CASE ONE ELEMENT */
            OPJ_S(0) *= 2;
        else {
            for (i = 0; i < dn; i++) OPJ_S(i) -= (OPJ_DD_(i) + OPJ_DD_(i - 1)) >> 1;
            for (i = 0; i < sn; i++) OPJ_D(i) += (OPJ_SS_(i) + OPJ_SS_(i + 1) + 2) >> 2;
        }
    }
}


/* <summary>                            */
/* Inverse 5-3 wavelet transform in 1-D. */
/* </summary>                           */
static void grk_dwt_decode_line_53(grk_dwt_t *v)
{
	int32_t *a = v->mem;
	int32_t dn = v->dn;
	int32_t sn = v->sn;
	int32_t cas = v->cas;
	int32_t i;

	if (!cas) {
		if ((dn > 0) || (sn > 1)) { /* NEW :  CASE ONE ELEMENT */
			for (i = 0; i < sn; i++) OPJ_S(i) -= (OPJ_D_(i - 1) + OPJ_D_(i) + 2) >> 2;
			for (i = 0; i < dn; i++) OPJ_D(i) += (OPJ_S_(i) + OPJ_S_(i + 1)) >> 1;
		}
	}
	else {
		if (!sn  && dn == 1)          /* NEW :  CASE ONE ELEMENT */
			OPJ_S(0) /= 2;
		else {
			for (i = 0; i < sn; i++) OPJ_D(i) -= (OPJ_SS_(i) + OPJ_SS_(i + 1) + 2) >> 2;
			for (i = 0; i < dn; i++) OPJ_S(i) += (OPJ_DD_(i) + OPJ_DD_(i - 1)) >> 1;
		}
	}

}

/* <summary>                             */
/* Forward 9-7 wavelet transform in 1-D. */
/* </summary>                            */
static void grk_dwt_encode_line_97(int32_t *a, int32_t dn, int32_t sn, int32_t cas)
{
    int32_t i;
    if (!cas) {
        if ((dn > 0) || (sn > 1)) {	/* NEW :  CASE ONE ELEMENT */
            for (i = 0; i < dn; i++)
                OPJ_D(i) -= grk_int_fix_mul(OPJ_S_(i) + OPJ_S_(i + 1), 12993);
            for (i = 0; i < sn; i++)
                OPJ_S(i) -= grk_int_fix_mul(OPJ_D_(i - 1) + OPJ_D_(i), 434);
            for (i = 0; i < dn; i++)
                OPJ_D(i) += grk_int_fix_mul(OPJ_S_(i) + OPJ_S_(i + 1), 7233);
            for (i = 0; i < sn; i++)
                OPJ_S(i) += grk_int_fix_mul(OPJ_D_(i - 1) + OPJ_D_(i), 3633);
            for (i = 0; i < dn; i++)
                OPJ_D(i) = grk_int_fix_mul(OPJ_D(i), 5038);	/*5038 */
            for (i = 0; i < sn; i++)
                OPJ_S(i) = grk_int_fix_mul(OPJ_S(i), 6659);	/*6660 */
        }
    } else {
        if ((sn > 0) || (dn > 1)) {	/* NEW :  CASE ONE ELEMENT */
            for (i = 0; i < dn; i++)
                OPJ_S(i) -= grk_int_fix_mul(OPJ_DD_(i) + OPJ_DD_(i - 1), 12993);
            for (i = 0; i < sn; i++)
                OPJ_D(i) -= grk_int_fix_mul(OPJ_SS_(i) + OPJ_SS_(i + 1), 434);
            for (i = 0; i < dn; i++)
                OPJ_S(i) += grk_int_fix_mul(OPJ_DD_(i) + OPJ_DD_(i - 1), 7233);
            for (i = 0; i < sn; i++)
                OPJ_D(i) += grk_int_fix_mul(OPJ_SS_(i) + OPJ_SS_(i + 1), 3633);
            for (i = 0; i < dn; i++)
                OPJ_S(i) = grk_int_fix_mul(OPJ_S(i), 5038);	/*5038 */
            for (i = 0; i < sn; i++)
                OPJ_D(i) = grk_int_fix_mul(OPJ_D(i), 6659);	/*6660 */
        }
    }
}

static void grk_dwt_encode_stepsize(int32_t stepsize, int32_t numbps, opj_stepsize_t *bandno_stepsize)
{
    int32_t p, n;
    p = grk_int_floorlog2(stepsize) - 13;
    n = 11 - grk_int_floorlog2(stepsize);
    bandno_stepsize->mant = (n < 0 ? stepsize >> -n : stepsize << n) & 0x7ff;
    bandno_stepsize->expn = numbps - p;
}

/*
==========================================================
   DWT interface
==========================================================
*/


/* Forward 5-3 wavelet transform in 2-D. */
/* </summary>                           */
bool grk_dwt_encode_53(grk_tcd_tilecomp_t * tilec)
{
	int32_t k;
	int32_t *a = nullptr;
	int32_t *aj = nullptr;
	int32_t *bj = nullptr;
	int32_t w;

	int32_t rw;			/* width of the resolution level computed   */
	int32_t rh;			/* height of the resolution level computed  */
	size_t l_data_size;

	grk_tcd_resolution_t * l_cur_res = 0;
	grk_tcd_resolution_t * l_last_res = 0;

	w = tilec->x1 - tilec->x0;
	int32_t num_decomps = (int32_t)tilec->numresolutions - 1;
	a = grk_tile_buf_get_ptr(tilec->buf, 0, 0, 0, 0);

	l_cur_res = tilec->resolutions + num_decomps;
	l_last_res = l_cur_res - 1;



#ifdef DEBUG_LOSSLESS_DWT
	int32_t rw_full = l_cur_res->x1 - l_cur_res->x0;
	int32_t rh_full = l_cur_res->y1 - l_cur_res->y0;
	int32_t* before = new int32_t[rw_full * rh_full];
	memcpy(before, a, rw_full * rh_full * sizeof(int32_t));
	int32_t* after = new int32_t[rw_full * rh_full];

#endif

	l_data_size = grk_dwt_max_resolution(tilec->resolutions, tilec->numresolutions) * sizeof(int32_t);
	/* overflow check */
	if (l_data_size > SIZE_MAX) {
		/* FIXME event manager error callback */
		return false;
	}
	bj = (int32_t*)grk_malloc(l_data_size);
	/* l_data_size is equal to 0 when numresolutions == 1 but bj is not used */
	/* in that case, so do not error out */
	if (l_data_size != 0 && !bj) {
		return false;
	}

	while (num_decomps--) {
		int32_t rw1;		/* width of the resolution level once lower than computed one                                       */
		int32_t rh1;		/* height of the resolution level once lower than computed one                                      */
		int32_t cas_col;	/* 0 = non inversion on horizontal filtering 1 = inversion between low-pass and high-pass filtering */
		int32_t cas_row;	/* 0 = non inversion on vertical filtering 1 = inversion between low-pass and high-pass filtering   */
		int32_t dn, sn;

		rw = l_cur_res->x1 - l_cur_res->x0;
		rh = l_cur_res->y1 - l_cur_res->y0;
		rw1 = l_last_res->x1 - l_last_res->x0;
		rh1 = l_last_res->y1 - l_last_res->y0;

		cas_row = l_cur_res->x0 & 1;
		cas_col = l_cur_res->y0 & 1;

		sn = rh1;
		dn = rh - rh1;

		for (int32_t j = 0; j < rw; ++j) {
			aj = a + j;
			for (k = 0; k < rh; ++k) {
				bj[k] = aj[k*w];
			}
			grk_dwt_encode_line_53(bj, dn, sn, cas_col);
			grk_dwt_deinterleave_v(bj, aj, dn, sn, w, cas_col);
		}
		sn = rw1;
		dn = rw - rw1;

		for (int32_t j = 0; j < rh; j++) {
			aj = a + j * w;
			for (k = 0; k < rw; k++)  bj[k] = aj[k];
			grk_dwt_encode_line_53(bj, dn, sn, cas_row);
			grk_dwt_deinterleave_h(bj, aj, dn, sn, cas_row);
		}
		l_cur_res = l_last_res;
		--l_last_res;
	}
	grk_free(bj);
#ifdef DEBUG_LOSSLESS_DWT
	memcpy(after, a, rw_full * rh_full * sizeof(int32_t));
	grk_dwt_decode_53(tilec, tilec->numresolutions, 8);
	for (int m = 0; m < rw_full; ++m) {
		for (int p = 0; p < rh_full; ++p) {
			auto expected = a[m + p * rw_full];
			auto actual = before[m + p * rw_full];
			if (expected != actual) {
				printf("(%d, %d); expected %d, got %d\n", m, p, expected, actual);
			}
		}
	}
	memcpy(a, after, rw_full * rh_full * sizeof(int32_t));
	delete[] before;
	delete[] after;
#endif
	return true;
}

/* <summary>                             */
/* Forward 9-7 wavelet transform in 2-D. */
/* </summary>                            */
bool grk_dwt_encode_97(grk_tcd_tilecomp_t * tilec)
{
	int32_t i, k;
	int32_t *a = nullptr;
	int32_t *aj = nullptr;
	int32_t *bj = nullptr;
	int32_t w, l;

	int32_t rw;			/* width of the resolution level computed   */
	int32_t rh;			/* height of the resolution level computed  */
	size_t l_data_size;

	grk_tcd_resolution_t * l_cur_res = 0;
	grk_tcd_resolution_t * l_last_res = 0;

	w = tilec->x1 - tilec->x0;
	l = (int32_t)tilec->numresolutions - 1;
	a = grk_tile_buf_get_ptr(tilec->buf, 0, 0, 0, 0);

	l_cur_res = tilec->resolutions + l;
	l_last_res = l_cur_res - 1;

	l_data_size = grk_dwt_max_resolution(tilec->resolutions, tilec->numresolutions) * sizeof(int32_t);
	/* overflow check */
	if (l_data_size > SIZE_MAX) {
		/* FIXME event manager error callback */
		return false;
	}
	bj = (int32_t*)grk_malloc(l_data_size);
	/* l_data_size is equal to 0 when numresolutions == 1 but bj is not used */
	/* in that case, so do not error out */
	if (l_data_size != 0 && !bj) {
		return false;
	}
	i = l;

	while (i--) {
		int32_t rw1;		/* width of the resolution level once lower than computed one                                       */
		int32_t rh1;		/* height of the resolution level once lower than computed one                                      */
		int32_t cas_col;	/* 0 = non inversion on horizontal filtering 1 = inversion between low-pass and high-pass filtering */
		int32_t cas_row;	/* 0 = non inversion on vertical filtering 1 = inversion between low-pass and high-pass filtering   */
		int32_t dn, sn;

		rw = l_cur_res->x1 - l_cur_res->x0;
		rh = l_cur_res->y1 - l_cur_res->y0;
		rw1 = l_last_res->x1 - l_last_res->x0;
		rh1 = l_last_res->y1 - l_last_res->y0;

		cas_row = l_cur_res->x0 & 1;
		cas_col = l_cur_res->y0 & 1;

		sn = rh1;
		dn = rh - rh1;
		for (int32_t j = 0; j < rw; ++j) {
			aj = a + j;
			for (k = 0; k < rh; ++k) {
				bj[k] = aj[k*w];
			}
			grk_dwt_encode_line_97(bj, dn, sn, cas_col);
			grk_dwt_deinterleave_v(bj, aj, dn, sn, w, cas_col);
		}
		sn = rw1;
		dn = rw - rw1;

		for (int32_t j = 0; j < rh; j++) {
			aj = a + j * w;
			for (k = 0; k < rw; k++)  bj[k] = aj[k];
			grk_dwt_encode_line_97(bj, dn, sn, cas_row);
			grk_dwt_deinterleave_h(bj, aj, dn, sn, cas_row);
		}
		l_cur_res = l_last_res;
		--l_last_res;
	}

	grk_free(bj);
	return true;
}

/* <summary>                            */
/* Inverse 5-3 wavelet transform in 2-D. */
/* </summary>                           */
bool grk_dwt_decode_53(grk_tcd_tilecomp_t* tilec, 
					uint32_t numres,
					uint32_t numThreads)
{
	if (numres == 1U) {
		return true;
	}
    if (grk_tile_buf_is_decode_region(tilec->buf))
        return grk_dwt_region_decode53(tilec, numres, numThreads);

	std::vector<std::thread> dwtWorkers;
	int rc = 0;
	auto tileBuf = (int32_t*)grk_tile_buf_get_ptr(tilec->buf, 0, 0, 0, 0);
	Barrier decode_dwt_barrier(numThreads);
	Barrier decode_dwt_calling_barrier(numThreads + 1);

	for (auto threadId = 0U; threadId < numThreads; threadId++) {
		dwtWorkers.push_back(std::thread([tilec,
			numres,
			&rc,
			tileBuf,
			&decode_dwt_barrier,
			&decode_dwt_calling_barrier,
			threadId,
			numThreads]()
		{
			auto numResolutions = numres;
			grk_dwt_t h;
			grk_dwt_t v;

			grk_tcd_resolution_t* tr = tilec->resolutions;

			uint32_t rw = (tr->x1 - tr->x0);	/* width of the resolution level computed */
			uint32_t rh = (tr->y1 - tr->y0);	/* height of the resolution level computed */

			uint32_t w = (tilec->x1 - tilec->x0);
			h.mem = (int32_t*)grk_aligned_malloc(grk_dwt_max_resolution(tr, numResolutions) * sizeof(int32_t));
			if (!h.mem) {
				rc++;
				goto cleanup;
			}

			v.mem = h.mem;

			while (--numResolutions) {
				int32_t * restrict tiledp = tileBuf;

				++tr;
				h.sn = (int32_t)rw;
				v.sn = (int32_t)rh;

				rw = (tr->x1 - tr->x0);
				rh = (tr->y1 - tr->y0);

				h.dn = (int32_t)(rw - (uint32_t)h.sn);
				h.cas = tr->x0 % 2;

				for (uint32_t j = threadId; j < rh; j += numThreads) {
					grk_dwt_interleave_h(&h, &tiledp[j*w]);
					grk_dwt_decode_line_53(&h);
					memcpy(&tiledp[j*w], h.mem, rw * sizeof(int32_t));
				}

				v.dn = (int32_t)(rh - (uint32_t)v.sn);
				v.cas = tr->y0 % 2;

				decode_dwt_barrier.arrive_and_wait();

				for (uint32_t j = threadId; j < rw; j += numThreads) {
					grk_dwt_interleave_v(&v, &tiledp[j], (int32_t)w);
					grk_dwt_decode_line_53(&v);
					for (uint32_t k = 0; k < rh; ++k) {
						tiledp[k * w + j] = v.mem[k];
					}
				}
				decode_dwt_barrier.arrive_and_wait();
			}
		cleanup:
			if (h.mem)
				grk_aligned_free(h.mem);
			decode_dwt_calling_barrier.arrive_and_wait();

		}));
	}
	decode_dwt_calling_barrier.arrive_and_wait();

	for (auto& t : dwtWorkers) {
		t.join();
	}
	return rc == 0 ? true : false;

}


/* <summary>                          */
/* Get gain of 5-3 wavelet transform. */
/* </summary>                         */
uint32_t grk_dwt_getgain(uint32_t orient)
{
    if (orient == 0)
        return 0;
    if (orient == 1 || orient == 2)
        return 1;
    return 2;
}

/* <summary>                */
/* Get norm of 5-3 wavelet. */
/* </summary>               */
double grk_dwt_getnorm(uint32_t level, uint32_t orient)
{
    return grk_dwt_norms[orient][level];
}


/* <summary>                          */
/* Get gain of 9-7 wavelet transform. */
/* </summary>                         */
uint32_t grk_dwt_getgain_real(uint32_t orient)
{
    (void)orient;
    return 0;
}

/* <summary>                */
/* Get norm of 9-7 wavelet. */
/* </summary>               */
double grk_dwt_getnorm_real(uint32_t level, uint32_t orient)
{
    return grk_dwt_norms_real[orient][level];
}

void grk_dwt_calc_explicit_stepsizes(opj_tccp_t * tccp, uint32_t prec)
{
    uint32_t numbands, bandno;
    numbands = 3 * tccp->numresolutions - 2;
    for (bandno = 0; bandno < numbands; bandno++) {
        double stepsize;
        uint32_t resno, level, orient, gain;

        resno = (bandno == 0) ? 0 : ((bandno - 1) / 3 + 1);
        orient = (bandno == 0) ? 0 : ((bandno - 1) % 3 + 1);
        level = tccp->numresolutions - 1 - resno;
        gain = (tccp->qmfbid == 0) ? 0 : ((orient == 0) ? 0 : (((orient == 1) || (orient == 2)) ? 1 : 2));
        if (tccp->qntsty == J2K_CCP_QNTSTY_NOQNT) {
            stepsize = 1.0;
        } else {
            double norm = grk_dwt_norms_real[orient][level];
            stepsize = ((uint64_t)1 << (gain)) / norm;
        }
        grk_dwt_encode_stepsize((int32_t) floor(stepsize * 8192.0), (int32_t)(prec + gain), &tccp->stepsizes[bandno]);
    }
}

/* <summary>                             */
/* Determine maximum computed resolution level for inverse wavelet transform */
/* </summary>                            */
uint32_t grk_dwt_max_resolution(grk_tcd_resolution_t* restrict r, uint32_t i)
{
    uint32_t mr	= 0;
    uint32_t w;
    while( --i ) {
        ++r;
        if( mr < ( w = r->x1 - r->x0 ) )
            mr = w ;
        if( mr < ( w = r->y1 - r->y0 ) )
            mr = w ;
    }
    return mr ;
}

static void grk_v4dwt_interleave_h(grk_v4dwt_t* restrict w, float* restrict a, int32_t x, int32_t size)
{
    float* restrict bi = (float*) (w->wavelet + w->cas);
    int32_t count = w->sn;
    int32_t i, k;

    for(k = 0; k < 2; ++k) {
        if ( count + 3 * x < size ) {
            /* Fast code path */
            for(i = 0; i < count; ++i) {
                int32_t j = i;
                bi[i*8    ] = a[j];
                j += x;
                bi[i*8 + 1] = a[j];
                j += x;
                bi[i*8 + 2] = a[j];
                j += x;
                bi[i*8 + 3] = a[j];
            }
        } else {
            /* Slow code path */
            for(i = 0; i < count; ++i) {
                int32_t j = i;
                bi[i*8    ] = a[j];
                j += x;
                if(j >= size) continue;
                bi[i*8 + 1] = a[j];
                j += x;
                if(j >= size) continue;
                bi[i*8 + 2] = a[j];
                j += x;
                if(j >= size) continue;
                bi[i*8 + 3] = a[j]; /* This one*/
            }
        }

        bi = (float*) (w->wavelet + 1 - w->cas);
        a += w->sn;
        size -= w->sn;
        count = w->dn;
    }
}

static void grk_v4dwt_interleave_v(grk_v4dwt_t* restrict v , float* restrict a , int32_t x, int32_t nb_elts_read)
{
    opj_v4_t* restrict bi = v->wavelet + v->cas;
    int32_t i;

    for(i = 0; i < v->sn; ++i) {
        memcpy(&bi[i*2], &a[i*x], (size_t)nb_elts_read * sizeof(float));
    }

    a += v->sn * x;
    bi = v->wavelet + 1 - v->cas;

    for(i = 0; i < v->dn; ++i) {
        memcpy(&bi[i*2], &a[i*x], (size_t)nb_elts_read * sizeof(float));
    }
}

#ifdef __SSE__

static void grk_v4dwt_decode_step1_sse(opj_v4_t* w, int32_t count, const __m128 c)
{
    __m128* restrict vw = (__m128*) w;
    int32_t i;
    /* 4x unrolled loop */
    for(i = 0; i < count >> 2; ++i) {
        *vw = _mm_mul_ps(*vw, c);
        vw += 2;
        *vw = _mm_mul_ps(*vw, c);
        vw += 2;
        *vw = _mm_mul_ps(*vw, c);
        vw += 2;
        *vw = _mm_mul_ps(*vw, c);
        vw += 2;
    }
    count &= 3;
    for(i = 0; i < count; ++i) {
        *vw = _mm_mul_ps(*vw, c);
        vw += 2;
    }
}

void grk_v4dwt_decode_step2_sse(opj_v4_t* l, opj_v4_t* w, int32_t k, int32_t m, __m128 c)
{
    __m128* restrict vl = (__m128*) l;
    __m128* restrict vw = (__m128*) w;
    int32_t i;
    __m128 tmp1, tmp2, tmp3;
    tmp1 = vl[0];
    for(i = 0; i < m; ++i) {
        tmp2 = vw[-1];
        tmp3 = vw[ 0];
        vw[-1] = _mm_add_ps(tmp2, _mm_mul_ps(_mm_add_ps(tmp1, tmp3), c));
        tmp1 = tmp3;
        vw += 2;
    }
    vl = vw - 2;
    if(m >= k) {
        return;
    }
    c = _mm_add_ps(c, c);
    c = _mm_mul_ps(c, vl[0]);
    for(; m < k; ++m) {
        __m128 tmp = vw[-1];
        vw[-1] = _mm_add_ps(tmp, c);
        vw += 2;
    }
}

#else

static void grk_v4dwt_decode_step1(opj_v4_t* w, int32_t count, const float c)
{
    float* restrict fw = (float*) w;
    int32_t i;
    for(i = 0; i < count; ++i) {
        float tmp1 = fw[i*8    ];
        float tmp2 = fw[i*8 + 1];
        float tmp3 = fw[i*8 + 2];
        float tmp4 = fw[i*8 + 3];
        fw[i*8    ] = tmp1 * c;
        fw[i*8 + 1] = tmp2 * c;
        fw[i*8 + 2] = tmp3 * c;
        fw[i*8 + 3] = tmp4 * c;
    }
}

static void grk_v4dwt_decode_step2(opj_v4_t* l, opj_v4_t* w, int32_t k, int32_t m, float c)
{
    float* fl = (float*) l;
    float* fw = (float*) w;
    int32_t i;
    for(i = 0; i < m; ++i) {
        float tmp1_1 = fl[0];
        float tmp1_2 = fl[1];
        float tmp1_3 = fl[2];
        float tmp1_4 = fl[3];
        float tmp2_1 = fw[-4];
        float tmp2_2 = fw[-3];
        float tmp2_3 = fw[-2];
        float tmp2_4 = fw[-1];
        float tmp3_1 = fw[0];
        float tmp3_2 = fw[1];
        float tmp3_3 = fw[2];
        float tmp3_4 = fw[3];
        fw[-4] = tmp2_1 + ((tmp1_1 + tmp3_1) * c);
        fw[-3] = tmp2_2 + ((tmp1_2 + tmp3_2) * c);
        fw[-2] = tmp2_3 + ((tmp1_3 + tmp3_3) * c);
        fw[-1] = tmp2_4 + ((tmp1_4 + tmp3_4) * c);
        fl = fw;
        fw += 8;
    }
    if(m < k) {
        float c1;
        float c2;
        float c3;
        float c4;
        c += c;
        c1 = fl[0] * c;
        c2 = fl[1] * c;
        c3 = fl[2] * c;
        c4 = fl[3] * c;
        for(; m < k; ++m) {
            float tmp1 = fw[-4];
            float tmp2 = fw[-3];
            float tmp3 = fw[-2];
            float tmp4 = fw[-1];
            fw[-4] = tmp1 + c1;
            fw[-3] = tmp2 + c2;
            fw[-2] = tmp3 + c3;
            fw[-1] = tmp4 + c4;
            fw += 8;
        }
    }
}

#endif

/* <summary>                             */
/* Inverse 9-7 wavelet transform in 1-D. */
/* </summary>                            */
static void grk_v4dwt_decode(grk_v4dwt_t* restrict dwt)
{
    int32_t a, b;
    if(dwt->cas == 0) {
        if(!((dwt->dn > 0) || (dwt->sn > 1))) {
            return;
        }
        a = 0;
        b = 1;
    } else {
        if(!((dwt->sn > 0) || (dwt->dn > 1))) {
            return;
        }
        a = 1;
        b = 0;
    }
#ifdef __SSE__
    grk_v4dwt_decode_step1_sse(dwt->wavelet+a, dwt->sn, _mm_set1_ps(opj_K));
    grk_v4dwt_decode_step1_sse(dwt->wavelet+b, dwt->dn, _mm_set1_ps(opj_c13318));
    grk_v4dwt_decode_step2_sse(dwt->wavelet+b, dwt->wavelet+a+1, dwt->sn, grk_min<int32_t>(dwt->sn, dwt->dn-a), _mm_set1_ps(grk_dwt_delta));
    grk_v4dwt_decode_step2_sse(dwt->wavelet+a, dwt->wavelet+b+1, dwt->dn, grk_min<int32_t>(dwt->dn, dwt->sn-b), _mm_set1_ps(grk_dwt_gamma));
    grk_v4dwt_decode_step2_sse(dwt->wavelet+b, dwt->wavelet+a+1, dwt->sn, grk_min<int32_t>(dwt->sn, dwt->dn-a), _mm_set1_ps(grk_dwt_beta));
    grk_v4dwt_decode_step2_sse(dwt->wavelet+a, dwt->wavelet+b+1, dwt->dn, grk_min<int32_t>(dwt->dn, dwt->sn-b), _mm_set1_ps(grk_dwt_alpha));
#else
    grk_v4dwt_decode_step1(dwt->wavelet+a, dwt->sn, opj_K);
    grk_v4dwt_decode_step1(dwt->wavelet+b, dwt->dn, opj_c13318);
    grk_v4dwt_decode_step2(dwt->wavelet+b, dwt->wavelet+a+1, dwt->sn, grk_min<int32_t>(dwt->sn, dwt->dn-a), grk_dwt_delta);
    grk_v4dwt_decode_step2(dwt->wavelet+a, dwt->wavelet+b+1, dwt->dn, grk_min<int32_t>(dwt->dn, dwt->sn-b), grk_dwt_gamma);
    grk_v4dwt_decode_step2(dwt->wavelet+b, dwt->wavelet+a+1, dwt->sn, grk_min<int32_t>(dwt->sn, dwt->dn-a), grk_dwt_beta);
    grk_v4dwt_decode_step2(dwt->wavelet+a, dwt->wavelet+b+1, dwt->dn, grk_min<int32_t>(dwt->dn, dwt->sn-b), grk_dwt_alpha);
#endif
}


/* <summary>                             */
/* Inverse 9-7 wavelet transform in 2-D. */
/* </summary>                            */
bool grk_dwt_decode_97(grk_tcd_tilecomp_t* restrict tilec, 
						uint32_t numres,
						uint32_t numThreads)
{
	if (numres == 1U) {
		return true;
	}
	if (grk_tile_buf_is_decode_region(tilec->buf))
		return grk_dwt_region_decode97(tilec, numres, numThreads);

	int rc = 0;
	auto tileBuf = (float*)grk_tile_buf_get_ptr(tilec->buf, 0, 0, 0, 0);
	Barrier decode_dwt_barrier(numThreads);
	Barrier decode_dwt_calling_barrier(numThreads + 1);
	std::vector<std::thread> dwtWorkers;
	for (auto threadId = 0U; threadId < numThreads; threadId++) {
		dwtWorkers.push_back(std::thread([ tilec,
											numres,
											&rc,
											tileBuf,
											&decode_dwt_barrier,
											&decode_dwt_calling_barrier,
											threadId,
											numThreads]()
		{
			auto numResolutions = numres;
			grk_v4dwt_t h;
			grk_v4dwt_t v;

			grk_tcd_resolution_t* res = tilec->resolutions;

			uint32_t rw = (res->x1 - res->x0);	/* width of the resolution level computed */
			uint32_t rh = (res->y1 - res->y0);	/* height of the resolution level computed */

			uint32_t w = (tilec->x1 - tilec->x0);

			h.wavelet = (opj_v4_t*)grk_aligned_malloc((grk_dwt_max_resolution(res, numResolutions)) * sizeof(opj_v4_t));
			if (!h.wavelet) {
				/* FIXME event manager error callback */
				rc++;
				goto cleanup;
			}
			v.wavelet = h.wavelet;

			
			while (--numResolutions) {
				float * restrict aj = tileBuf + ((w << 2) * threadId);
				uint64_t bufsize = (uint64_t)(tilec->x1 - tilec->x0) * (tilec->y1 - tilec->y0) - (threadId * (w << 2));

				h.sn = (int32_t)rw;
				v.sn = (int32_t)rh;

				++res;

				rw = (res->x1 - res->x0);	// width of the resolution level computed 
				rh = (res->y1 - res->y0);	// height of the resolution level computed 

				h.dn = (int32_t)(rw - (uint32_t)h.sn);
				h.cas = res->x0 & 1;
				int32_t j;
				for (j = (int32_t)rh - (threadId<<2); j > 3; j -= (numThreads <<2)) {
					grk_v4dwt_interleave_h(&h, aj, (int32_t)w, (int32_t)bufsize);
					grk_v4dwt_decode(&h);
					
					for (int32_t k = (int32_t)rw; k-- > 0;) {
						aj[(uint32_t)k] = h.wavelet[k].f[0];
						aj[(uint32_t)k + w] = h.wavelet[k].f[1];
						aj[(uint32_t)k + (w << 1)] = h.wavelet[k].f[2];
						aj[(uint32_t)k + w * 3] = h.wavelet[k].f[3];
					}
					
					aj += (w << 2) * numThreads;
					bufsize -= (w << 2) * numThreads;
				}
				decode_dwt_barrier.arrive_and_wait();
				
				if (j > 0 ) {
					grk_v4dwt_interleave_h(&h, aj, (int32_t)w, (int32_t)bufsize);
					grk_v4dwt_decode(&h);
					for (int32_t k = (int32_t)rw; k-- > 0;) {
						switch (j) {
						case 3:
							aj[k + (int32_t)(w << 1)] = h.wavelet[k].f[2];
						case 2:
							aj[k + (int32_t)w] = h.wavelet[k].f[1];
						case 1:
							aj[k] = h.wavelet[k].f[0];
						}
					}
				}
				
				decode_dwt_barrier.arrive_and_wait();
				
				v.dn = (int32_t)(rh - (uint32_t)v.sn);
				v.cas = res->y0 & 1;
				
				decode_dwt_barrier.arrive_and_wait();
				
				aj = tileBuf + (threadId << 2);
				for (j = (int32_t)rw - (threadId<<2); j > 3; j -= (numThreads <<2)) {
					grk_v4dwt_interleave_v(&v, aj, (int32_t)w, 4);
					grk_v4dwt_decode(&v);

					for (uint32_t k = 0; k < rh; ++k) {
						memcpy(&aj[k*w], &v.wavelet[k], 4 * sizeof(float));
					}
					aj += (numThreads <<2);
				}
				
				if (j > 0) {
					grk_v4dwt_interleave_v(&v, aj, (int32_t)w, j);
					grk_v4dwt_decode(&v);

					for (uint32_t k = 0; k < rh; ++k) {
						memcpy(&aj[k*w], &v.wavelet[k], (size_t)j * sizeof(float));
					}
				}
				
				decode_dwt_barrier.arrive_and_wait();
			}

		cleanup:
			if (h.wavelet)
				grk_aligned_free(h.wavelet);
			decode_dwt_calling_barrier.arrive_and_wait();

		}));
	}

	decode_dwt_calling_barrier.arrive_and_wait();

	for (auto& t : dwtWorkers) {
		t.join();
	}
    return rc == 0 ? true : false;
}
