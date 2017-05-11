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

#ifdef __SSE__
#include <xmmintrin.h>
#endif
#ifdef __SSE2__
#include <emmintrin.h>
#endif
#ifdef __SSE4_1__
#include <smmintrin.h>
#endif

#include "grk_includes.h"

/* <summary> */
/* This table contains the norms of the basis function of the reversible MCT. */
/* </summary> */
static const double grk_mct_norms[3] = { 1.732, .8292, .8292 };

/* <summary> */
/* This table contains the norms of the basis function of the irreversible MCT. */
/* </summary> */
static const double grk_mct_norms_real[3] = { 1.732, 1.805, 1.573 };

const double * grk_mct_get_mct_norms ()
{
    return grk_mct_norms;
}

const double * grk_mct_get_mct_norms_real ()
{
    return grk_mct_norms_real;
}

/* <summary> */
/* Forward reversible MCT. */
/* </summary> */
#ifdef __SSE2__
void grk_mct_encode(
    int32_t* restrict c0,
    int32_t* restrict c1,
    int32_t* restrict c2,
    uint64_t n)
{
    size_t i;
    const size_t len = n;
    /* buffer are aligned on 16 bytes */
    assert( ((size_t)c0 & 0xf) == 0 );
    assert( ((size_t)c1 & 0xf) == 0 );
    assert( ((size_t)c2 & 0xf) == 0 );

    for(i = 0; i < (len & ~3U); i += 4) {
        __m128i y, u, v;
        __m128i r = _mm_load_si128((const __m128i *)&(c0[i]));
        __m128i g = _mm_load_si128((const __m128i *)&(c1[i]));
        __m128i b = _mm_load_si128((const __m128i *)&(c2[i]));
        y = _mm_add_epi32(g, g);
        y = _mm_add_epi32(y, b);
        y = _mm_add_epi32(y, r);
        y = _mm_srai_epi32(y, 2);
        u = _mm_sub_epi32(b, g);
        v = _mm_sub_epi32(r, g);
        _mm_store_si128((__m128i *)&(c0[i]), y);
        _mm_store_si128((__m128i *)&(c1[i]), u);
        _mm_store_si128((__m128i *)&(c2[i]), v);
    }

    for(; i < len; ++i) {
        int32_t r = c0[i];
        int32_t g = c1[i];
        int32_t b = c2[i];
        int32_t y = (r + (g * 2) + b) >> 2;
        int32_t u = b - g;
        int32_t v = r - g;
        c0[i] = y;
        c1[i] = u;
        c2[i] = v;
    }
}
#else
void grk_mct_encode(
    int32_t* restrict c0,
    int32_t* restrict c1,
    int32_t* restrict c2,
    uint64_t n)
{
    size_t i;
    const size_t len = n;

    for(i = 0; i < len; ++i) {
        int32_t r = c0[i];
        int32_t g = c1[i];
        int32_t b = c2[i];
        int32_t y = (r + (g * 2) + b) >> 2;
        int32_t u = b - g;
        int32_t v = r - g;
        c0[i] = y;
        c1[i] = u;
        c2[i] = v;
    }
}
#endif

/* <summary> */
/* Inverse reversible MCT. */
/* </summary> */
#ifdef __SSE2__
void grk_mct_decode(
    int32_t* restrict c0,
    int32_t* restrict c1,
    int32_t* restrict c2,
    uint64_t n)
{
    size_t i;
    const size_t len = n;

    for(i = 0; i < (len & ~3U); i += 4) {
        __m128i r, g, b;
        __m128i y = _mm_load_si128((const __m128i *)&(c0[i]));
        __m128i u = _mm_load_si128((const __m128i *)&(c1[i]));
        __m128i v = _mm_load_si128((const __m128i *)&(c2[i]));
        g = y;
        g = _mm_sub_epi32(g, _mm_srai_epi32(_mm_add_epi32(u, v), 2));
        r = _mm_add_epi32(v, g);
        b = _mm_add_epi32(u, g);
        _mm_store_si128((__m128i *)&(c0[i]), r);
        _mm_store_si128((__m128i *)&(c1[i]), g);
        _mm_store_si128((__m128i *)&(c2[i]), b);
    }
    for (; i < len; ++i) {
        int32_t y = c0[i];
        int32_t u = c1[i];
        int32_t v = c2[i];
        int32_t g = y - ((u + v) >> 2);
        int32_t r = v + g;
        int32_t b = u + g;
        c0[i] = r;
        c1[i] = g;
        c2[i] = b;
    }
}
#else
void grk_mct_decode(
    int32_t* restrict c0,
    int32_t* restrict c1,
    int32_t* restrict c2,
    uint64_t n)
{
    uint64_t i=0;
#ifdef _OPENMP
#pragma omp parallel default(none) private(i) shared(c0, c1,c2,n)
	{
#pragma omp for
#endif

		for (i = 0; i < n; ++i) {
			int32_t y = c0[i];
			int32_t u = c1[i];
			int32_t v = c2[i];
			int32_t g = y - ((u + v) >> 2);
			int32_t r = v + g;
			int32_t b = u + g;
			c0[i] = r;
			c1[i] = g;
			c2[i] = b;
		}
#ifdef _OPENMP
	}
#endif

}
#endif

/* <summary> */
/* Get norm of basis function of reversible MCT. */
/* </summary> */
double grk_mct_getnorm(uint32_t compno)
{
    return grk_mct_norms[compno];
}

/* <summary> */
/* Forward irreversible MCT. */
/* </summary> */
#ifdef __SSE4_1__
void grk_mct_encode_real(
    int32_t* restrict c0,
    int32_t* restrict c1,
    int32_t* restrict c2,
    uint64_t n)
{
    size_t i;
    const size_t len = n;

    const __m128i ry = _mm_set1_epi32(2449);
    const __m128i gy = _mm_set1_epi32(4809);
    const __m128i by = _mm_set1_epi32(934);
    const __m128i ru = _mm_set1_epi32(1382);
    const __m128i gu = _mm_set1_epi32(2714);
    /* const __m128i bu = _mm_set1_epi32(4096); */
    /* const __m128i rv = _mm_set1_epi32(4096); */
    const __m128i gv = _mm_set1_epi32(3430);
    const __m128i bv = _mm_set1_epi32(666);
    const __m128i mulround = _mm_shuffle_epi32(_mm_cvtsi32_si128(4096), _MM_SHUFFLE(1, 0, 1, 0));

    for(i = 0; i < (len & ~3U); i += 4) {
        __m128i lo, hi;
        __m128i y, u, v;
        __m128i r = _mm_load_si128((const __m128i *)&(c0[i]));
        __m128i g = _mm_load_si128((const __m128i *)&(c1[i]));
        __m128i b = _mm_load_si128((const __m128i *)&(c2[i]));

        lo = r;
        hi = _mm_shuffle_epi32(r, _MM_SHUFFLE(3, 3, 1, 1));
        lo = _mm_mul_epi32(lo, ry);
        hi = _mm_mul_epi32(hi, ry);
        lo = _mm_add_epi64(lo, mulround);
        hi = _mm_add_epi64(hi, mulround);
        lo = _mm_srli_epi64(lo, 13);
        hi = _mm_slli_epi64(hi, 32-13);
        y = _mm_blend_epi16(lo, hi, 0xCC);

        lo = g;
        hi = _mm_shuffle_epi32(g, _MM_SHUFFLE(3, 3, 1, 1));
        lo = _mm_mul_epi32(lo, gy);
        hi = _mm_mul_epi32(hi, gy);
        lo = _mm_add_epi64(lo, mulround);
        hi = _mm_add_epi64(hi, mulround);
        lo = _mm_srli_epi64(lo, 13);
        hi = _mm_slli_epi64(hi, 32-13);
        y = _mm_add_epi32(y, _mm_blend_epi16(lo, hi, 0xCC));

        lo = b;
        hi = _mm_shuffle_epi32(b, _MM_SHUFFLE(3, 3, 1, 1));
        lo = _mm_mul_epi32(lo, by);
        hi = _mm_mul_epi32(hi, by);
        lo = _mm_add_epi64(lo, mulround);
        hi = _mm_add_epi64(hi, mulround);
        lo = _mm_srli_epi64(lo, 13);
        hi = _mm_slli_epi64(hi, 32-13);
        y = _mm_add_epi32(y, _mm_blend_epi16(lo, hi, 0xCC));
        _mm_store_si128((__m128i *)&(c0[i]), y);

        /*lo = b;
        hi = _mm_shuffle_epi32(b, _MM_SHUFFLE(3, 3, 1, 1));
        lo = _mm_mul_epi32(lo, mulround);
        hi = _mm_mul_epi32(hi, mulround);*/
        lo = _mm_cvtepi32_epi64(_mm_shuffle_epi32(b, _MM_SHUFFLE(3, 2, 2, 0)));
        hi = _mm_cvtepi32_epi64(_mm_shuffle_epi32(b, _MM_SHUFFLE(3, 2, 3, 1)));
        lo = _mm_slli_epi64(lo, 12);
        hi = _mm_slli_epi64(hi, 12);
        lo = _mm_add_epi64(lo, mulround);
        hi = _mm_add_epi64(hi, mulround);
        lo = _mm_srli_epi64(lo, 13);
        hi = _mm_slli_epi64(hi, 32-13);
        u = _mm_blend_epi16(lo, hi, 0xCC);

        lo = r;
        hi = _mm_shuffle_epi32(r, _MM_SHUFFLE(3, 3, 1, 1));
        lo = _mm_mul_epi32(lo, ru);
        hi = _mm_mul_epi32(hi, ru);
        lo = _mm_add_epi64(lo, mulround);
        hi = _mm_add_epi64(hi, mulround);
        lo = _mm_srli_epi64(lo, 13);
        hi = _mm_slli_epi64(hi, 32-13);
        u = _mm_sub_epi32(u, _mm_blend_epi16(lo, hi, 0xCC));

        lo = g;
        hi = _mm_shuffle_epi32(g, _MM_SHUFFLE(3, 3, 1, 1));
        lo = _mm_mul_epi32(lo, gu);
        hi = _mm_mul_epi32(hi, gu);
        lo = _mm_add_epi64(lo, mulround);
        hi = _mm_add_epi64(hi, mulround);
        lo = _mm_srli_epi64(lo, 13);
        hi = _mm_slli_epi64(hi, 32-13);
        u = _mm_sub_epi32(u, _mm_blend_epi16(lo, hi, 0xCC));
        _mm_store_si128((__m128i *)&(c1[i]), u);

        /*lo = r;
        hi = _mm_shuffle_epi32(r, _MM_SHUFFLE(3, 3, 1, 1));
        lo = _mm_mul_epi32(lo, mulround);
        hi = _mm_mul_epi32(hi, mulround);*/
        lo = _mm_cvtepi32_epi64(_mm_shuffle_epi32(r, _MM_SHUFFLE(3, 2, 2, 0)));
        hi = _mm_cvtepi32_epi64(_mm_shuffle_epi32(r, _MM_SHUFFLE(3, 2, 3, 1)));
        lo = _mm_slli_epi64(lo, 12);
        hi = _mm_slli_epi64(hi, 12);
        lo = _mm_add_epi64(lo, mulround);
        hi = _mm_add_epi64(hi, mulround);
        lo = _mm_srli_epi64(lo, 13);
        hi = _mm_slli_epi64(hi, 32-13);
        v = _mm_blend_epi16(lo, hi, 0xCC);

        lo = g;
        hi = _mm_shuffle_epi32(g, _MM_SHUFFLE(3, 3, 1, 1));
        lo = _mm_mul_epi32(lo, gv);
        hi = _mm_mul_epi32(hi, gv);
        lo = _mm_add_epi64(lo, mulround);
        hi = _mm_add_epi64(hi, mulround);
        lo = _mm_srli_epi64(lo, 13);
        hi = _mm_slli_epi64(hi, 32-13);
        v = _mm_sub_epi32(v, _mm_blend_epi16(lo, hi, 0xCC));

        lo = b;
        hi = _mm_shuffle_epi32(b, _MM_SHUFFLE(3, 3, 1, 1));
        lo = _mm_mul_epi32(lo, bv);
        hi = _mm_mul_epi32(hi, bv);
        lo = _mm_add_epi64(lo, mulround);
        hi = _mm_add_epi64(hi, mulround);
        lo = _mm_srli_epi64(lo, 13);
        hi = _mm_slli_epi64(hi, 32-13);
        v = _mm_sub_epi32(v, _mm_blend_epi16(lo, hi, 0xCC));
        _mm_store_si128((__m128i *)&(c2[i]), v);
    }
    for(; i < len; ++i) {
        int32_t r = c0[i];
        int32_t g = c1[i];
        int32_t b = c2[i];
        int32_t y =  grk_int_fix_mul(r, 2449) + grk_int_fix_mul(g, 4809) + grk_int_fix_mul(b, 934);
        int32_t u = -grk_int_fix_mul(r, 1382) - grk_int_fix_mul(g, 2714) + grk_int_fix_mul(b, 4096);
        int32_t v =  grk_int_fix_mul(r, 4096) - grk_int_fix_mul(g, 3430) - grk_int_fix_mul(b, 666);
        c0[i] = y;
        c1[i] = u;
        c2[i] = v;
    }
}
#else
void grk_mct_encode_real(
    int32_t* restrict c0,
    int32_t* restrict c1,
    int32_t* restrict c2,
    uint64_t n)
{
    uint64_t i;
    for(i = 0; i < n; ++i) {
        int32_t r = c0[i];
        int32_t g = c1[i];
        int32_t b = c2[i];
        int32_t y =  grk_int_fix_mul(r, 2449) + grk_int_fix_mul(g, 4809) + grk_int_fix_mul(b, 934);
        int32_t u = -grk_int_fix_mul(r, 1382) - grk_int_fix_mul(g, 2714) + grk_int_fix_mul(b, 4096);
        int32_t v =  grk_int_fix_mul(r, 4096) - grk_int_fix_mul(g, 3430) - grk_int_fix_mul(b, 666);
        c0[i] = y;
        c1[i] = u;
        c2[i] = v;
    }
}
#endif

/* <summary> */
/* Inverse irreversible MCT. */
/* </summary> */
void grk_mct_decode_real(
    float* restrict c0,
    float* restrict c1,
    float* restrict c2,
    uint64_t n)
{
    uint64_t i=0;
#ifdef __SSE__
    __m128 vrv, vgu, vgv, vbu;
    vrv = _mm_set1_ps(1.402f);
    vgu = _mm_set1_ps(0.34413f);
    vgv = _mm_set1_ps(0.71414f);
    vbu = _mm_set1_ps(1.772f);
    for (i = 0; i < (n >> 3); ++i) {
        __m128 vy, vu, vv;
        __m128 vr, vg, vb;

        vy = _mm_load_ps(c0);
        vu = _mm_load_ps(c1);
        vv = _mm_load_ps(c2);
        vr = _mm_add_ps(vy, _mm_mul_ps(vv, vrv));
        vg = _mm_sub_ps(_mm_sub_ps(vy, _mm_mul_ps(vu, vgu)), _mm_mul_ps(vv, vgv));
        vb = _mm_add_ps(vy, _mm_mul_ps(vu, vbu));
        _mm_store_ps(c0, vr);
        _mm_store_ps(c1, vg);
        _mm_store_ps(c2, vb);
        c0 += 4;
        c1 += 4;
        c2 += 4;

        vy = _mm_load_ps(c0);
        vu = _mm_load_ps(c1);
        vv = _mm_load_ps(c2);
        vr = _mm_add_ps(vy, _mm_mul_ps(vv, vrv));
        vg = _mm_sub_ps(_mm_sub_ps(vy, _mm_mul_ps(vu, vgu)), _mm_mul_ps(vv, vgv));
        vb = _mm_add_ps(vy, _mm_mul_ps(vu, vbu));
        _mm_store_ps(c0, vr);
        _mm_store_ps(c1, vg);
        _mm_store_ps(c2, vb);
        c0 += 4;
        c1 += 4;
        c2 += 4;
    }
    n &= 7;
#endif
#ifdef _OPENMP
#pragma omp parallel default(none) private(i) shared(c0, c1,c2,n)
	{
#pragma omp for
#endif

		for (i = 0; i < n; ++i) {
			float y = c0[i];
			float u = c1[i];
			float v = c2[i];
			float r = y + (v * 1.402f);
			float g = y - (u * 0.34413f) - (v * (0.71414f));
			float b = y + (u * 1.772f);
			c0[i] = r;
			c1[i] = g;
			c2[i] = b;
		}
#ifdef _OPENMP
	}
#endif
}

/* <summary> */
/* Get norm of basis function of irreversible MCT. */
/* </summary> */
double grk_mct_getnorm_real(uint32_t compno)
{
    return grk_mct_norms_real[compno];
}


bool grk_mct_encode_custom(
    uint8_t * pCodingdata,
    uint64_t n,
    uint8_t ** pData,
    uint32_t pNbComp,
    uint32_t isSigned)
{
    float * lMct = (float *) pCodingdata;
    uint64_t i;
    uint32_t j;
    uint32_t k;
    uint32_t lNbMatCoeff = pNbComp * pNbComp;
    int32_t * lCurrentData = nullptr;
    int32_t * lCurrentMatrix = nullptr;
    int32_t ** lData = (int32_t **) pData;
    uint32_t lMultiplicator = 1 << 13;
    int32_t * lMctPtr;

    OPJ_ARG_NOT_USED(isSigned);

    lCurrentData = (int32_t *) grk_malloc((pNbComp + lNbMatCoeff) * sizeof(int32_t));
    if (! lCurrentData) {
        return false;
    }

    lCurrentMatrix = lCurrentData + pNbComp;

    for (i =0; i<lNbMatCoeff; ++i) {
        lCurrentMatrix[i] = (int32_t) (*(lMct++) * (float)lMultiplicator);
    }

    for (i = 0; i < n; ++i)  {
        lMctPtr = lCurrentMatrix;
        for (j=0; j<pNbComp; ++j) {
            lCurrentData[j] = (*(lData[j]));
        }

        for (j=0; j<pNbComp; ++j) {
            *(lData[j]) = 0;
            for (k=0; k<pNbComp; ++k) {
                *(lData[j]) += grk_int_fix_mul(*lMctPtr, lCurrentData[k]);
                ++lMctPtr;
            }

            ++lData[j];
        }
    }

    grk_free(lCurrentData);

    return true;
}

bool grk_mct_decode_custom(
    uint8_t * pDecodingData,
    uint64_t n,
    uint8_t ** pData,
    uint32_t pNbComp,
    uint32_t isSigned)
{
    float * lMct;
    uint64_t i;
    uint32_t j;
    uint32_t k;

    float * lCurrentData = nullptr;
    float * lCurrentResult = nullptr;
    float ** lData = (float **) pData;

    OPJ_ARG_NOT_USED(isSigned);

    lCurrentData = (float *) grk_malloc (2 * pNbComp * sizeof(float));
    if (! lCurrentData) {
        return false;
    }
    lCurrentResult = lCurrentData + pNbComp;

    for (i = 0; i < n; ++i) {
        lMct = (float *) pDecodingData;
        for (j=0; j<pNbComp; ++j) {
            lCurrentData[j] = (float) (*(lData[j]));
        }
        for (j=0; j<pNbComp; ++j) {
            lCurrentResult[j] = 0;
            for	(k=0; k<pNbComp; ++k)	{
                lCurrentResult[j] += *(lMct++) * lCurrentData[k];
            }
            *(lData[j]++) = (float) (lCurrentResult[j]);
        }
    }
    grk_free(lCurrentData);
    return true;
}

void opj_calculate_norms(	double * pNorms,
                            uint32_t pNbComps,
                            float * pMatrix)
{
    uint32_t i,j,lIndex;
    float lCurrentValue;
    double * lNorms = (double *) pNorms;
    float * lMatrix = (float *) pMatrix;

    for	(i=0; i<pNbComps; ++i) {
        lNorms[i] = 0;
        lIndex = i;

        for	(j=0; j<pNbComps; ++j) {
            lCurrentValue = lMatrix[lIndex];
            lIndex += pNbComps;
            lNorms[i] += lCurrentValue * lCurrentValue;
        }
        lNorms[i] = sqrt(lNorms[i]);
    }
}
