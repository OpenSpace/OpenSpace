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

#include "grk_includes.h"
#include "t1_luts.h"

/** @defgroup T1 T1 - Implementation of the tier-1 coding */
/*@{*/

/** @name Local static functions */
/*@{*/

#define ENC_FLAGS(x, y) (t1->flags[(x) + 1 + (((y) >> 2) + 1) * t1->flags_stride])
#define ENC_FLAGS_ADDRESS(x, y) (t1->flags + ((x) + 1 + (((y) >> 2) + 1) * t1->flags_stride))

static inline uint8_t		grk_t1_getctxno_zc_opt(uint32_t f, uint32_t orient);
static inline uint8_t		grk_t1_getctxno_sc_opt(uint32_t fX, uint32_t pfX, uint32_t nfX, uint32_t ci);
static inline uint32_t		grk_t1_getctxno_mag_opt(uint32_t f);
static inline uint8_t		grk_t1_getspb_opt(uint32_t fX, uint32_t pfX, uint32_t nfX, uint32_t ci);
static inline void			grk_t1_updateflags_opt(opj_flag_opt_t *flagsp, uint32_t ci, uint32_t s, uint32_t stride);

/**
Encode significant pass
*/
static void grk_t1_enc_sigpass_step(grk_t1_opt_t *t1,
                                    opj_flag_opt_t *flagsp,
                                    uint32_t *datap,
                                    uint32_t orient,
                                    int32_t bpno,
                                    int32_t one,
                                    int32_t *nmsedec);

/**
Encode significant pass
*/
static void grk_t1_enc_sigpass( grk_t1_opt_t *t1,
                                int32_t bpno,
                                uint32_t orient,
                                int32_t *nmsedec);


/**
Encode refinement pass
*/
static void grk_t1_enc_refpass_step(grk_t1_opt_t *t1,
                                    opj_flag_opt_t *flagsp,
                                    uint32_t *datap,
                                    int32_t bpno,
                                    int32_t one,
                                    int32_t *nmsedec);


/**
Encode refinement pass
*/
static void grk_t1_enc_refpass( grk_t1_opt_t *t1,
                                int32_t bpno,
                                int32_t *nmsedec);

/**
Encode clean-up pass
*/
static void grk_t1_enc_clnpass_step(
    grk_t1_opt_t *t1,
    opj_flag_opt_t *flagsp,
    uint32_t *datap,
    uint32_t orient,
    int32_t bpno,
    int32_t one,
    int32_t *nmsedec,
    uint32_t agg,
    uint32_t runlen,
    uint32_t y);

/**
Encode clean-up pass
*/
static void grk_t1_enc_clnpass(
    grk_t1_opt_t *t1,
    int32_t bpno,
    uint32_t orient,
    int32_t *nmsedec);

/**
* Creates a new Tier 1 handle
* and initializes the look-up tables of the Tier-1 coder/decoder
* @return a new T1 handle if successful, returns NULL otherwise
*/
grk_t1_opt_t* grk_t1_opt_create(bool isEncoder){
    grk_t1_opt_t *l_t1 = nullptr;

    l_t1 = (grk_t1_opt_t*)grk_calloc(1, sizeof(grk_t1_opt_t));
    if (!l_t1) {
        return nullptr;
    }

    /* create MQC handles */
    l_t1->mqc = grk_mqc_create();
    if (!l_t1->mqc) {
        grk_t1_opt_destroy(l_t1);
        return nullptr;
    }

    l_t1->encoder = isEncoder;

    return l_t1;
}


/**
* Destroys a previously created T1 handle
*
* @param p_t1 Tier 1 handle to destroy
*/
void grk_t1_opt_destroy(grk_t1_opt_t *p_t1){
    if (!p_t1) {
        return;
    }

    /* destroy MQC handles */
    grk_mqc_destroy(p_t1->mqc);
    p_t1->mqc = nullptr;

    /* encoder uses tile buffer, so no need to free */
    if (p_t1->data) {
        grk_aligned_free(p_t1->data);
        p_t1->data = nullptr;
    }

    if (p_t1->flags) {
        grk_aligned_free(p_t1->flags);
        p_t1->flags = nullptr;
    }

    grk_free(p_t1);
}

static inline uint8_t grk_t1_getctxno_zc_opt(uint32_t f, uint32_t orient){
    return lut_ctxno_zc_opt[(orient << 9) | (f & T1_SIGMA_NEIGHBOURS)];
}


static uint8_t grk_t1_getctxno_sc_opt(uint32_t fX, uint32_t pfX, uint32_t nfX, uint32_t ci){
    /*
    0 pfX T1_CHI_THIS           T1_LUT_CTXNO_SGN_W
    1 tfX T1_SIGMA_1            T1_LUT_CTXNO_SIG_N
    2 nfX T1_CHI_THIS           T1_LUT_CTXNO_SGN_E
    3 tfX T1_SIGMA_3            T1_LUT_CTXNO_SIG_W
    4  fX T1_CHI_(THIS - 1)     T1_LUT_CTXNO_SGN_N
    5 tfX T1_SIGMA_5            T1_LUT_CTXNO_SIG_E
    6  fX T1_CHI_(THIS + 1)     T1_LUT_CTXNO_SGN_S
    7 tfX T1_SIGMA_7            T1_LUT_CTXNO_SIG_S
    */

    uint32_t lu = (fX >> (ci * 3)) & (T1_SIGMA_1 | T1_SIGMA_3 | T1_SIGMA_5 | T1_SIGMA_7);

    lu |= (pfX >> (T1_CHI_THIS_I + (ci * 3U))) & (1U << 0);
    lu |= (nfX >> (T1_CHI_THIS_I - 2U + (ci * 3U))) & (1U << 2);
    if (ci == 0U) {
        lu |= (fX >> (T1_CHI_0_I - 4U)) & (1U << 4);
    } else {
        lu |= (fX >> (T1_CHI_1_I - 4U + ((ci - 1U) * 3U))) & (1U << 4);
    }
    lu |= (fX >> (T1_CHI_2_I - 6U + (ci * 3U))) & (1U << 6);

    return lut_ctxno_sc_opt[lu];
}


static inline uint32_t grk_t1_getctxno_mag_opt(uint32_t f){
    return (f & T1_MU_THIS) ? (T1_CTXNO_MAG + 2) : ((f & T1_SIGMA_NEIGHBOURS) ? T1_CTXNO_MAG + 1 : T1_CTXNO_MAG);
}

static uint8_t grk_t1_getspb_opt(uint32_t fX, uint32_t pfX, uint32_t nfX, uint32_t ci)
{
    /*
    0 pfX T1_CHI_THIS           T1_LUT_SGN_W
    1 tfX T1_SIGMA_1            T1_LUT_SIG_N
    2 nfX T1_CHI_THIS           T1_LUT_SGN_E
    3 tfX T1_SIGMA_3            T1_LUT_SIG_W
    4  fX T1_CHI_(THIS - 1)     T1_LUT_SGN_N
    5 tfX T1_SIGMA_5            T1_LUT_SIG_E
    6  fX T1_CHI_(THIS + 1)     T1_LUT_SGN_S
    7 tfX T1_SIGMA_7            T1_LUT_SIG_S
    */

    uint32_t lu = (fX >> (ci * 3U)) & (T1_SIGMA_1 | T1_SIGMA_3 | T1_SIGMA_5 | T1_SIGMA_7);

    lu |= (pfX >> (T1_CHI_THIS_I + (ci * 3U))) & (1U << 0);
    lu |= (nfX >> (T1_CHI_THIS_I - 2U + (ci * 3U))) & (1U << 2);
    if (ci == 0U) {
        lu |= (fX >> (T1_CHI_0_I - 4U)) & (1U << 4);
    } else {
        lu |= (fX >> (T1_CHI_1_I - 4U + ((ci - 1U) * 3U))) & (1U << 4);
    }
    lu |= (fX >> (T1_CHI_2_I - 6U + (ci * 3U))) & (1U << 6);

    return lut_spb_opt[lu];
}

static void grk_t1_updateflags_opt(opj_flag_opt_t *flagsp, uint32_t ci, uint32_t s, uint32_t stride)
{
    /* set up to point to the north and south data points' flags words, if required */
    opj_flag_opt_t* north = NULL;
    opj_flag_opt_t* south = NULL;

    /* mark target as significant */
    *flagsp |= T1_SIGMA_THIS << (3U * ci);

    /* north-west, north, north-east */
    if (ci == 0U) {
        north = flagsp - stride;
        *north |= T1_SIGMA_16;
        north[-1] |= T1_SIGMA_17;
        north[1] |= T1_SIGMA_15;
    }

    /* south-west, south, south-east */
    if (ci == 3U) {
        south = flagsp + stride;
        *south |= T1_SIGMA_1;
        south[-1] |= T1_SIGMA_2;
        south[1] |= T1_SIGMA_0;
    }

    /* east */
    flagsp[-1] |= T1_SIGMA_5 << (3U * ci);

    /* west */
    flagsp[1] |= T1_SIGMA_3 << (3U * ci);

    if (s) {
        switch (ci) {
        case 0U: {
            *flagsp |= T1_CHI_1;
            *north |= T1_CHI_5;
            break;
        }
        case 1:
            *flagsp |= T1_CHI_2;
            break;
        case 2:
            *flagsp |= T1_CHI_3;
            break;
        case 3: {
            *flagsp |= T1_CHI_4;
            *south |= T1_CHI_0;
            break;
        }

        }
    }
}

static void  grk_t1_enc_sigpass_step(   grk_t1_opt_t *t1,
                                        opj_flag_opt_t *flagsp,
                                        uint32_t *datap,
                                        uint32_t orient,
                                        int32_t bpno,
                                        int32_t one,
                                        int32_t *nmsedec)
{
    uint32_t v;
    uint32_t ci;

    grk_mqc_t *mqc = t1->mqc;
    if (*flagsp == 0U) {
        return;  /* Nothing to do for any of the 4 data points */
    }
    for (ci = 0U; ci < 4U; ++ci) {
        uint32_t const shift_flags = *flagsp >> (ci * 3U);
        /* if location is not significant, has not been coded in significance pass, and is in preferred neighbourhood,
        then code in this pass: */
        if ((shift_flags & (T1_SIGMA_THIS | T1_PI_THIS)) == 0U && (shift_flags & T1_SIGMA_NEIGHBOURS) != 0U) {
            v = (*datap >> one) & 1;
            grk_mqc_setcurctx(mqc, grk_t1_getctxno_zc_opt(shift_flags, orient));
            grk_mqc_encode(mqc, v);
            if (v) {
                /* sign bit */
                v = *datap >> T1_DATA_SIGN_BIT_INDEX;
                *nmsedec += grk_t1_getnmsedec_sig(*datap, (uint32_t)bpno);
                grk_mqc_setcurctx(mqc, grk_t1_getctxno_sc_opt(*flagsp, flagsp[-1], flagsp[1], ci));
                grk_mqc_encode(mqc, v ^ grk_t1_getspb_opt(*flagsp, flagsp[-1], flagsp[1], ci));
                grk_t1_updateflags_opt(flagsp, ci, v, t1->flags_stride);
            }
            /* set propagation pass bit for this location */
            *flagsp |= T1_PI_THIS << (ci * 3U);
        }
        datap += t1->w;
    }
}



static void grk_t1_enc_sigpass(grk_t1_opt_t *t1,
                               int32_t bpno,
                               uint32_t orient,
                               int32_t *nmsedec  )
{
    uint32_t i, k;
    int32_t const one =  (bpno + T1_NMSEDEC_FRACBITS);
    uint32_t const flag_row_extra = t1->flags_stride - t1->w;
    uint32_t const data_row_extra =  (t1->w << 2) - t1->w;

    opj_flag_opt_t* f = ENC_FLAGS_ADDRESS(0, 0);
    uint32_t* d = t1->data;

    *nmsedec = 0;
    for (k = 0; k < t1->h; k += 4) {
        for (i = 0; i < t1->w; ++i) {
            grk_t1_enc_sigpass_step(
                t1,
                f,
                d,
                orient,
                bpno,
                one,
                nmsedec);

            ++f;
            ++d;
        }
        d += data_row_extra;
        f += flag_row_extra;
    }
}

static void grk_t1_enc_refpass_step(   grk_t1_opt_t *t1,
                                       opj_flag_opt_t *flagsp,
                                       uint32_t *datap,
                                       int32_t bpno,
                                       int32_t one,
                                       int32_t *nmsedec)
{
    uint32_t v;
    uint32_t ci;

    grk_mqc_t *mqc = t1->mqc;

    if ((*flagsp & (T1_SIGMA_4 | T1_SIGMA_7 | T1_SIGMA_10 | T1_SIGMA_13)) == 0) {
        /* none significant */
        return;
    }
    if ((*flagsp & (T1_PI_0 | T1_PI_1 | T1_PI_2 | T1_PI_3)) == (T1_PI_0 | T1_PI_1 | T1_PI_2 | T1_PI_3)) {
        /* all processed by sigpass */
        return;
    }

    for (ci = 0U; ci < 4U; ++ci) {
        uint32_t shift_flags = *flagsp >> (ci * 3U);
        /* if location is significant, but has not been coded in significance propagation pass, then code in this pass: */
        if ((shift_flags & (T1_SIGMA_THIS | T1_PI_THIS)) == T1_SIGMA_THIS) {
            *nmsedec += grk_t1_getnmsedec_ref(*datap, (uint32_t)bpno);
            v = (*datap >> one) & 1;
            grk_mqc_setcurctx(mqc, grk_t1_getctxno_mag_opt(shift_flags));
            grk_mqc_encode(mqc, v);
            /* flip magnitude refinement bit*/
            *flagsp |= T1_MU_THIS << (ci * 3U);
        }
        datap += t1->w;
    }
}

static void grk_t1_enc_refpass(
    grk_t1_opt_t *t1,
    int32_t bpno,
    int32_t *nmsedec)
{
    uint32_t i, k;
    const int32_t one =  (bpno + T1_NMSEDEC_FRACBITS);
    opj_flag_opt_t* f = ENC_FLAGS_ADDRESS(0, 0);
    uint32_t const flag_row_extra = t1->flags_stride - t1->w;
    uint32_t const data_row_extra = (t1->w << 2) - t1->w;
    uint32_t* d = t1->data;

    *nmsedec = 0;
    for (k = 0U; k < t1->h; k += 4U) {
        for (i = 0U; i < t1->w; ++i) {
            grk_t1_enc_refpass_step(
                t1,
                f,
                d,
                bpno,
                one,
                nmsedec);
            ++f;
            ++d;
        }
        f += flag_row_extra;
        d += data_row_extra;
    }
}

static void grk_t1_enc_clnpass_step(   grk_t1_opt_t *t1,
										opj_flag_opt_t *flagsp,
										uint32_t *datap,
										uint32_t orient,
										int32_t bpno,
										int32_t one,
										int32_t *nmsedec,
										uint32_t agg,
										uint32_t runlen,
										uint32_t y)
{
    uint32_t v;
    uint32_t ci;
    grk_mqc_t *mqc = t1->mqc;

    uint32_t lim;
    const uint32_t check = (T1_SIGMA_4 | T1_SIGMA_7 | T1_SIGMA_10 | T1_SIGMA_13 | T1_PI_0 | T1_PI_1 | T1_PI_2 | T1_PI_3);

    if ((*flagsp & check) == check) {
        if (runlen == 0) {
            *flagsp &= ~(T1_PI_0 | T1_PI_1 | T1_PI_2 | T1_PI_3);
        } else if (runlen == 1) {
            *flagsp &= ~(T1_PI_1 | T1_PI_2 | T1_PI_3);
        } else if (runlen == 2) {
            *flagsp &= ~(T1_PI_2 | T1_PI_3);
        } else if (runlen == 3) {
            *flagsp &= ~(T1_PI_3);
        }
        return;
    }

    lim = 4U < (t1->h - y) ? 4U : (t1->h - y);
    for (ci = runlen; ci < lim; ++ci) {
        opj_flag_opt_t shift_flags;
        if ((agg != 0) && (ci == runlen)) {
            goto LABEL_PARTIAL;
        }

        shift_flags = *flagsp >> (ci * 3U);

        if (!(shift_flags & (T1_SIGMA_THIS | T1_PI_THIS))) {
            grk_mqc_setcurctx(mqc, grk_t1_getctxno_zc_opt(shift_flags, orient));
            v = (*datap >> one) & 1;
            grk_mqc_encode(mqc, v);
            if (v) {
LABEL_PARTIAL:
                *nmsedec += grk_t1_getnmsedec_sig(*datap, (uint32_t)bpno);
                grk_mqc_setcurctx(mqc, grk_t1_getctxno_sc_opt(*flagsp, flagsp[-1], flagsp[1], ci));
                /* sign bit */
                v = *datap >> T1_DATA_SIGN_BIT_INDEX;
                grk_mqc_encode(mqc, v ^ grk_t1_getspb_opt(*flagsp, flagsp[-1], flagsp[1], ci));
                grk_t1_updateflags_opt(flagsp, ci, v, t1->flags_stride);
            }
        }
        *flagsp &= ~(T1_PI_0 << (3U * ci));
        datap += t1->w;
    }
}


static void grk_t1_enc_clnpass( grk_t1_opt_t *t1,
								int32_t bpno,
								uint32_t orient,
								int32_t *nmsedec)
{
    uint32_t i, k;
    const int32_t one = (bpno + T1_NMSEDEC_FRACBITS);
    uint32_t agg, runlen;

    grk_mqc_t *mqc = t1->mqc;

    *nmsedec = 0;

    for (k = 0; k < t1->h; k += 4) {
        for (i = 0; i < t1->w; ++i) {
            agg = !ENC_FLAGS(i, k);
            if (agg) {
                for (runlen = 0; runlen < 4; ++runlen) {
                    if ( (t1->data[((k + runlen)*t1->w) + i] >> one) & 1)
                        break;
                }
                grk_mqc_setcurctx(mqc, T1_CTXNO_AGG);
                grk_mqc_encode(mqc, runlen != 4);
                if (runlen == 4) {
                    continue;
                }
                grk_mqc_setcurctx(mqc, T1_CTXNO_UNI);
                grk_mqc_encode(mqc, runlen >> 1);
                grk_mqc_encode(mqc, runlen & 1);
            } else {
                runlen = 0;
            }
            grk_t1_enc_clnpass_step(
                t1,
                ENC_FLAGS_ADDRESS(i, k),
                t1->data + ((k + runlen) * t1->w) + i,
                orient,
                bpno,
                one,
                nmsedec,
                agg,
                runlen,
                k);
        }
    }
}


bool grk_t1_opt_allocate_buffers(   grk_t1_opt_t *t1,
									uint32_t cblkw,
									uint32_t cblkh)
{
    if (!t1->data) {
        t1->data = (uint32_t*)grk_aligned_malloc(cblkw*cblkh * sizeof(int32_t));
        if (!t1->data) {
            /* FIXME event manager error callback */
            return false;
        }
    }
    if (!t1->flags) {
		auto flags_stride = cblkw + 2;
		auto flags_height = (cblkh + 3U) / 4U;
		auto flagssize = flags_stride * (flags_height + 2);
        t1->flags = (opj_flag_opt_t*)grk_aligned_malloc(flagssize * sizeof(opj_flag_opt_t));
        if (!t1->flags) {
            /* FIXME event manager error callback */
            return false;
        }
    }
     return true;
}

void grk_t1_opt_init_buffers(grk_t1_opt_t *t1,
							uint32_t w,
							uint32_t h){
	uint32_t x;
	opj_flag_opt_t* p;
	if (t1->data)
		memset(t1->data, 0, w*h * sizeof(int32_t));

	t1->flags_stride = w + 2;
	auto flags_height = (h + 3U) / 4U;
	auto flagssize = t1->flags_stride * (flags_height + 2);
	memset(t1->flags, 0, flagssize * sizeof(opj_flag_opt_t)); /* Shall we keep memset for encoder ? */

	/* BIG FAT XXX */
	p = &t1->flags[0];
	for (x = 0; x < t1->flags_stride; ++x) {
		/* magic value to hopefully stop any passes being interested in this entry */
		*p++ = (T1_PI_0 | T1_PI_1 | T1_PI_2 | T1_PI_3);
	}

	p = &t1->flags[((flags_height + 1) * t1->flags_stride)];
	for (x = 0; x < t1->flags_stride; ++x) {
		/* magic value to hopefully stop any passes being interested in this entry */
		*p++ = (T1_PI_0 | T1_PI_1 | T1_PI_2 | T1_PI_3);
	}

	if (h % 4) {
		uint32_t v = 0;
		p = &t1->flags[((flags_height)* t1->flags_stride)];
		if (h % 4 == 1) {
			v |= T1_PI_1 | T1_PI_2 | T1_PI_3;
		}
		else if (h % 4 == 2) {
			v |= T1_PI_2 | T1_PI_3;
		}
		else if (h % 4 == 3) {
			v |= T1_PI_3;
		}
		for (x = 0; x < t1->flags_stride; ++x) {
			*p++ = v;
		}
	}

	t1->w = w;
	t1->h = h;
}

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
                               uint32_t max){
    double cumwmsedec = 0.0;

    grk_mqc_t *mqc = t1->mqc;

    uint32_t passno;
    int32_t bpno;
    uint32_t passtype;
    int32_t nmsedec = 0;
    double tempwmsedec;

	auto logMax = grk_int_floorlog2((int32_t)max) + 1;
    cblk->numbps = (max && (logMax > T1_NMSEDEC_FRACBITS)) ? (uint32_t)(logMax - T1_NMSEDEC_FRACBITS) : 0;
	if (!cblk->numbps)
		return 0;

	bool TERMALL = (cblksty & J2K_CCP_CBLKSTY_TERMALL) ? true : false;

    bpno = (int32_t)(cblk->numbps - 1);
    passtype = 2;
    grk_mqc_init_enc(mqc, cblk->data);
	uint32_t state = opj_plugin_get_debug_state();
	if (state & OPJ_PLUGIN_STATE_DEBUG) {
		mqc->debug_mqc.contextStream = cblk->contextStream;
	}

    for (passno = 0; bpno >= 0; ++passno) {
        grk_tcd_pass_t *pass = &cblk->passes[passno];
        uint32_t correction = 3;
        switch (passtype) {
        case 0:
            grk_t1_enc_sigpass(t1, bpno, orient, &nmsedec);
            break;
        case 1:
            grk_t1_enc_refpass(t1, bpno, &nmsedec);
            break;
        case 2:
            grk_t1_enc_clnpass(t1, bpno, orient, &nmsedec);
			if (state & OPJ_PLUGIN_STATE_DEBUG) {
				mqc_next_plane(&mqc->debug_mqc);
			}
            break;
        }
		if (cblksty == J2K_CCP_CBLKSTY_RESET)
			grk_mqc_resetstates(mqc);
        tempwmsedec = grk_t1_getwmsedec(nmsedec, compno, level, orient, bpno, qmfbid, stepsize, numcomps,mct_norms, mct_numcomps) ;
        cumwmsedec += tempwmsedec;

		if (TERMALL){
			grk_mqc_flush(mqc);
			correction = 0;
			pass->term = 1;
		}
		else {
			if (mqc->COUNT < 5)
				correction++;
			pass->term = 0;
		}
        if (++passtype == 3) {
            passtype = 0;
            bpno--;
        }
		pass->distortiondec = cumwmsedec;
		pass->rate = grk_mqc_numbytes(mqc) + correction;

		if (bpno >= 0) {
			if (pass->term) {
				grk_mqc_restart_init_enc(mqc);
			}
		}
    }

	grk_tcd_pass_t *finalPass = &cblk->passes[passno-1];
	if (!finalPass->term)
		grk_mqc_flush(mqc);
    cblk->num_passes_encoded = passno;

    for (passno = 0; passno<cblk->num_passes_encoded; passno++) {
        grk_tcd_pass_t *pass = &cblk->passes[passno];
		if (!pass->term) {
			pass->rate++;
			auto bytes = grk_mqc_numbytes(mqc);
			if (pass->rate > (uint32_t)bytes)
				pass->rate = (uint32_t)bytes;
			/*Preventing generation of FF as last data byte of a pass*/
			if (pass->rate > 0 && cblk->data[pass->rate - 1] == 0xFF) {
				pass->rate--;
			}
		}
        pass->len = pass->rate - (passno == 0 ? 0 : cblk->passes[passno - 1].rate);
    }
	return cumwmsedec;
}