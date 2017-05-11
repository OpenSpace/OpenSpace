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
#include "T1Encoder.h"

/** @defgroup T1 T1 - Implementation of the tier-1 coding */
/*@{*/

/** @name Local static functions */
/*@{*/

/**
Tier-1 coding (coding of code-block coefficients)
*/
static inline uint8_t grk_t1_getctxno_zc(uint32_t f, uint32_t orient);
static uint8_t grk_t1_getctxno_sc(uint32_t f);
static inline uint32_t grk_t1_getctxno_mag(uint32_t f);
static uint8_t grk_t1_getspb(uint32_t f);
static void grk_t1_updateflags(opj_flag_t *flagsp, uint32_t s, uint32_t stride);
/**
Encode significant pass
*/
static void grk_t1_enc_sigpass_step(grk_t1_t *t1,
                                    opj_flag_t *flagsp,
                                    int32_t *datap,
                                    uint32_t orient,
                                    int32_t bpno,
                                    int32_t one,
                                    int32_t *nmsedec,
                                    uint8_t type,
                                    uint32_t vsc);

/**
Decode significant pass
*/
static inline void grk_t1_dec_sigpass_step_raw(
    grk_t1_t *t1,
    opj_flag_t *flagsp,
    int32_t *datap,
    int32_t orient,
    int32_t oneplushalf,
    int32_t vsc);
static inline void grk_t1_dec_sigpass_step_mqc(
    grk_t1_t *t1,
    opj_flag_t *flagsp,
    int32_t *datap,
    int32_t orient,
    int32_t oneplushalf);
static inline void grk_t1_dec_sigpass_step_mqc_vsc(
    grk_t1_t *t1,
    opj_flag_t *flagsp,
    int32_t *datap,
    int32_t orient,
    int32_t oneplushalf,
    int32_t vsc);


/**
Encode significant pass
*/
static void grk_t1_enc_sigpass( grk_t1_t *t1,
                                int32_t bpno,
                                uint32_t orient,
                                int32_t *nmsedec,
                                uint8_t type,
                                uint32_t cblksty);

/**
Decode significant pass
*/
static void grk_t1_dec_sigpass_raw(
    grk_t1_t *t1,
    int32_t bpno,
    int32_t orient,
    int32_t cblksty);
static void grk_t1_dec_sigpass_mqc(
    grk_t1_t *t1,
    int32_t bpno,
    int32_t orient);
static void grk_t1_dec_sigpass_mqc_vsc(
    grk_t1_t *t1,
    int32_t bpno,
    int32_t orient);



/**
Encode refinement pass
*/
static void grk_t1_enc_refpass_step(grk_t1_t *t1,
                                    opj_flag_t *flagsp,
                                    int32_t *datap,
                                    int32_t bpno,
                                    int32_t one,
                                    int32_t *nmsedec,
                                    uint8_t type,
                                    uint32_t vsc);


/**
Encode refinement pass
*/
static void grk_t1_enc_refpass( grk_t1_t *t1,
                                int32_t bpno,
                                int32_t *nmsedec,
                                uint8_t type,
                                uint32_t cblksty);

/**
Decode refinement pass
*/
static void grk_t1_dec_refpass_raw(
    grk_t1_t *t1,
    int32_t bpno,
    int32_t cblksty);
static void grk_t1_dec_refpass_mqc(
    grk_t1_t *t1,
    int32_t bpno);
static void grk_t1_dec_refpass_mqc_vsc(
    grk_t1_t *t1,
    int32_t bpno);


/**
Decode refinement pass
*/
static inline void  grk_t1_dec_refpass_step_raw(
    grk_t1_t *t1,
    opj_flag_t *flagsp,
    int32_t *datap,
    int32_t poshalf,
    int32_t neghalf,
    int32_t vsc);
static inline void grk_t1_dec_refpass_step_mqc(
    grk_t1_t *t1,
    opj_flag_t *flagsp,
    int32_t *datap,
    int32_t poshalf,
    int32_t neghalf);
static inline void grk_t1_dec_refpass_step_mqc_vsc(
    grk_t1_t *t1,
    opj_flag_t *flagsp,
    int32_t *datap,
    int32_t poshalf,
    int32_t neghalf,
    int32_t vsc);



/**
Encode clean-up pass
*/
static void grk_t1_enc_clnpass_step(
    grk_t1_t *t1,
    opj_flag_t *flagsp,
    int32_t *datap,
    uint32_t orient,
    int32_t bpno,
    int32_t one,
    int32_t *nmsedec,
    uint32_t partial,
    uint32_t vsc);
/**
Decode clean-up pass
*/
static void grk_t1_dec_clnpass_step_partial(
    grk_t1_t *t1,
    opj_flag_t *flagsp,
    int32_t *datap,
    int32_t orient,
    int32_t oneplushalf);
static void grk_t1_dec_clnpass_step(
    grk_t1_t *t1,
    opj_flag_t *flagsp,
    int32_t *datap,
    int32_t orient,
    int32_t oneplushalf);
static void grk_t1_dec_clnpass_step_vsc(
    grk_t1_t *t1,
    opj_flag_t *flagsp,
    int32_t *datap,
    int32_t orient,
    int32_t oneplushalf,
    int32_t partial,
    int32_t vsc);
/**
Encode clean-up pass
*/
static void grk_t1_enc_clnpass(
    grk_t1_t *t1,
    int32_t bpno,
    uint32_t orient,
    int32_t *nmsedec,
    uint32_t cblksty);
/**
Decode clean-up pass
*/
static void grk_t1_dec_clnpass(
    grk_t1_t *t1,
    int32_t bpno,
    int32_t orient,
    int32_t cblksty);

/*@}*/

/*@}*/



static uint8_t grk_t1_getctxno_zc(uint32_t f, uint32_t orient){
    return lut_ctxno_zc[(orient << 8) | (f & T1_SIG_OTH)];
}

static uint8_t grk_t1_getctxno_sc(uint32_t f){
    return lut_ctxno_sc[(f & (T1_SIG_PRIM | T1_SGN)) >> 4];
}

static uint32_t grk_t1_getctxno_mag(uint32_t f){
    uint32_t tmp1 = (f & T1_SIG_OTH) ? T1_CTXNO_MAG + 1 : T1_CTXNO_MAG;
    uint32_t tmp2 = (f & T1_REFINE) ? T1_CTXNO_MAG + 2 : tmp1;
    return (tmp2);
}

static uint8_t grk_t1_getspb(uint32_t f){
    return lut_spb[(f & (T1_SIG_PRIM | T1_SGN)) >> 4];
}

int16_t grk_t1_getnmsedec_sig(uint32_t x, uint32_t bitpos){
    if (bitpos > 0) {
        return lut_nmsedec_sig[(x >> (bitpos)) & ((1 << T1_NMSEDEC_BITS) - 1)];
    }

    return lut_nmsedec_sig0[x & ((1 << T1_NMSEDEC_BITS) - 1)];
}

int16_t grk_t1_getnmsedec_ref(uint32_t x, uint32_t bitpos){
    if (bitpos > 0) {
        return lut_nmsedec_ref[(x >> (bitpos)) & ((1 << T1_NMSEDEC_BITS) - 1)];
    }

    return lut_nmsedec_ref0[x & ((1 << T1_NMSEDEC_BITS) - 1)];
}

static void grk_t1_updateflags(opj_flag_t *flagsp, uint32_t s, uint32_t stride){
    opj_flag_t *np = flagsp - stride;
    opj_flag_t *sp = flagsp + stride;

    static const opj_flag_t mod[] = {
        T1_SIG_S, T1_SIG_S|T1_SGN_S,
        T1_SIG_E, T1_SIG_E|T1_SGN_E,
        T1_SIG_W, T1_SIG_W|T1_SGN_W,
        T1_SIG_N, T1_SIG_N|T1_SGN_N
    };

    np[-1] |= T1_SIG_SE;
    np[0]  |= mod[s];
    np[1]  |= T1_SIG_SW;

    flagsp[-1] |= mod[s+2];
    flagsp[0]  |= T1_SIG;
    flagsp[1]  |= mod[s+4];

    sp[-1] |= T1_SIG_NE;
    sp[0]  |= mod[s+6];
    sp[1]  |= T1_SIG_NW;
}

static void grk_t1_enc_sigpass_step(   grk_t1_t *t1,
                                       opj_flag_t *flagsp,
                                       int32_t *datap,
                                       uint32_t orient,
                                       int32_t bpno,
                                       int32_t one,
                                       int32_t *nmsedec,
                                       uint8_t type,
                                       uint32_t vsc
                                   )
{
    int32_t v;
    uint32_t flag;

    grk_mqc_t *mqc = t1->mqc;	

    flag = vsc ? (uint32_t)((*flagsp) & (~(T1_SIG_S | T1_SIG_SE | T1_SIG_SW | T1_SGN_S))) : (uint32_t)(*flagsp);
    if ((flag & T1_SIG_OTH) && !(flag & (T1_SIG))) {
        v = (abs(*datap) & one) ? 1 : 0;
        grk_mqc_setcurctx(mqc, grk_t1_getctxno_zc(flag, orient));	
        if (type == T1_TYPE_RAW) {	/* BYPASS/LAZY MODE */
            grk_mqc_bypass_enc(mqc, (uint32_t)v);
        } else {
            grk_mqc_encode(mqc, (uint32_t)v);
        }
        if (v) {
            v = *datap < 0 ? 1 : 0;
            *nmsedec +=	grk_t1_getnmsedec_sig((uint32_t)abs(*datap), (uint32_t)(bpno));
            grk_mqc_setcurctx(mqc, grk_t1_getctxno_sc(flag));	
            if (type == T1_TYPE_RAW) {	/* BYPASS/LAZY MODE */
                grk_mqc_bypass_enc(mqc, (uint32_t)v);
            } else {
                grk_mqc_encode(mqc, (uint32_t)(v ^ grk_t1_getspb((uint32_t)flag)));
            }
            grk_t1_updateflags(flagsp, (uint32_t)v, t1->flags_stride);
        }
        *flagsp |= T1_VISIT;
    }
}


static inline void grk_t1_dec_sigpass_step_raw( grk_t1_t *t1,
												opj_flag_t *flagsp,
												int32_t *datap,
												int32_t orient,
												int32_t oneplushalf,
												int32_t vsc)
{
    int32_t v, flag;
    grk_raw_t *raw = t1->raw;       
    OPJ_ARG_NOT_USED(orient);

    flag = vsc ? ((*flagsp) & (~(T1_SIG_S | T1_SIG_SE | T1_SIG_SW | T1_SGN_S))) : (*flagsp);
    if ((flag & T1_SIG_OTH) && !(flag & (T1_SIG))) {
        if (grk_raw_decode(raw)) {
            v = (int32_t)grk_raw_decode(raw);    
            *datap = v ? -oneplushalf : oneplushalf;
            grk_t1_updateflags(flagsp, (uint32_t)v, t1->flags_stride);
        }
        *flagsp |= T1_VISIT;
    }
}

static inline void grk_t1_dec_sigpass_step_mqc( grk_t1_t *t1,
												opj_flag_t *flagsp,
												int32_t *datap,
												int32_t orient,
												int32_t oneplushalf)
{
    int32_t flag;
	uint8_t v;

    grk_mqc_t *mqc = t1->mqc;       

    flag = *flagsp;
    if ((flag & T1_SIG_OTH) && !(flag & (T1_SIG))) {
        grk_mqc_setcurctx(mqc, grk_t1_getctxno_zc((uint32_t)flag, (uint32_t)orient));
        if (grk_mqc_decode(mqc)) {
            grk_mqc_setcurctx(mqc, grk_t1_getctxno_sc((uint32_t)flag));
            v = grk_mqc_decode(mqc) ^ grk_t1_getspb((uint32_t)flag);
            *datap = v ? -oneplushalf : oneplushalf;
            grk_t1_updateflags(flagsp, (uint32_t)v, t1->flags_stride);
        }
        *flagsp |= T1_VISIT;
    }
}      

static inline void grk_t1_dec_sigpass_step_mqc_vsc( grk_t1_t *t1,
													opj_flag_t *flagsp,
													int32_t *datap,
													int32_t orient,
													int32_t oneplushalf,
													int32_t vsc)
{
    int32_t flag;
	uint8_t v;

    grk_mqc_t *mqc = t1->mqc;     

    flag = vsc ? ((*flagsp) & (~(T1_SIG_S | T1_SIG_SE | T1_SIG_SW | T1_SGN_S))) : (*flagsp);
    if ((flag & T1_SIG_OTH) && !(flag & (T1_SIG))) {
        grk_mqc_setcurctx(mqc, grk_t1_getctxno_zc((uint32_t)flag, (uint32_t)orient));
        if (grk_mqc_decode(mqc)) {
            grk_mqc_setcurctx(mqc, grk_t1_getctxno_sc((uint32_t)flag));
            v = grk_mqc_decode(mqc) ^ grk_t1_getspb((uint32_t)flag);
            *datap = v ? -oneplushalf : oneplushalf;
            grk_t1_updateflags(flagsp, (uint32_t)v, t1->flags_stride);
        }
        *flagsp |= T1_VISIT;
    }
}                           



static void grk_t1_enc_sigpass(grk_t1_t *t1,
                               int32_t bpno,
                               uint32_t orient,
                               int32_t *nmsedec,
                               uint8_t type,
                               uint32_t cblksty
                              )
{
    uint32_t i, j, k, vsc;
    int32_t one;

    *nmsedec = 0;
    one = 1 << (bpno + T1_NMSEDEC_FRACBITS);
    for (k = 0; k < t1->h; k += 4) {
        for (i = 0; i < t1->w; ++i) {
            for (j = k; j < k + 4 && j < t1->h; ++j) {
                vsc = ((cblksty & J2K_CCP_CBLKSTY_VSC) && (j == k + 3 || j == t1->h - 1)) ? 1 : 0;
                grk_t1_enc_sigpass_step(
                    t1,
                    &t1->flags[((j+1) * t1->flags_stride) + i + 1],
                    &t1->data[(j * t1->data_stride) + i],
                    orient,
                    bpno,
                    one,
                    nmsedec,
                    type,
                    vsc);
            }
        }
    }
}

static void grk_t1_dec_sigpass_raw( grk_t1_t *t1,
									int32_t bpno,
									int32_t orient,
									int32_t cblksty)
{
    int32_t one, half, oneplushalf, vsc;
    uint32_t i, j, k;
    one = 1 << bpno;
    half = one >> 1;
    oneplushalf = one | half;
    for (k = 0; k < t1->h; k += 4) {
        for (i = 0; i < t1->w; ++i) {
            for (j = k; j < k + 4 && j < t1->h; ++j) {
                vsc = ((cblksty & J2K_CCP_CBLKSTY_VSC) && (j == k + 3 || j == t1->h - 1)) ? 1 : 0;
                grk_t1_dec_sigpass_step_raw(
                    t1,
                    &t1->flags[((j+1) * t1->flags_stride) + i + 1],
                    &t1->data[(j * t1->w) + i],
                    orient,
                    oneplushalf,
                    vsc);
            }
        }
    }
}                              

static void grk_t1_dec_sigpass_mqc(  grk_t1_t *t1,
									int32_t bpno,
									int32_t orient)
{
    int32_t one, half, oneplushalf;
    uint32_t i, j, k;
    int32_t *data1 = t1->data;
    opj_flag_t *flags1 = &t1->flags[1];
    one = 1 << bpno;
    half = one >> 1;
    oneplushalf = one | half;
    for (k = 0; k < (t1->h & ~3u); k += 4) {
        for (i = 0; i < t1->w; ++i) {
            int32_t *data2 = data1 + i;
            opj_flag_t *flags2 = flags1 + i;
            flags2 += t1->flags_stride;
            grk_t1_dec_sigpass_step_mqc(t1, flags2, data2, orient, oneplushalf);
            data2 += t1->w;
            flags2 += t1->flags_stride;
            grk_t1_dec_sigpass_step_mqc(t1, flags2, data2, orient, oneplushalf);
            data2 += t1->w;
            flags2 += t1->flags_stride;
            grk_t1_dec_sigpass_step_mqc(t1, flags2, data2, orient, oneplushalf);
            data2 += t1->w;
            flags2 += t1->flags_stride;
            grk_t1_dec_sigpass_step_mqc(t1, flags2, data2, orient, oneplushalf);
            data2 += t1->w;
        }
        data1 += (size_t)t1->w << 2;
        flags1 += (size_t)t1->flags_stride << 2;
    }
    for (i = 0; i < t1->w; ++i) {
        int32_t *data2 = data1 + i;
        opj_flag_t *flags2 = flags1 + i;
        for (j = k; j < t1->h; ++j) {
            flags2 += t1->flags_stride;
            grk_t1_dec_sigpass_step_mqc(t1, flags2, data2, orient, oneplushalf);
            data2 += t1->w;
        }
    }
}                              

static void grk_t1_dec_sigpass_mqc_vsc( grk_t1_t *t1,
										int32_t bpno,
										int32_t orient)
{
    int32_t one, half, oneplushalf, vsc;
    uint32_t i, j, k;
    one = 1 << bpno;
    half = one >> 1;
    oneplushalf = one | half;
    for (k = 0; k < t1->h; k += 4) {
        for (i = 0; i < t1->w; ++i) {
            for (j = k; j < k + 4 && j < t1->h; ++j) {
                vsc = (j == k + 3 || j == t1->h - 1) ? 1 : 0;
                grk_t1_dec_sigpass_step_mqc_vsc(
                    t1,
                    &t1->flags[((j+1) * t1->flags_stride) + i + 1],
                    &t1->data[(j * t1->w) + i],
                    orient,
                    oneplushalf,
                    vsc);
            }
        }
    }
}                          



static void grk_t1_enc_refpass_step(   grk_t1_t *t1,
                                       opj_flag_t *flagsp,
                                       int32_t *datap,
                                       int32_t bpno,
                                       int32_t one,
                                       int32_t *nmsedec,
                                       uint8_t type,
                                       uint32_t vsc)
{
    int32_t v;
    uint32_t flag;

    grk_mqc_t *mqc = t1->mqc;	

    flag = vsc ? (uint32_t)((*flagsp) & (~(T1_SIG_S | T1_SIG_SE | T1_SIG_SW | T1_SGN_S))) : (uint32_t)(*flagsp);
    if ((flag & (T1_SIG | T1_VISIT)) == T1_SIG) {
        *nmsedec += grk_t1_getnmsedec_ref((uint32_t)abs(*datap), (uint32_t)(bpno));
        v = (abs(*datap) & one) ? 1 : 0;
        grk_mqc_setcurctx(mqc, grk_t1_getctxno_mag(flag));	
        if (type == T1_TYPE_RAW) {	/* BYPASS/LAZY MODE */
            grk_mqc_bypass_enc(mqc, (uint32_t)v);
        } else {
            grk_mqc_encode(mqc, (uint32_t)v);
        }
        *flagsp |= T1_REFINE;
    }
}

static inline void grk_t1_dec_refpass_step_raw( grk_t1_t *t1,
												opj_flag_t *flagsp,
												int32_t *datap,
												int32_t poshalf,
												int32_t neghalf,
												int32_t vsc)
{
    int32_t v, t, flag;

    grk_raw_t *raw = t1->raw;       

    flag = vsc ? ((*flagsp) & (~(T1_SIG_S | T1_SIG_SE | T1_SIG_SW | T1_SGN_S))) : (*flagsp);
    if ((flag & (T1_SIG | T1_VISIT)) == T1_SIG) {
        v = (int32_t)grk_raw_decode(raw);
        t = v ? poshalf : neghalf;
        *datap += *datap < 0 ? -t : t;
        *flagsp |= T1_REFINE;
    }
}                               

static inline void grk_t1_dec_refpass_step_mqc( grk_t1_t *t1,
												opj_flag_t *flagsp,
												int32_t *datap,
												int32_t poshalf,
												int32_t neghalf)
{
    int32_t t, flag;
	uint8_t v;

    grk_mqc_t *mqc = t1->mqc;       

    flag = *flagsp;
    if ((flag & (T1_SIG | T1_VISIT)) == T1_SIG) {
        grk_mqc_setcurctx(mqc, grk_t1_getctxno_mag((uint32_t)flag));      
        v = grk_mqc_decode(mqc);
        t = v ? poshalf : neghalf;
        *datap += *datap < 0 ? -t : t;
        *flagsp |= T1_REFINE;
    }
}                               

static inline void grk_t1_dec_refpass_step_mqc_vsc( grk_t1_t *t1,
													opj_flag_t *flagsp,
													int32_t *datap,
													int32_t poshalf,
													int32_t neghalf,
													int32_t vsc)
{
    int32_t t, flag;
	uint8_t v;

    grk_mqc_t *mqc = t1->mqc;       

    flag = vsc ? ((*flagsp) & (~(T1_SIG_S | T1_SIG_SE | T1_SIG_SW | T1_SGN_S))) : (*flagsp);
    if ((flag & (T1_SIG | T1_VISIT)) == T1_SIG) {
        grk_mqc_setcurctx(mqc, grk_t1_getctxno_mag((uint32_t)flag));      
        v = grk_mqc_decode(mqc);
        t = v ? poshalf : neghalf;
        *datap += *datap < 0 ? -t : t;
        *flagsp |= T1_REFINE;
    }
}                               


static void grk_t1_enc_refpass( grk_t1_t *t1,
								int32_t bpno,
								int32_t *nmsedec,
								uint8_t type,
								uint32_t cblksty)
{
    uint32_t i, j, k, vsc;
    int32_t one;

    *nmsedec = 0;
    one = 1 << (bpno + T1_NMSEDEC_FRACBITS);
    for (k = 0; k < t1->h; k += 4) {
        for (i = 0; i < t1->w; ++i) {
            for (j = k; j < k + 4 && j < t1->h; ++j) {
                vsc = ((cblksty & J2K_CCP_CBLKSTY_VSC) && (j == k + 3 || j == t1->h - 1)) ? 1 : 0;
                grk_t1_enc_refpass_step(
                    t1,
                    &t1->flags[((j+1) * t1->flags_stride) + i + 1],
                    &t1->data[(j * t1->data_stride) + i],
                    bpno,
                    one,
                    nmsedec,
                    type,
                    vsc);
            }
        }
    }
}

static void grk_t1_dec_refpass_raw( grk_t1_t *t1,
									int32_t bpno,
									int32_t cblksty)
{
    int32_t one, poshalf, neghalf;
    uint32_t i, j, k;
    int32_t vsc;
    one = 1 << bpno;
    poshalf = one >> 1;
    neghalf = bpno > 0 ? -poshalf : -1;
    for (k = 0; k < t1->h; k += 4) {
        for (i = 0; i < t1->w; ++i) {
            for (j = k; j < k + 4 && j < t1->h; ++j) {
                vsc = ((cblksty & J2K_CCP_CBLKSTY_VSC) && (j == k + 3 || j == t1->h - 1)) ? 1 : 0;
                grk_t1_dec_refpass_step_raw(
                    t1,
                    &t1->flags[((j+1) * t1->flags_stride) + i + 1],
                    &t1->data[(j * t1->w) + i],
                    poshalf,
                    neghalf,
                    vsc);
            }
        }
    }
}                               

static void grk_t1_dec_refpass_mqc( grk_t1_t *t1,
									int32_t bpno){
    int32_t one, poshalf, neghalf;
    uint32_t i, j, k;
    int32_t *data1 = t1->data;
    opj_flag_t *flags1 = &t1->flags[1];
    one = 1 << bpno;
    poshalf = one >> 1;
    neghalf = bpno > 0 ? -poshalf : -1;
    for (k = 0; k < (t1->h & ~3u); k += 4) {
        for (i = 0; i < t1->w; ++i) {
            int32_t *data2 = data1 + i;
            opj_flag_t *flags2 = flags1 + i;
            flags2 += t1->flags_stride;
            grk_t1_dec_refpass_step_mqc(t1, flags2, data2, poshalf, neghalf);
            data2 += t1->w;
            flags2 += t1->flags_stride;
            grk_t1_dec_refpass_step_mqc(t1, flags2, data2, poshalf, neghalf);
            data2 += t1->w;
            flags2 += t1->flags_stride;
            grk_t1_dec_refpass_step_mqc(t1, flags2, data2, poshalf, neghalf);
            data2 += t1->w;
            flags2 += t1->flags_stride;
            grk_t1_dec_refpass_step_mqc(t1, flags2, data2, poshalf, neghalf);
            data2 += t1->w;
        }
        data1 += (size_t)t1->w << 2;
        flags1 += (size_t)t1->flags_stride << 2;
    }
    for (i = 0; i < t1->w; ++i) {
        int32_t *data2 = data1 + i;
        opj_flag_t *flags2 = flags1 + i;
        for (j = k; j < t1->h; ++j) {
            flags2 += t1->flags_stride;
            grk_t1_dec_refpass_step_mqc(t1, flags2, data2, poshalf, neghalf);
            data2 += t1->w;
        }
    }
}                               

static void grk_t1_dec_refpass_mqc_vsc(grk_t1_t *t1,
										int32_t bpno){
    int32_t one, poshalf, neghalf;
    uint32_t i, j, k;
    int32_t vsc;
    one = 1 << bpno;
    poshalf = one >> 1;
    neghalf = bpno > 0 ? -poshalf : -1;
    for (k = 0; k < t1->h; k += 4) {
        for (i = 0; i < t1->w; ++i) {
            for (j = k; j < k + 4 && j < t1->h; ++j) {
                vsc = ((j == k + 3 || j == t1->h - 1)) ? 1 : 0;
                grk_t1_dec_refpass_step_mqc_vsc(
                    t1,
                    &t1->flags[((j+1) * t1->flags_stride) + i + 1],
                    &t1->data[(j * t1->w) + i],
                    poshalf,
                    neghalf,
                    vsc);
            }
        }
    }
}                               


static void grk_t1_enc_clnpass_step(grk_t1_t *t1,
									opj_flag_t *flagsp,
									int32_t *datap,
									uint32_t orient,
									int32_t bpno,
									int32_t one,
									int32_t *nmsedec,
									uint32_t partial,
									uint32_t vsc){
    int32_t v;
    uint32_t flag;

    grk_mqc_t *mqc = t1->mqc;	

    flag = vsc ? (uint32_t)((*flagsp) & (~(T1_SIG_S | T1_SIG_SE | T1_SIG_SW | T1_SGN_S))) : (uint32_t)(*flagsp);
    if (partial) {
        goto LABEL_PARTIAL;
    }
    if (!(*flagsp & (T1_SIG | T1_VISIT))) {
        grk_mqc_setcurctx(mqc, grk_t1_getctxno_zc(flag, orient));
        v = (abs(*datap) & one) ? 1 : 0;
        grk_mqc_encode(mqc, (uint32_t)v);
        if (v) {
LABEL_PARTIAL:
            *nmsedec += grk_t1_getnmsedec_sig((uint32_t)abs(*datap), (uint32_t)(bpno));
            grk_mqc_setcurctx(mqc, grk_t1_getctxno_sc(flag));
            v = *datap < 0 ? 1 : 0;
            grk_mqc_encode(mqc, (uint32_t)(v ^ grk_t1_getspb((uint32_t)flag)));
            grk_t1_updateflags(flagsp, (uint32_t)v, t1->flags_stride);
        }
    }
    *flagsp &= ~T1_VISIT;
}

static void grk_t1_dec_clnpass_step_partial( grk_t1_t *t1,
											opj_flag_t *flagsp,
											int32_t *datap,
											int32_t orient,
											int32_t oneplushalf){
    int32_t flag;
	uint8_t v;
    grk_mqc_t *mqc = t1->mqc;	

    OPJ_ARG_NOT_USED(orient);

    flag = *flagsp;
    grk_mqc_setcurctx(mqc, grk_t1_getctxno_sc((uint32_t)flag));
    v = grk_mqc_decode(mqc) ^ grk_t1_getspb((uint32_t)flag);
    *datap = v ? -oneplushalf : oneplushalf;
    grk_t1_updateflags(flagsp, (uint32_t)v, t1->flags_stride);
    *flagsp &= ~T1_VISIT;
}				

static void grk_t1_dec_clnpass_step(grk_t1_t *t1,
									opj_flag_t *flagsp,
									int32_t *datap,
									int32_t orient,
									int32_t oneplushalf){
    int32_t flag;
	uint8_t v;

    grk_mqc_t *mqc = t1->mqc;	

    flag = *flagsp;
    if (!(flag & (T1_SIG | T1_VISIT))) {
        grk_mqc_setcurctx(mqc, grk_t1_getctxno_zc((uint32_t)flag, (uint32_t)orient));
        if (grk_mqc_decode(mqc)) {
            grk_mqc_setcurctx(mqc, grk_t1_getctxno_sc((uint32_t)flag));
            v = grk_mqc_decode(mqc) ^ grk_t1_getspb((uint32_t)flag);
            *datap = v ? -oneplushalf : oneplushalf;
            grk_t1_updateflags(flagsp, (uint32_t)v, t1->flags_stride);
        }
    }
    *flagsp &= ~T1_VISIT;
}				

static void grk_t1_dec_clnpass_step_vsc( grk_t1_t *t1,
										opj_flag_t *flagsp,
										int32_t *datap,
										int32_t orient,
										int32_t oneplushalf,
										int32_t partial,
										int32_t vsc){
    int32_t flag;
	uint8_t v;

    grk_mqc_t *mqc = t1->mqc;	

    flag = vsc ? ((*flagsp) & (~(T1_SIG_S | T1_SIG_SE | T1_SIG_SW | T1_SGN_S))) : (*flagsp);
    if (partial) {
        goto LABEL_PARTIAL;
    }
    if (!(flag & (T1_SIG | T1_VISIT))) {
        grk_mqc_setcurctx(mqc, grk_t1_getctxno_zc((uint32_t)flag, (uint32_t)orient));
        if (grk_mqc_decode(mqc)) {
LABEL_PARTIAL:
            grk_mqc_setcurctx(mqc, grk_t1_getctxno_sc((uint32_t)flag));
            v = grk_mqc_decode(mqc) ^ grk_t1_getspb((uint32_t)flag);
            *datap = v ? -oneplushalf : oneplushalf;
            grk_t1_updateflags(flagsp, (uint32_t)v, t1->flags_stride);
        }
    }
    *flagsp &= ~T1_VISIT;
}

static void grk_t1_enc_clnpass( grk_t1_t *t1,
								int32_t bpno,
								uint32_t orient,
								int32_t *nmsedec,
								uint32_t cblksty){
    uint32_t i, j, k;
    int32_t one;
    uint32_t agg, runlen, vsc;

    grk_mqc_t *mqc = t1->mqc;	

    *nmsedec = 0;
    one = 1 << (bpno + T1_NMSEDEC_FRACBITS);
    for (k = 0; k < t1->h; k += 4) {
        for (i = 0; i < t1->w; ++i) {
            if (k + 3 < t1->h) {
                if (cblksty & J2K_CCP_CBLKSTY_VSC) {
                    agg = !(MACRO_t1_flags(1 + k,1 + i) & (T1_SIG | T1_VISIT | T1_SIG_OTH)
                            || MACRO_t1_flags(1 + k + 1,1 + i) & (T1_SIG | T1_VISIT | T1_SIG_OTH)
                            || MACRO_t1_flags(1 + k + 2,1 + i) & (T1_SIG | T1_VISIT | T1_SIG_OTH)
                            || (MACRO_t1_flags(1 + k + 3,1 + i)
                                & (~(T1_SIG_S | T1_SIG_SE | T1_SIG_SW |	T1_SGN_S))) & (T1_SIG | T1_VISIT | T1_SIG_OTH));
                } else {
                    agg = !((MACRO_t1_flags(1 + k,1 + i) |
                             MACRO_t1_flags(1 + k + 1,1 + i) |
                             MACRO_t1_flags(1 + k + 2,1 + i) |
                             MACRO_t1_flags(1 + k + 3,1 + i)) & (T1_SIG | T1_VISIT | T1_SIG_OTH));
                }
            } else {
                agg = 0;
            }
            if (agg) {
                for (runlen = 0; runlen < 4; ++runlen) {
                    if (abs(t1->data[((k + runlen)*t1->data_stride) + i]) & one)
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
            for (j = k + runlen; j < k + 4 && j < t1->h; ++j) {
                vsc = ((cblksty & J2K_CCP_CBLKSTY_VSC) && (j == k + 3 || j == t1->h - 1)) ? 1 : 0;
                grk_t1_enc_clnpass_step(
                    t1,
                    &t1->flags[((j+1) * t1->flags_stride) + i + 1],
                    &t1->data[(j * t1->data_stride) + i],
                    orient,
                    bpno,
                    one,
                    nmsedec,
                    agg && (j == k + runlen),
                    vsc);
            }
        }
    }
}

static void grk_t1_dec_clnpass( grk_t1_t *t1,
								int32_t bpno,
								int32_t orient,
								int32_t cblksty){
    int32_t one, half, oneplushalf, agg, vsc;
	uint8_t runlen;
    uint32_t i, j, k;
    int32_t segsym = cblksty & J2K_CCP_CBLKSTY_SEGSYM;

    grk_mqc_t *mqc = t1->mqc;	

    one = 1 << bpno;
    half = one >> 1;
    oneplushalf = one | half;
    if (cblksty & J2K_CCP_CBLKSTY_VSC) {
        for (k = 0; k < t1->h; k += 4) {
            for (i = 0; i < t1->w; ++i) {
                if (k + 3 < t1->h) {
                    agg = !(MACRO_t1_flags(1 + k,1 + i) & (T1_SIG | T1_VISIT | T1_SIG_OTH)
                            || MACRO_t1_flags(1 + k + 1,1 + i) & (T1_SIG | T1_VISIT | T1_SIG_OTH)
                            || MACRO_t1_flags(1 + k + 2,1 + i) & (T1_SIG | T1_VISIT | T1_SIG_OTH)
                            || (MACRO_t1_flags(1 + k + 3,1 + i)
                                & (~(T1_SIG_S | T1_SIG_SE | T1_SIG_SW |	T1_SGN_S))) & (T1_SIG | T1_VISIT | T1_SIG_OTH));
                } else {
                    agg = 0;
                }
                if (agg) {
                    grk_mqc_setcurctx(mqc, T1_CTXNO_AGG);
                    if (!grk_mqc_decode(mqc)) {
                        continue;
                    }
                    grk_mqc_setcurctx(mqc, T1_CTXNO_UNI);
                    runlen = grk_mqc_decode(mqc);
                    runlen = (runlen << 1) | grk_mqc_decode(mqc);
                } else {
                    runlen = 0;
                }
                for (j = k + (uint32_t)runlen; j < k + 4 && j < t1->h; ++j) {
                    vsc = (j == k + 3 || j == t1->h - 1) ? 1 : 0;
                    grk_t1_dec_clnpass_step_vsc(
                        t1,
                        &t1->flags[((j+1) * t1->flags_stride) + i + 1],
                        &t1->data[(j * t1->w) + i],
                        orient,
                        oneplushalf,
                        agg && (j == k + (uint32_t)runlen),
                        vsc);
                }
            }
        }
    } else {
        int32_t *data1 = t1->data;
        opj_flag_t *flags1 = &t1->flags[1];
        for (k = 0; k < (t1->h & ~3u); k += 4) {
            for (i = 0; i < t1->w; ++i) {
                int32_t *data2 = data1 + i;
                opj_flag_t *flags2 = flags1 + i;
                agg = !((MACRO_t1_flags(1 + k, 1 + i) |
                         MACRO_t1_flags(1 + k + 1, 1 + i) |
                         MACRO_t1_flags(1 + k + 2, 1 + i) |
                         MACRO_t1_flags(1 + k + 3, 1 + i)) & (T1_SIG | T1_VISIT | T1_SIG_OTH));
                if (agg) {
                    grk_mqc_setcurctx(mqc, T1_CTXNO_AGG);
                    if (!grk_mqc_decode(mqc)) {
                        continue;
                    }
                    grk_mqc_setcurctx(mqc, T1_CTXNO_UNI);
                    runlen = grk_mqc_decode(mqc);
                    runlen = (runlen << 1) | grk_mqc_decode(mqc);
                    flags2 += (uint32_t)runlen * t1->flags_stride;
                    data2 += (uint32_t)runlen * t1->w;
                    for (j = (uint32_t)runlen; j < 4 && j < t1->h; ++j) {
                        flags2 += t1->flags_stride;
                        if (agg && (j == (uint32_t)runlen)) {
                            grk_t1_dec_clnpass_step_partial(t1, flags2, data2, orient, oneplushalf);
                        } else {
                            grk_t1_dec_clnpass_step(t1, flags2, data2, orient, oneplushalf);
                        }
                        data2 += t1->w;
                    }
                } else {
                    flags2 += t1->flags_stride;
                    grk_t1_dec_clnpass_step(t1, flags2, data2, orient, oneplushalf);
                    data2 += t1->w;
                    flags2 += t1->flags_stride;
                    grk_t1_dec_clnpass_step(t1, flags2, data2, orient, oneplushalf);
                    data2 += t1->w;
                    flags2 += t1->flags_stride;
                    grk_t1_dec_clnpass_step(t1, flags2, data2, orient, oneplushalf);
                    data2 += t1->w;
                    flags2 += t1->flags_stride;
                    grk_t1_dec_clnpass_step(t1, flags2, data2, orient, oneplushalf);
                    data2 += t1->w;
                }
            }
            data1 += (size_t)t1->w << 2;
            flags1 += (size_t)t1->flags_stride << 2;
        }
        for (i = 0; i < t1->w; ++i) {
            int32_t *data2 = data1 + i;
            opj_flag_t *flags2 = flags1 + i;
            for (j = k; j < t1->h; ++j) {
                flags2 += t1->flags_stride;
                grk_t1_dec_clnpass_step(t1, flags2, data2, orient, oneplushalf);
                data2 += t1->w;
            }
        }
    }

    if (segsym) {
        uint8_t v = 0;
        grk_mqc_setcurctx(mqc, T1_CTXNO_UNI);
        v = grk_mqc_decode(mqc);
        v = (v << 1) | grk_mqc_decode(mqc);
        v = (v << 1) | grk_mqc_decode(mqc);
        v = (v << 1) | grk_mqc_decode(mqc);
        /*
        if (v!=0xa) {
        	grk_event_msg(t1->cinfo, EVT_WARNING, "Bad segmentation symbol %x\n", v);
        }
        */
    }
}				

double grk_t1_getwmsedec(  int32_t nmsedec,
							uint32_t compno,
							uint32_t level,
							uint32_t orient,
							int32_t bpno,
							uint32_t qmfbid,
							double stepsize,
							uint32_t numcomps,
							const double * mct_norms,
							uint32_t mct_numcomps){
    double w1 = 1, w2, wmsedec;
    OPJ_ARG_NOT_USED(numcomps);

    if (mct_norms && (compno < mct_numcomps)) {
        w1 = mct_norms[compno];
    }

    if (qmfbid == 1) {
        w2 = grk_dwt_getnorm(level, orient);
    } else {	/* if (qmfbid == 0) */
        w2 = grk_dwt_getnorm_real(level, orient);
    }

    wmsedec = w1 * w2 * stepsize * ((size_t)1 << bpno);
    wmsedec *= wmsedec * nmsedec / 8192.0;

    return wmsedec;
}

bool grk_t1_allocate_buffers(grk_t1_t *t1,
							uint32_t w,
							uint32_t h){
    uint32_t datasize=w * h;
    uint32_t flagssize;

    /* encoder uses tile buffer, so no need to allocate */
    if (!t1->encoder) {
        if(datasize > t1->datasize) {
            grk_aligned_free(t1->data);
            t1->data = (int32_t*) grk_aligned_malloc(datasize * sizeof(int32_t));
            if(!t1->data) {
                /* FIXME event manager error callback */
                return false;
            }
            t1->datasize=datasize;
        }
		if (t1->data)
			memset(t1->data, 0, datasize * sizeof(int32_t));
    }


    t1->flags_stride=w+2;
    flagssize=t1->flags_stride * (h+2);

    if(flagssize > t1->flagssize) {
        grk_aligned_free(t1->flags);
        t1->flags = (opj_flag_t*) grk_aligned_malloc(flagssize * sizeof(opj_flag_t));
        if(!t1->flags) {
            /* FIXME event manager error callback */
            return false;
        }
        t1->flagssize=flagssize;
    }
    memset(t1->flags,0,flagssize * sizeof(opj_flag_t));

    t1->w=w;
    t1->h=h;

    return true;
}

/**
 * Creates a new Tier 1 handle
 * and initializes the look-up tables of the Tier-1 coder/decoder
 * @return a new T1 handle if successful, returns NULL otherwise
*/
grk_t1_t* grk_t1_create(bool isEncoder, uint16_t code_block_width, uint16_t code_block_height){
    grk_t1_t *l_t1 = nullptr;

    l_t1 = (grk_t1_t*) grk_calloc(1,sizeof(grk_t1_t));
    if (!l_t1) {
        return nullptr;
    }

    /* create MQC and RAW handles */
    l_t1->mqc = grk_mqc_create();
    if (! l_t1->mqc) {
        grk_t1_destroy(l_t1);
        return nullptr;
    }

    l_t1->raw = grk_raw_create();
    if (! l_t1->raw) {
        grk_t1_destroy(l_t1);
        return nullptr;
    }

    if (!isEncoder && code_block_width > 0 && code_block_height > 0) {
        l_t1->compressed_block = (uint8_t*)grk_malloc((size_t)code_block_width * (size_t)code_block_height);
        if (!l_t1->compressed_block) {
            grk_t1_destroy(l_t1);
            return nullptr;
        }
        l_t1->compressed_block_size = (size_t)(code_block_width * code_block_height);

    }
    l_t1->encoder = isEncoder;

    return l_t1;
}


/**
 * Destroys a previously created T1 handle
 *
 * @param p_t1 Tier 1 handle to destroy
*/
void grk_t1_destroy(grk_t1_t *p_t1){
    if (! p_t1) {
        return;
    }

    /* destroy MQC and RAW handles */
    grk_mqc_destroy(p_t1->mqc);
    p_t1->mqc = nullptr;
    grk_raw_destroy(p_t1->raw);
    p_t1->raw = nullptr;

    /* encoder uses tile buffer, so no need to free */
    if (!p_t1->encoder && p_t1->data) {
        grk_aligned_free(p_t1->data);
        p_t1->data = nullptr;
    }

    if (p_t1->flags) {
        grk_aligned_free(p_t1->flags);
        p_t1->flags = nullptr;
    }
    if (p_t1->compressed_block)
        grk_free(p_t1->compressed_block);
    grk_free(p_t1);
}

bool grk_t1_prepare_decode_cblks(  grk_tcd_tilecomp_t* tilec,
                           opj_tccp_t* tccp,
						   std::vector<decodeBlockInfo*>* blocks,
                           grk_event_mgr_t * p_manager)
{
    uint32_t resno, bandno, precno;
    bool rc = true;
    if (!grk_tile_buf_alloc_component_data_decode(tilec->buf)) {
        grk_event_msg(p_manager, EVT_ERROR, "Not enough memory for tile data\n");
        return false;
    }

    for (resno = 0; resno < tilec->minimum_num_resolutions; ++resno) {
        grk_tcd_resolution_t* res = &tilec->resolutions[resno];

        for (bandno = 0; bandno < res->numbands; ++bandno) {
            grk_tcd_band_t* restrict band = &res->bands[bandno];

            for (precno = 0; precno < res->pw * res->ph; ++precno) {
                grk_tcd_precinct_t* precinct = &band->precincts[precno];
                int32_t cblkno;
                    for (cblkno = 0; cblkno < (int32_t)(precinct->cw * precinct->ch); ++cblkno) {
                        rect_t cblk_rect;
                        grk_tcd_cblk_dec_t* cblk = &precinct->cblks.dec[cblkno];
                        int32_t x, y;		/* relative code block offset */
                        /* get code block offset relative to band*/
                        x = cblk->x0;
                        y = cblk->y0;

                        /* check if block overlaps with decode region */
						cblk_rect= rect_t(x, y, x + (1<< tccp->cblkw), y + (1<<tccp->cblkh));


                        if (!grk_tile_buf_hit_test(tilec->buf, &cblk_rect))
                            continue;


                        x -= band->x0;
                        y -= band->y0;

                        /* add band offset relative to previous resolution */
                        if (band->bandno & 1) {
                            grk_tcd_resolution_t* pres = &tilec->resolutions[resno - 1];
                            x += pres->x1 - pres->x0;
                        }
                        if (band->bandno & 2) {
                            grk_tcd_resolution_t* pres = &tilec->resolutions[resno - 1];
                            y += pres->y1 - pres->y0;
                        }

						auto block = new decodeBlockInfo();
						block->bandno = band->bandno;
						block->cblk = cblk;
						block->cblksty = tccp->cblksty;
						block->qmfbid = tccp->qmfbid;
						block->resno = resno;
						block->roishift = tccp->roishift;
						block->stepsize = band->stepsize;
						block->tilec = tilec;
						block->x = x;
						block->y = y;
						block->tiledp = grk_tile_buf_get_ptr(tilec->buf, resno, bandno, (uint32_t)x, (uint32_t)y);
						blocks->push_back(block);

                    } /* cblkno */
            } /* precno */
        } /* bandno */
    } /* resno */
    return rc;
}


bool grk_t1_decode_cblk(grk_t1_t *t1,
                               grk_tcd_cblk_dec_t* cblk,
                               uint32_t orient,
                               uint32_t roishift,
                               uint32_t cblksty)
{
    grk_raw_t *raw = t1->raw;	
    grk_mqc_t *mqc = t1->mqc;	

    int32_t bpno_plus_one;
    uint32_t passtype;
    uint32_t segno, passno;
    uint8_t type = T1_TYPE_MQ; /* BYPASS mode */
    uint8_t* block_buffer = NULL;
    size_t total_seg_len;

    if(!grk_t1_allocate_buffers(
                t1,
                cblk->x1 - cblk->x0,
                cblk->y1 - cblk->y0)) {
        return false;
    }

    total_seg_len = grk_min_buf_vec_get_len(&cblk->seg_buffers);
    if (cblk->numSegments && total_seg_len) {
        /* if there is only one segment, then it is already contiguous, so no need to make a copy*/
        if (total_seg_len == 1 && cblk->seg_buffers.get(0)) {
            block_buffer = ((grk_buf_t*)(cblk->seg_buffers.get(0)))->buf;
        } else {
            /* block should have been allocated on creation of t1*/
            if (!t1->compressed_block)
                return false;
            if (t1->compressed_block_size < total_seg_len) {
                uint8_t* new_block = (uint8_t*)grk_realloc(t1->compressed_block, total_seg_len);
                if (!new_block)
                    return false;
                t1->compressed_block = new_block;
                t1->compressed_block_size = total_seg_len;
            }
            grk_min_buf_vec_copy_to_contiguous_buffer(&cblk->seg_buffers, t1->compressed_block);
            block_buffer = t1->compressed_block;
        }
    } else {
        return true;
    }



    bpno_plus_one = (int32_t)(roishift + cblk->numbps);
    passtype = 2;

    grk_mqc_resetstates(mqc);
    for (segno = 0; segno < cblk->numSegments; ++segno) {
        grk_tcd_seg_t *seg = &cblk->segs[segno];

        /* BYPASS mode */
        type = ((bpno_plus_one <= ((int32_t) (cblk->numbps)) - 4) && (passtype < 2) && (cblksty & J2K_CCP_CBLKSTY_LAZY)) ? T1_TYPE_RAW : T1_TYPE_MQ;
        if (type == T1_TYPE_RAW) {
            grk_raw_init_dec(raw, block_buffer + seg->dataindex, seg->len);
		}
		else {
			grk_mqc_init_dec(mqc, block_buffer + seg->dataindex, seg->len);
		}

        for (passno = 0; (passno < seg->numpasses) && (bpno_plus_one >= 1); ++passno) {
            switch (passtype) {
            case 0:
                if (type == T1_TYPE_RAW) {
                    grk_t1_dec_sigpass_raw(t1, bpno_plus_one, (int32_t)orient, (int32_t)cblksty);
                } else {
                    if (cblksty & J2K_CCP_CBLKSTY_VSC) {
                        grk_t1_dec_sigpass_mqc_vsc(t1, bpno_plus_one, (int32_t)orient);
                    } else {
                        grk_t1_dec_sigpass_mqc(t1, bpno_plus_one, (int32_t)orient);
                    }
                }
                break;
            case 1:
                if (type == T1_TYPE_RAW) {
                    grk_t1_dec_refpass_raw(t1, bpno_plus_one, (int32_t)cblksty);
                } else {
                    if (cblksty & J2K_CCP_CBLKSTY_VSC) {
                        grk_t1_dec_refpass_mqc_vsc(t1, bpno_plus_one);
                    } else {
                        grk_t1_dec_refpass_mqc(t1, bpno_plus_one);
                    }
                }
                break;
            case 2:
                grk_t1_dec_clnpass(t1, bpno_plus_one, (int32_t)orient, (int32_t)cblksty);
                break;
            }

            if ((cblksty & J2K_CCP_CBLKSTY_RESET) && type == T1_TYPE_MQ) {
                grk_mqc_resetstates(mqc);
            }
            if (++passtype == 3) {
                passtype = 0;
                bpno_plus_one--;
            }
        }
    }
    return true;
}




bool grk_t1_encode_cblks(   grk_tcd_tile_t *tile,
                            grk_tcp_t *tcp,
                            const double * mct_norms,
                            uint32_t mct_numcomps,
							uint32_t numThreads )
{
    bool do_opt = true;
    uint32_t compno, resno, bandno, precno;
    tile->distotile = 0;		
    for (compno = 0; compno < tile->numcomps; ++compno) {
        opj_tccp_t* tccp = tcp->tccps + compno;
        if (tccp->cblksty != 0 && 
				tccp->cblksty != J2K_CCP_CBLKSTY_RESET && 
					tccp->cblksty != J2K_CCP_CBLKSTY_TERMALL) {
            do_opt = false;
            break;
        }
    }

	std::vector<encodeBlockInfo*> blocks;
	auto maxCblkW = 0;
	auto maxCblkH = 0;

    for (compno = 0; compno < tile->numcomps; ++compno) {
        grk_tcd_tilecomp_t* tilec = &tile->comps[compno];
        opj_tccp_t* tccp = &tcp->tccps[compno];
        for (resno = 0; resno < tilec->numresolutions; ++resno) {
            grk_tcd_resolution_t *res = &tilec->resolutions[resno];

            for (bandno = 0; bandno < res->numbands; ++bandno) {
                grk_tcd_band_t* restrict band = &res->bands[bandno];
                int32_t bandconst = 8192 * 8192 / ((int32_t) floor(band->stepsize * 8192));

                for (precno = 0; precno < res->pw * res->ph; ++precno) {
                    grk_tcd_precinct_t *prc = &band->precincts[precno];
                    int32_t cblkno;
                    int32_t bandOdd = band->bandno & 1;
                    int32_t bandModTwo = band->bandno & 2;

                    for (cblkno = 0; cblkno < (int32_t)(prc->cw * prc->ch); ++cblkno) {
                        grk_tcd_cblk_enc_t* cblk = prc->cblks.enc + cblkno;
                        int32_t x = cblk->x0 - band->x0;
                        int32_t y = cblk->y0 - band->y0;
                        if (bandOdd) {
                            grk_tcd_resolution_t *pres = &tilec->resolutions[resno - 1];
                            x += pres->x1 - pres->x0;
                        }
                        if (bandModTwo) {
                            grk_tcd_resolution_t *pres = &tilec->resolutions[resno - 1];
                            y += pres->y1 - pres->y0;
                        }

						maxCblkW = grk_max<int32_t>(maxCblkW, 1 << tccp->cblkw);
						maxCblkH = grk_max<int32_t>(maxCblkH, 1 << tccp->cblkh);
						auto block = new encodeBlockInfo();
						block->compno = compno;
						block->bandno = band->bandno;
						block->cblk = cblk;
						block->cblksty = tccp->cblksty;
						block->qmfbid = tccp->qmfbid;
						block->resno = resno;
						block->bandconst = bandconst;
						block->stepsize = band->stepsize;
						block->x = x;
						block->y = y;
						block->mct_norms = mct_norms;
						block->mct_numcomps = mct_numcomps;
						block->tiledp = grk_tile_buf_get_ptr(tilec->buf, resno, bandno, (uint32_t)x, (uint32_t)y);
						blocks.push_back(block);
					
                    } /* cblkno */
                } /* precno */
            } /* bandno */
        } /* resno  */
    } /* compno  */

	T1Encoder encoder;
	return encoder.encode(do_opt,
							tile,
							&blocks,
							maxCblkW,
							maxCblkH,
							numThreads);
	
}

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
                            uint32_t mct_numcomps)
{
    double cumwmsedec = 0.0;

    grk_mqc_t *mqc = t1->mqc;	

    uint32_t passno;
    int32_t bpno;
    uint32_t passtype;
    int32_t nmsedec = 0;
    int32_t max;
    uint32_t i, j;
    uint8_t type = T1_TYPE_MQ;
    double tempwmsedec;

    max = 0;
    for (i = 0; i < t1->w; ++i) {
        for (j = 0; j < t1->h; ++j) {
            int32_t tmp = abs(t1->data[i + j*t1->data_stride]);
            max = grk_max<int32_t>(max, tmp);
        }
    }

	auto logMax = grk_int_floorlog2((int32_t)max) + 1;
	cblk->numbps = (max && (logMax > T1_NMSEDEC_FRACBITS)) ? (uint32_t)(logMax - T1_NMSEDEC_FRACBITS) : 0;
	if (!cblk->numbps)
		return 0;

    bpno = (int32_t)(cblk->numbps - 1);
    passtype = 2;
    grk_mqc_init_enc(mqc, cblk->data);
	uint32_t state = opj_plugin_get_debug_state();
	if (state & OPJ_PLUGIN_STATE_DEBUG) {
		mqc->debug_mqc.contextStream = cblk->contextStream;
	}

	bool TERMALL = (cblksty & J2K_CCP_CBLKSTY_TERMALL) ? true : false;
	bool LAZY = (cblksty & J2K_CCP_CBLKSTY_LAZY);

    for (passno = 0; bpno >= 0; ++passno) {
        grk_tcd_pass_t *pass = &cblk->passes[passno];
        uint32_t correction = 3;
		type = T1_TYPE_MQ;
		if (LAZY && (bpno < ((int32_t)(cblk->numbps) - 4)) && (passtype < 2))
			type = T1_TYPE_RAW;

        switch (passtype) {
        case 0:
            grk_t1_enc_sigpass(t1, bpno, orient, &nmsedec, type, cblksty);
            break;
        case 1:
            grk_t1_enc_refpass(t1, bpno, &nmsedec, type, cblksty);
            break;
        case 2:
            grk_t1_enc_clnpass(t1, bpno, orient, &nmsedec, cblksty);
            /* code switch SEGMARK (i.e. SEGSYM) */
            if (cblksty & J2K_CCP_CBLKSTY_SEGSYM)
                grk_mqc_segmark_enc(mqc);
			if (state & OPJ_PLUGIN_STATE_DEBUG) {
				mqc_next_plane(&mqc->debug_mqc);
			}
            break;
        }

        tempwmsedec = grk_t1_getwmsedec(nmsedec, compno, level, orient, bpno, qmfbid, stepsize, numcomps,mct_norms, mct_numcomps) ;
        cumwmsedec += tempwmsedec;

		// In LAZY mode, we need to terminate pass 2 from fourth bit plane, 
		// and passes 1 and 2 from subsequent bit planes. Pass 0 in lazy region
		// does not get terminated unless TERMALL is also set
        if ( TERMALL ||
				(LAZY && ((bpno < ((int32_t)(cblk->numbps) - 4) && (passtype > 0)) ||
							 ((bpno == ((int32_t)cblk->numbps - 4)) && (passtype == 2))))) {

			correction = 0;
			auto bypassFlush = false;
			if (LAZY) {
				if (TERMALL) {
					bypassFlush = (bpno < ((int32_t)(cblk->numbps) - 4)) && (passtype < 2);
				}
				else {
					bypassFlush = passtype == 1;
				}
			}
			if (bypassFlush) {
				grk_mqc_bypass_flush_enc(mqc);
			}
			else
				grk_mqc_flush(mqc);
            pass->term = 1;
        } else {
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

		//note: passtype and bpno have already been updated to next pass,
		// while pass pointer still points to current pass
		if (bpno >= 0) {
			if (pass->term) {
				type = T1_TYPE_MQ;
				if (LAZY && (bpno < ((int32_t)(cblk->numbps) - 4)) && (passtype < 2))
					type = T1_TYPE_RAW;
				if (type == T1_TYPE_RAW) 
					grk_mqc_bypass_init_enc(mqc);
				else
					grk_mqc_restart_init_enc(mqc);
			}

			/* Code-switch "RESET" */
			if (cblksty & J2K_CCP_CBLKSTY_RESET)
				grk_mqc_resetstates(mqc);
		}
    }

    /* Code switch "ERTERM" (i.e. PTERM) */
	grk_tcd_pass_t *finalPass = &cblk->passes[passno - 1];
    if (cblksty & J2K_CCP_CBLKSTY_PTERM)
        grk_mqc_erterm_enc(mqc);
    else if (!finalPass->term)
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
			if (pass->rate > 0 && (cblk->data[pass->rate - 1] == 0xFF)) {
				pass->rate--;
			}
		}
        pass->len = pass->rate - (passno == 0 ? 0 : cblk->passes[passno - 1].rate);
    }

	return cumwmsedec;
}
