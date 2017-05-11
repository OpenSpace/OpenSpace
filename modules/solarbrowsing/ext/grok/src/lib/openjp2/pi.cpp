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
 * Copyright (c) 2006-2007, Parvatha Elangovan
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

/** @defgroup PI PI - Implementation of a packet iterator */
/*@{*/

/** @name Local static functions */
/*@{*/

/**
Get next packet in layer-resolution-component-precinct order.
@param pi packet iterator to modify
@return returns false if pi pointed to the last packet or else returns true
*/
static bool grk_pi_next_lrcp(grk_pi_iterator_t * pi);
/**
Get next packet in resolution-layer-component-precinct order.
@param pi packet iterator to modify
@return returns false if pi pointed to the last packet or else returns true
*/
static bool grk_pi_next_rlcp(grk_pi_iterator_t * pi);
/**
Get next packet in resolution-precinct-component-layer order.
@param pi packet iterator to modify
@return returns false if pi pointed to the last packet or else returns true
*/
static bool grk_pi_next_rpcl(grk_pi_iterator_t * pi);
/**
Get next packet in precinct-component-resolution-layer order.
@param pi packet iterator to modify
@return returns false if pi pointed to the last packet or else returns true
*/
static bool grk_pi_next_pcrl(grk_pi_iterator_t * pi);
/**
Get next packet in component-precinct-resolution-layer order.
@param pi packet iterator to modify
@return returns false if pi pointed to the last packet or else returns true
*/
static bool grk_pi_next_cprl(grk_pi_iterator_t * pi);

/**
 * Updates the coding parameters if the encoding is used with Progression order changes and final (or cinema parameters are used).
 *
 * @param	p_cp		the coding parameters to modify
 * @param	p_tileno	the tile index being concerned.
 * @param	p_tx0		X0 parameter for the tile
 * @param	p_tx1		X1 parameter for the tile
 * @param	p_ty0		Y0 parameter for the tile
 * @param	p_ty1		Y1 parameter for the tile
 * @param	p_max_prec	the maximum precision for all the bands of the tile
 * @param	p_max_res	the maximum number of resolutions for all the poc inside the tile.
 * @param	p_dx_min		the minimum dx of all the components of all the resolutions for the tile.
 * @param	p_dy_min		the minimum dy of all the components of all the resolutions for the tile.
 */
static void grk_pi_update_encode_poc_and_final ( opj_cp_t *p_cp,
        uint32_t p_tileno,
        uint32_t p_tx0,
        uint32_t p_tx1,
        uint32_t p_ty0,
        uint32_t p_ty1,
        uint32_t p_max_prec,
        uint32_t p_max_res,
        uint32_t p_dx_min,
        uint32_t p_dy_min);

/**
 * Updates the coding parameters if the encoding is not used with Progression order changes and final (and cinema parameters are used).
 *
 * @param	p_cp		the coding parameters to modify
 * @param	p_num_comps		the number of components
 * @param	p_tileno	the tile index being concerned.
 * @param	p_tx0		X0 parameter for the tile
 * @param	p_tx1		X1 parameter for the tile
 * @param	p_ty0		Y0 parameter for the tile
 * @param	p_ty1		Y1 parameter for the tile
 * @param	p_max_prec	the maximum precision for all the bands of the tile
 * @param	p_max_res	the maximum number of resolutions for all the poc inside the tile.
 * @param	p_dx_min		the minimum dx of all the components of all the resolutions for the tile.
 * @param	p_dy_min		the minimum dy of all the components of all the resolutions for the tile.
 */
static void grk_pi_update_encode_not_poc (  opj_cp_t *p_cp,
        uint32_t p_num_comps,
        uint32_t p_tileno,
        uint32_t p_tx0,
        uint32_t p_tx1,
        uint32_t p_ty0,
        uint32_t p_ty1,
        uint32_t p_max_prec,
        uint32_t p_max_res,
        uint32_t p_dx_min,
        uint32_t p_dy_min);
/**
 * Gets the encoding parameters needed to update the coding parameters and all the pocs.
 *
 * @param	p_image			the image being encoded.
 * @param	p_cp			the coding parameters.
 * @param	tileno			the tile index of the tile being encoded.
 * @param	p_tx0			pointer that will hold the X0 parameter for the tile
 * @param	p_tx1			pointer that will hold the X1 parameter for the tile
 * @param	p_ty0			pointer that will hold the Y0 parameter for the tile
 * @param	p_ty1			pointer that will hold the Y1 parameter for the tile
 * @param	p_max_prec		pointer that will hold the maximum precision for all the bands of the tile
 * @param	p_max_res		pointer that will hold the maximum number of resolutions for all the poc inside the tile.
 * @param	p_dx_min			pointer that will hold the minimum dx of all the components of all the resolutions for the tile.
 * @param	p_dy_min			pointer that will hold the minimum dy of all the components of all the resolutions for the tile.
 */
static void opj_get_encoding_parameters(const opj_image_t *p_image,
                                        const opj_cp_t *p_cp,
                                        uint32_t  tileno,
                                        uint32_t  * p_tx0,
                                        uint32_t * p_tx1,
                                        uint32_t * p_ty0,
                                        uint32_t * p_ty1,
                                        uint32_t * p_dx_min,
                                        uint32_t * p_dy_min,
                                        uint32_t * p_max_prec,
                                        uint32_t * p_max_res );

/**
 * Gets the encoding parameters needed to update the coding parameters and all the pocs.
 * The precinct widths, heights, dx and dy for each component at each resolution will be stored as well.
 * the last parameter of the function should be an array of pointers of size nb components, each pointer leading
 * to an area of size 4 * max_res. The data is stored inside this area with the following pattern :
 * dx_compi_res0 , dy_compi_res0 , w_compi_res0, h_compi_res0 , dx_compi_res1 , dy_compi_res1 , w_compi_res1, h_compi_res1 , ...
 *
 * @param	p_image			the image being encoded.
 * @param	p_cp			the coding parameters.
 * @param	tileno			the tile index of the tile being encoded.
 * @param	p_tx0			pointer that will hold the X0 parameter for the tile
 * @param	p_tx1			pointer that will hold the X1 parameter for the tile
 * @param	p_ty0			pointer that will hold the Y0 parameter for the tile
 * @param	p_ty1			pointer that will hold the Y1 parameter for the tile
 * @param	p_max_prec		pointer that will hold the maximum precision for all the bands of the tile
 * @param	p_max_res		pointer that will hold the maximum number of resolutions for all the poc inside the tile.
 * @param	p_dx_min		pointer that will hold the minimum dx of all the components of all the resolutions for the tile.
 * @param	p_dy_min		pointer that will hold the minimum dy of all the components of all the resolutions for the tile.
 * @param	p_resolutions	pointer to an area corresponding to the one described above.
 */
static void opj_get_all_encoding_parameters(const opj_image_t *p_image,
        const opj_cp_t *p_cp,
        uint32_t tileno,
        uint32_t * p_tx0,
        uint32_t * p_tx1,
        uint32_t * p_ty0,
        uint32_t * p_ty1,
        uint32_t * p_dx_min,
        uint32_t * p_dy_min,
        uint32_t * p_max_prec,
        uint32_t * p_max_res,
        uint32_t ** p_resolutions );
/**
 * Allocates memory for a packet iterator. Data and data sizes are set by this operation.
 * No other data is set. The include section of the packet  iterator is not allocated.
 *
 * @param	p_image		the image used to initialize the packet iterator (in fact only the number of components is relevant.
 * @param	p_cp		the coding parameters.
 * @param	tileno	the index of the tile from which creating the packet iterator.
 */
static grk_pi_iterator_t * grk_pi_create(	const opj_image_t *p_image,
        const opj_cp_t *p_cp,
        uint32_t tileno );
/**
 * FIXME DOC
 */
static void grk_pi_update_decode_not_poc (grk_pi_iterator_t * p_pi,
        grk_tcp_t * p_tcp,
        uint32_t p_max_precision,
        uint32_t p_max_res);
/**
 * FIXME DOC
 */
static void grk_pi_update_decode_poc (  grk_pi_iterator_t * p_pi,
                                        grk_tcp_t * p_tcp,
                                        uint32_t p_max_precision,
                                        uint32_t p_max_res);

/**
 * FIXME DOC
 */
static bool grk_pi_check_next_level(	int32_t pos,
                                        opj_cp_t *cp,
                                        uint32_t tileno,
                                        uint32_t pino,
                                        const char *prog);

static void update_pi_dxy(grk_pi_iterator_t * pi);
static void update_pi_dxy_for_comp(grk_pi_iterator_t * pi, grk_pi_comp_t *comp);


/*@}*/

/*@}*/

/*
==========================================================
   local functions
==========================================================
*/
static void update_pi_dxy_for_comp(grk_pi_iterator_t * pi, grk_pi_comp_t *comp) {
	for (uint32_t resno = 0; resno < comp->numresolutions; resno++) {
		grk_pi_resolution_t* res = &comp->resolutions[resno];
		uint64_t dx = comp->dx * ((uint64_t)1u << (res->pdx + comp->numresolutions - 1 - resno));
		uint64_t dy = comp->dy * ((uint64_t)1u << (res->pdy + comp->numresolutions - 1 - resno));
		if (dx < UINT_MAX) {
			pi->dx = !pi->dx ? (uint32_t)dx : grk_min<uint32_t>(pi->dx, (uint32_t)dx);
		}
		if (dy < UINT_MAX) {
			pi->dy = !pi->dy ? (uint32_t)dy : grk_min<uint32_t>(pi->dy, (uint32_t)dy);
		}
	}
}
static void update_pi_dxy(grk_pi_iterator_t * pi) {
	pi->first = 0;
	pi->dx = 0;
	pi->dy = 0;
	for (uint32_t compno = 0; compno < pi->numcomps; compno++) {
		update_pi_dxy_for_comp(pi, pi->comps+compno);
	}
}

static bool grk_pi_next_lrcp(grk_pi_iterator_t * pi)
{
    grk_pi_comp_t *comp = NULL;
    grk_pi_resolution_t *res = NULL;
    uint32_t index = 0;

    if (!pi->first) {
        comp = &pi->comps[pi->compno];
        res = &comp->resolutions[pi->resno];
        goto LABEL_SKIP;
    } 

    pi->first = 0;
    for (pi->layno = pi->poc.layno0; pi->layno < pi->poc.layno1; pi->layno++) {
        for (pi->resno = pi->poc.resno0; pi->resno < pi->poc.resno1;
                pi->resno++) {
            for (pi->compno = pi->poc.compno0; pi->compno < pi->poc.compno1; pi->compno++) {
                comp = &pi->comps[pi->compno];
                if (pi->resno >= comp->numresolutions) {
                    continue;
                }
                res = &comp->resolutions[pi->resno];
                if (!pi->tp_on) {
                    pi->poc.precno1 = res->pw * res->ph;
                }
                for (pi->precno = pi->poc.precno0; pi->precno < pi->poc.precno1; pi->precno++) {
                    index = pi->layno * pi->step_l + pi->resno * pi->step_r + pi->compno * pi->step_c + pi->precno * pi->step_p;
                    if (!pi->include[index]) {
                        pi->include[index] = 1;
                        return true;
                    }
LABEL_SKIP:
                    ;
                }
            }
        }
    }

    return false;
}

static bool grk_pi_next_rlcp(grk_pi_iterator_t * pi)
{
    grk_pi_comp_t *comp = NULL;
    grk_pi_resolution_t *res = NULL;
    uint32_t index = 0;

    if (!pi->first) {
        comp = &pi->comps[pi->compno];
        res = &comp->resolutions[pi->resno];
        goto LABEL_SKIP;
    } 

    pi->first = 0;
    for (pi->resno = pi->poc.resno0; pi->resno < pi->poc.resno1; pi->resno++) {
        for (pi->layno = pi->poc.layno0; pi->layno < pi->poc.layno1; pi->layno++) {
            for (pi->compno = pi->poc.compno0; pi->compno < pi->poc.compno1; pi->compno++) {
                comp = &pi->comps[pi->compno];
                if (pi->resno >= comp->numresolutions) {
                    continue;
                }
                res = &comp->resolutions[pi->resno];
                if(!pi->tp_on) {
                    pi->poc.precno1 = res->pw * res->ph;
                }
                for (pi->precno = pi->poc.precno0; pi->precno < pi->poc.precno1; pi->precno++) {
                    index = pi->layno * pi->step_l + pi->resno * pi->step_r + pi->compno * pi->step_c + pi->precno * pi->step_p;
                    if (!pi->include[index]) {
                        pi->include[index] = 1;
                        return true;
                    }
LABEL_SKIP:
                    ;
                }
            }
        }
    }
    return false;
}


static bool grk_pi_next_rpcl(grk_pi_iterator_t * pi)
{
    grk_pi_comp_t *comp = NULL;
    grk_pi_resolution_t *res = NULL;
    uint32_t index = 0;

    if (!pi->first) {
        goto LABEL_SKIP;
    } else {
		update_pi_dxy(pi);
    }
    if (!pi->tp_on) {
        pi->poc.ty0 = pi->ty0;
        pi->poc.tx0 = pi->tx0;
        pi->poc.ty1 = pi->ty1;
        pi->poc.tx1 = pi->tx1;
    }
    for (pi->resno = pi->poc.resno0; pi->resno < pi->poc.resno1; pi->resno++) {
        for (pi->y = pi->poc.ty0; pi->y < pi->poc.ty1; pi->y += pi->dy - (pi->y % pi->dy)) {
            for (pi->x = pi->poc.tx0; pi->x < pi->poc.tx1; pi->x += pi->dx - (pi->x % pi->dx)) {
                for (pi->compno = pi->poc.compno0; pi->compno < pi->poc.compno1; pi->compno++) {
                    uint32_t levelno;
                    uint32_t trx0, try0;
                    uint32_t  trx1, try1;
                    uint32_t  rpx, rpy;
                    uint32_t  prci, prcj;
                    comp = &pi->comps[pi->compno];
                    if (pi->resno >= comp->numresolutions) {
                        continue;
                    }
                    res = &comp->resolutions[pi->resno];
                    levelno = comp->numresolutions - 1 - pi->resno;
					if (levelno >= OPJ_J2K_MAXRLVLS)
						continue;
                    trx0 = grk_uint64_ceildiv((uint64_t)pi->tx0, ((uint64_t)comp->dx << levelno));
                    try0 = grk_uint64_ceildiv((uint64_t)pi->ty0, ((uint64_t)comp->dy << levelno));
                    trx1 = grk_uint64_ceildiv((uint64_t)pi->tx1, ((uint64_t)comp->dx << levelno));
                    try1 = grk_uint64_ceildiv((uint64_t)pi->ty1, ((uint64_t)comp->dy << levelno));
                    rpx = res->pdx + levelno;
                    rpy = res->pdy + levelno;
                    if (!(((uint64_t)pi->y % ((uint64_t)comp->dy << rpy) == 0) || ((pi->y == pi->ty0) && (((uint64_t)try0 << levelno) % ((uint64_t)1 << rpy))  ))) {
                        continue;
                    }
                    if (!(((uint64_t)pi->x % ((uint64_t)comp->dx << rpx) == 0) || ((pi->x == pi->tx0) && (((uint64_t)trx0 << levelno) % ((uint64_t)1 << rpx))))) {
                        continue;
                    }

                    if ((res->pw==0)||(res->ph==0)) continue;

                    if ((trx0==trx1)||(try0==try1)) continue;

                    prci = grk_uint_floordivpow2(grk_uint64_ceildiv((uint64_t)pi->x, ((uint64_t)comp->dx << levelno)), res->pdx)
                           - grk_uint_floordivpow2(trx0, res->pdx);
                    prcj = grk_uint_floordivpow2(grk_uint64_ceildiv((uint64_t)pi->y, ((uint64_t)comp->dy << levelno)), res->pdy)
                           - grk_uint_floordivpow2(try0, res->pdy);
                    pi->precno = (prci + prcj * res->pw);
                    for (pi->layno = pi->poc.layno0; pi->layno < pi->poc.layno1; pi->layno++) {
                        index = pi->layno * pi->step_l + pi->resno * pi->step_r + pi->compno * pi->step_c + pi->precno * pi->step_p;
                        if (!pi->include[index]) {
                            pi->include[index] = 1;
                            return true;
                        }
LABEL_SKIP:
                        ;
                    }
                }
            }
        }
    }

    return false;
}

static bool grk_pi_next_pcrl(grk_pi_iterator_t * pi)
{
    grk_pi_comp_t *comp = NULL;
    grk_pi_resolution_t *res = NULL;
    uint32_t index = 0;

    if (!pi->first) {
        comp = &pi->comps[pi->compno];
        goto LABEL_SKIP;
    } 

	update_pi_dxy(pi);
    if (!pi->tp_on) {
        pi->poc.ty0 = pi->ty0;
        pi->poc.tx0 = pi->tx0;
        pi->poc.ty1 = pi->ty1;
        pi->poc.tx1 = pi->tx1;
    }
    for (pi->y = pi->poc.ty0; pi->y < pi->poc.ty1; pi->y += pi->dy - (pi->y % pi->dy)) {
        for (pi->x = pi->poc.tx0; pi->x < pi->poc.tx1; pi->x += pi->dx - (pi->x % pi->dx)) {
            for (pi->compno = pi->poc.compno0; pi->compno < pi->poc.compno1; pi->compno++) {
                comp = &pi->comps[pi->compno];
                for (pi->resno = pi->poc.resno0; pi->resno < grk_min<uint32_t>(pi->poc.resno1, comp->numresolutions); pi->resno++) {
                    uint32_t levelno;
                    uint32_t trx0, try0;
                    uint32_t trx1, try1;
                    uint32_t rpx, rpy;
                    uint32_t prci, prcj;
                    res = &comp->resolutions[pi->resno];
                    levelno = comp->numresolutions - 1 - pi->resno;
					if (levelno >= OPJ_J2K_MAXRLVLS)
						continue;
                    trx0 = grk_uint64_ceildiv((uint64_t)pi->tx0, ((uint64_t)comp->dx << levelno));
                    try0 = grk_uint64_ceildiv((uint64_t)pi->ty0, ((uint64_t)comp->dy << levelno));
                    trx1 = grk_uint64_ceildiv((uint64_t)pi->tx1, ((uint64_t)comp->dx << levelno));
                    try1 = grk_uint64_ceildiv((uint64_t)pi->ty1, ((uint64_t)comp->dy << levelno));
                    rpx = res->pdx + levelno;
                    rpy = res->pdy + levelno;
                    if (!(((uint64_t)pi->y % ((uint64_t)comp->dy << rpy) == 0) || ((pi->y == pi->ty0) && (((uint64_t)try0 << levelno) % ((uint64_t)1 << rpy))))) {
                        continue;
                    }
                    if (!(((uint64_t)pi->x % ((uint64_t)comp->dx << rpx) == 0) || ((pi->x == pi->tx0) && (((uint64_t)trx0 << levelno) % ((uint64_t)1 << rpx))))) {
                        continue;
                    }

                    if ((res->pw==0)||(res->ph==0)) continue;

                    if ((trx0==trx1)||(try0==try1)) continue;

                    prci = grk_uint_floordivpow2(grk_uint64_ceildiv((uint64_t)pi->x, ((uint64_t)comp->dx << levelno)),res->pdx)
                           - grk_uint_floordivpow2(trx0, res->pdx);
                    prcj = grk_uint_floordivpow2(grk_uint64_ceildiv((uint64_t)pi->y, ((uint64_t)comp->dy << levelno)), res->pdy)
                           - grk_uint_floordivpow2(try0, res->pdy);
                    pi->precno = (prci + prcj * res->pw);
                    for (pi->layno = pi->poc.layno0; pi->layno < pi->poc.layno1; pi->layno++) {
                        index = pi->layno * pi->step_l + pi->resno * pi->step_r + pi->compno * pi->step_c + pi->precno * pi->step_p;
                        if (!pi->include[index]) {
                            pi->include[index] = 1;
                            return true;
                        }
LABEL_SKIP:
                        ;
                    }
                }
            }
        }
    }

    return false;
}

static bool grk_pi_next_cprl(grk_pi_iterator_t * pi)
{
    grk_pi_comp_t *comp = NULL;
    grk_pi_resolution_t *res = NULL;
    uint32_t index = 0;

    if (!pi->first) {
        comp = &pi->comps[pi->compno];
        goto LABEL_SKIP;
    } 

	pi->first = 0;
    for (pi->compno = pi->poc.compno0; pi->compno < pi->poc.compno1; pi->compno++) {
        comp = &pi->comps[pi->compno];
        pi->dx = 0;
        pi->dy = 0;
		update_pi_dxy_for_comp(pi, comp);
        if (!pi->tp_on) {
            pi->poc.ty0 = pi->ty0;
            pi->poc.tx0 = pi->tx0;
            pi->poc.ty1 = pi->ty1;
            pi->poc.tx1 = pi->tx1;
        }
        for (pi->y = pi->poc.ty0; pi->y < pi->poc.ty1; pi->y += pi->dy - (pi->y % pi->dy)) {
            for (pi->x = pi->poc.tx0; pi->x < pi->poc.tx1; pi->x += pi->dx - (pi->x % pi->dx)) {
                for (pi->resno = pi->poc.resno0; pi->resno < grk_min<uint32_t>(pi->poc.resno1, comp->numresolutions); pi->resno++) {
                    uint32_t levelno;
                    uint32_t trx0, try0;
                    uint32_t trx1, try1;
                    uint32_t rpx, rpy;
                    int32_t prci, prcj;
                    res = &comp->resolutions[pi->resno];
                    levelno = comp->numresolutions - 1 - pi->resno;
					if (levelno >= OPJ_J2K_MAXRLVLS)
						continue;
                    trx0 = grk_uint64_ceildiv((uint64_t)pi->tx0, ((uint64_t)comp->dx << levelno));
                    try0 = grk_uint64_ceildiv((uint64_t)pi->ty0, ((uint64_t)comp->dy << levelno));
                    trx1 = grk_uint64_ceildiv((uint64_t)pi->tx1, ((uint64_t)comp->dx << levelno));
                    try1 = grk_uint64_ceildiv((uint64_t)pi->ty1, ((uint64_t)comp->dy << levelno));
                    rpx = res->pdx + levelno;
                    rpy = res->pdy + levelno;
                    if (!(((uint64_t)pi->y % ((uint64_t)comp->dy << rpy) == 0) || ((pi->y == pi->ty0) && (((uint64_t)try0 << levelno) % ((uint64_t)1 << rpy))))) {
                        continue;
                    }
                    if (!(((uint64_t)pi->x % ((uint64_t)comp->dx << rpx) == 0) || ((pi->x == pi->tx0) && (((uint64_t)trx0 << levelno) % ((uint64_t)1 << rpx))))) {
                        continue;
                    }

                    if ((res->pw==0)||(res->ph==0)) continue;

                    if ((trx0==trx1)||(try0==try1)) continue;

                    prci = grk_uint_floordivpow2(grk_uint64_ceildiv((uint64_t)pi->x, ((uint64_t)comp->dx << levelno)), res->pdx)
                           - grk_uint_floordivpow2(trx0, res->pdx);
                    prcj = grk_uint_floordivpow2(grk_uint64_ceildiv((uint64_t)pi->y, ((uint64_t)comp->dy << levelno)), res->pdy)
                           - grk_uint_floordivpow2(try0, res->pdy);
                    pi->precno = prci + prcj * res->pw;
                    for (pi->layno = pi->poc.layno0; pi->layno < pi->poc.layno1; pi->layno++) {
                        index = pi->layno * pi->step_l + pi->resno * pi->step_r + pi->compno * pi->step_c + pi->precno * pi->step_p;
                        if (!pi->include[index]) {
                            pi->include[index] = 1;
                            return true;
                        }
LABEL_SKIP:
                        ;
                    }
                }
            }
        }
    }

    return false;
}

static void opj_get_encoding_parameters(	const opj_image_t *p_image,
        const opj_cp_t *p_cp,
        uint32_t p_tileno,
        uint32_t * p_tx0,
        uint32_t  * p_tx1,
        uint32_t  * p_ty0,
        uint32_t  * p_ty1,
        uint32_t * p_dx_min,
        uint32_t * p_dy_min,
        uint32_t * p_max_prec,
        uint32_t * p_max_res )
{
    /* loop */
    uint32_t  compno, resno;
    /* pointers */
    const grk_tcp_t *l_tcp = nullptr;
    const opj_tccp_t * l_tccp = nullptr;
    const opj_image_comp_t * l_img_comp = nullptr;

    /* position in x and y of tile */
    uint32_t p, q;

    /* preconditions */
    assert(p_cp != nullptr);
    assert(p_image != nullptr);
    assert(p_tileno < p_cp->tw * p_cp->th);

    /* initializations */
    l_tcp = &p_cp->tcps [p_tileno];
    l_img_comp = p_image->comps;
    l_tccp = l_tcp->tccps;

    /* here calculation of tx0, tx1, ty0, ty1, maxprec, dx and dy */
    p = p_tileno % p_cp->tw;
    q = p_tileno / p_cp->tw;

    /* find extent of tile */
    *p_tx0 = grk_max<uint32_t>(p_cp->tx0 + p * p_cp->tdx, p_image->x0);
    *p_tx1 = grk_min<uint32_t>(p_cp->tx0 + (p + 1) * p_cp->tdx, p_image->x1);
    *p_ty0 = grk_max<uint32_t>(p_cp->ty0 + q * p_cp->tdy, p_image->y0);
    *p_ty1 = grk_min<uint32_t>(p_cp->ty0 + (q + 1) * p_cp->tdy, p_image->y1);

    /* max precision is 0 (can only grow) */
    *p_max_prec = 0;
    *p_max_res = 0;

    /* take the largest value for dx_min and dy_min */
    *p_dx_min = 0x7fffffff;
    *p_dy_min  = 0x7fffffff;

    for (compno = 0; compno < p_image->numcomps; ++compno) {
        /* arithmetic variables to calculate */
        uint32_t l_level_no;
        uint32_t l_rx0, l_ry0, l_rx1, l_ry1;
        uint32_t l_px0, l_py0, l_px1, py1;
        uint32_t l_pdx, l_pdy;
        uint32_t l_pw, l_ph;
        uint32_t l_product;
        uint32_t l_tcx0, l_tcy0, l_tcx1, l_tcy1;

        l_tcx0 = grk_uint_ceildiv(*p_tx0, l_img_comp->dx);
        l_tcy0 = grk_uint_ceildiv(*p_ty0, l_img_comp->dy);
        l_tcx1 = grk_uint_ceildiv(*p_tx1, l_img_comp->dx);
        l_tcy1 = grk_uint_ceildiv(*p_ty1, l_img_comp->dy);

        if (l_tccp->numresolutions > *p_max_res) {
            *p_max_res = l_tccp->numresolutions;
        }

        /* use custom size for precincts */
        for (resno = 0; resno < l_tccp->numresolutions; ++resno) {

            /* precinct width and height */
            l_pdx = l_tccp->prcw[resno];
            l_pdy = l_tccp->prch[resno];

            uint64_t l_dx = l_img_comp->dx * ((uint64_t)1u << (l_pdx + l_tccp->numresolutions - 1 - resno));
			uint64_t l_dy = l_img_comp->dy * ((uint64_t)1u << (l_pdy + l_tccp->numresolutions - 1 - resno));

            /* take the minimum size for dx for each comp and resolution */
			if (l_dx < UINT_MAX)
				*p_dx_min = grk_min<uint32_t>(*p_dx_min, (uint32_t)l_dx);
			if (l_dy < UINT_MAX)
				*p_dy_min = grk_min<uint32_t>(*p_dy_min, (uint32_t)l_dy);

            /* various calculations of extents */
            l_level_no = l_tccp->numresolutions - 1 - resno;

            l_rx0 = grk_uint_ceildivpow2(l_tcx0, l_level_no);
            l_ry0 = grk_uint_ceildivpow2(l_tcy0, l_level_no);
            l_rx1 = grk_uint_ceildivpow2(l_tcx1, l_level_no);
            l_ry1 = grk_uint_ceildivpow2(l_tcy1, l_level_no);

            l_px0 = grk_uint_floordivpow2(l_rx0, l_pdx) << l_pdx;
            l_py0 = grk_uint_floordivpow2(l_ry0, l_pdy) << l_pdy;
            l_px1 = grk_uint_ceildivpow2(l_rx1, l_pdx) << l_pdx;

            py1 = grk_uint_ceildivpow2(l_ry1, l_pdy) << l_pdy;

            l_pw = (l_rx0==l_rx1)?0:((l_px1 - l_px0) >> l_pdx);
            l_ph = (l_ry0==l_ry1)?0:((py1 - l_py0) >> l_pdy);

            l_product = l_pw * l_ph;

            /* update precision */
            if (l_product > *p_max_prec) {
                *p_max_prec = l_product;
            }
        }
        ++l_img_comp;
        ++l_tccp;
    }
}


static void opj_get_all_encoding_parameters(   const opj_image_t *p_image,
        const opj_cp_t *p_cp,
        uint32_t tileno,
        uint32_t * p_tx0,
        uint32_t * p_tx1,
        uint32_t * p_ty0,
        uint32_t * p_ty1,
        uint32_t * p_dx_min,
        uint32_t * p_dy_min,
        uint32_t * p_max_prec,
        uint32_t * p_max_res,
        uint32_t ** p_resolutions )
{
    /* loop*/
    uint32_t compno, resno;

    /* pointers*/
    const grk_tcp_t *tcp = nullptr;
    const opj_tccp_t * l_tccp = nullptr;
    const opj_image_comp_t * l_img_comp = nullptr;

    /* to store l_dx, l_dy, w and h for each resolution and component.*/
    uint32_t * lResolutionPtr;

    /* position in x and y of tile*/
    uint32_t p, q;

    /* non-corrected (in regard to image offset) tile offset */
    uint32_t l_tx0, l_ty0;

    /* preconditions in debug*/
    assert(p_cp != nullptr);
    assert(p_image != nullptr);
    assert(tileno < p_cp->tw * p_cp->th);

    /* initializations*/
    tcp = &p_cp->tcps [tileno];
    l_tccp = tcp->tccps;
    l_img_comp = p_image->comps;

    /* position in x and y of tile*/
    p = tileno % p_cp->tw;
    q = tileno / p_cp->tw;

    /* here calculation of tx0, tx1, ty0, ty1, maxprec, l_dx and l_dy */
    l_tx0 = p_cp->tx0 + p * p_cp->tdx; /* can't be greater than p_image->x1 so won't overflow */
    *p_tx0 = grk_max<uint32_t>(l_tx0, p_image->x0);
    *p_tx1 = grk_min<uint32_t>(grk_uint_adds(l_tx0, p_cp->tdx), p_image->x1);
    l_ty0 = p_cp->ty0 + q * p_cp->tdy; /* can't be greater than p_image->y1 so won't overflow */
    *p_ty0 = grk_max<uint32_t>(l_ty0, p_image->y0);
    *p_ty1 = grk_min<uint32_t>(grk_uint_adds(l_ty0, p_cp->tdy), p_image->y1);

    /* max precision and resolution is 0 (can only grow)*/
    *p_max_prec = 0;
    *p_max_res = 0;

    /* take the largest value for dx_min and dy_min*/
    *p_dx_min = 0x7fffffff;
    *p_dy_min = 0x7fffffff;

    for (compno = 0; compno < p_image->numcomps; ++compno) {
        /* arithmetic variables to calculate*/
        uint32_t l_level_no;
        uint32_t l_rx0, l_ry0, l_rx1, l_ry1;
        uint32_t l_px0, l_py0, l_px1, py1;
        uint32_t l_product;
        uint32_t l_tcx0, l_tcy0, l_tcx1, l_tcy1;
        uint32_t l_pdx, l_pdy , l_pw , l_ph;

        lResolutionPtr = p_resolutions[compno];

        l_tcx0 = grk_uint_ceildiv(*p_tx0, l_img_comp->dx);
        l_tcy0 = grk_uint_ceildiv(*p_ty0, l_img_comp->dy);
        l_tcx1 = grk_uint_ceildiv(*p_tx1, l_img_comp->dx);
        l_tcy1 = grk_uint_ceildiv(*p_ty1, l_img_comp->dy);

        if (l_tccp->numresolutions > *p_max_res) {
            *p_max_res = l_tccp->numresolutions;
        }

        /* use custom size for precincts*/
        l_level_no = l_tccp->numresolutions - 1;
        for (resno = 0; resno < l_tccp->numresolutions; ++resno) {

            /* precinct width and height*/
            l_pdx = l_tccp->prcw[resno];
            l_pdy = l_tccp->prch[resno];
            *lResolutionPtr++ = l_pdx;
            *lResolutionPtr++ = l_pdy;
            uint64_t l_dx = l_img_comp->dx * ((uint64_t)1u << (l_pdx + l_level_no));
			uint64_t l_dy = l_img_comp->dy * ((uint64_t)1u << (l_pdy + l_level_no));
            /* take the minimum size for l_dx for each comp and resolution*/
			if (l_dx < UINT_MAX)
	            *p_dx_min = grk_min<uint32_t>(*p_dx_min, (uint32_t)l_dx);
			if (l_dy < UINT_MAX)
				*p_dy_min = grk_min<uint32_t>(*p_dy_min, (uint32_t)l_dy);

            /* various calculations of extents*/
            l_rx0 = grk_uint_ceildivpow2(l_tcx0, l_level_no);
            l_ry0 = grk_uint_ceildivpow2(l_tcy0, l_level_no);
            l_rx1 = grk_uint_ceildivpow2(l_tcx1, l_level_no);
            l_ry1 = grk_uint_ceildivpow2(l_tcy1, l_level_no);
            l_px0 = grk_uint_floordivpow2(l_rx0, l_pdx) << l_pdx;
            l_py0 = grk_uint_floordivpow2(l_ry0, l_pdy) << l_pdy;
            l_px1 = grk_uint_ceildivpow2(l_rx1, l_pdx) << l_pdx;
            py1 = grk_uint_ceildivpow2(l_ry1, l_pdy) << l_pdy;
            l_pw = (l_rx0==l_rx1)?0:((l_px1 - l_px0) >> l_pdx);
            l_ph = (l_ry0==l_ry1)?0:((py1 - l_py0) >> l_pdy);
            *lResolutionPtr++ = l_pw;
            *lResolutionPtr++ = l_ph;
            l_product = l_pw * l_ph;

            /* update precision*/
            if (l_product > *p_max_prec) {
                *p_max_prec = l_product;
            }

            --l_level_no;
        }
        ++l_tccp;
        ++l_img_comp;
    }
}

static grk_pi_iterator_t * grk_pi_create(	const opj_image_t *image,
        const opj_cp_t *cp,
        uint32_t tileno )
{
    /* loop*/
    uint32_t pino, compno;
    /* number of poc in the p_pi*/
    uint32_t l_poc_bound;

    /* pointers to tile coding parameters and components.*/
    grk_pi_iterator_t *l_pi = nullptr;
    grk_tcp_t *tcp = nullptr;
    const opj_tccp_t *tccp = nullptr;

    /* current packet iterator being allocated*/
    grk_pi_iterator_t *l_current_pi = nullptr;

    /* preconditions in debug*/
    assert(cp != nullptr);
    assert(image != nullptr);
    assert(tileno < cp->tw * cp->th);

    /* initializations*/
    tcp = &cp->tcps[tileno];
    l_poc_bound = tcp->numpocs+1;

    /* memory allocations*/
    l_pi = (grk_pi_iterator_t*) grk_calloc((l_poc_bound), sizeof(grk_pi_iterator_t));
    if (!l_pi) {
        return NULL;
    }

    l_current_pi = l_pi;
    for (pino = 0; pino < l_poc_bound ; ++pino) {

        l_current_pi->comps = (grk_pi_comp_t*) grk_calloc(image->numcomps, sizeof(grk_pi_comp_t));
        if (! l_current_pi->comps) {
            grk_pi_destroy(l_pi, l_poc_bound);
            return NULL;
        }

        l_current_pi->numcomps = image->numcomps;

        for (compno = 0; compno < image->numcomps; ++compno) {
            grk_pi_comp_t *comp = &l_current_pi->comps[compno];

            tccp = &tcp->tccps[compno];

            comp->resolutions = (grk_pi_resolution_t*) grk_calloc(tccp->numresolutions, sizeof(grk_pi_resolution_t));
            if (!comp->resolutions) {
                grk_pi_destroy(l_pi, l_poc_bound);
                return nullptr;
            }

            comp->numresolutions = tccp->numresolutions;
        }
        ++l_current_pi;
    }
    return l_pi;
}

static void grk_pi_update_encode_poc_and_final (   opj_cp_t *p_cp,
        uint32_t p_tileno,
        uint32_t p_tx0,
        uint32_t p_tx1,
        uint32_t p_ty0,
        uint32_t p_ty1,
        uint32_t p_max_prec,
        uint32_t p_max_res,
        uint32_t p_dx_min,
        uint32_t p_dy_min)
{
    /* loop*/
    uint32_t pino;
    /* tile coding parameter*/
    grk_tcp_t *l_tcp = nullptr;
    /* current poc being updated*/
    opj_poc_t * l_current_poc = nullptr;

    /* number of pocs*/
    uint32_t l_poc_bound;

    OPJ_ARG_NOT_USED(p_max_res);

    /* preconditions in debug*/
    assert(p_cp != nullptr);
    assert(p_tileno < p_cp->tw * p_cp->th);

    /* initializations*/
    l_tcp = &p_cp->tcps [p_tileno];
    /* number of iterations in the loop */
    l_poc_bound = l_tcp->numpocs+1;

    /* start at first element, and to make sure the compiler will not make a calculation each time in the loop
       store a pointer to the current element to modify rather than l_tcp->pocs[i]*/
    l_current_poc = l_tcp->pocs;

    l_current_poc->compS = l_current_poc->compno0;
    l_current_poc->compE = l_current_poc->compno1;
    l_current_poc->resS = l_current_poc->resno0;
    l_current_poc->resE = l_current_poc->resno1;
    l_current_poc->layE = l_current_poc->layno1;

    /* special treatment for the first element*/
    l_current_poc->layS = 0;
    l_current_poc->prg  = l_current_poc->prg1;
    l_current_poc->prcS = 0;

    l_current_poc->prcE = p_max_prec;
    l_current_poc->txS = p_tx0;
    l_current_poc->txE = p_tx1;
    l_current_poc->tyS = p_ty0;
    l_current_poc->tyE = p_ty1;
    l_current_poc->dx = p_dx_min;
    l_current_poc->dy = p_dy_min;

    ++ l_current_poc;
    for (pino = 1; pino < l_poc_bound ; ++pino) {
        l_current_poc->compS = l_current_poc->compno0;
        l_current_poc->compE= l_current_poc->compno1;
        l_current_poc->resS = l_current_poc->resno0;
        l_current_poc->resE = l_current_poc->resno1;
        l_current_poc->layE = l_current_poc->layno1;
        l_current_poc->prg  = l_current_poc->prg1;
        l_current_poc->prcS = 0;
        /* special treatment here different from the first element*/
        l_current_poc->layS = (l_current_poc->layE > (l_current_poc-1)->layE) ? l_current_poc->layE : 0;

        l_current_poc->prcE = p_max_prec;
        l_current_poc->txS = p_tx0;
        l_current_poc->txE = p_tx1;
        l_current_poc->tyS = p_ty0;
        l_current_poc->tyE = p_ty1;
        l_current_poc->dx = p_dx_min;
        l_current_poc->dy = p_dy_min;
        ++ l_current_poc;
    }
}

static void grk_pi_update_encode_not_poc (	opj_cp_t *p_cp,
        uint32_t p_num_comps,
        uint32_t p_tileno,
        uint32_t p_tx0,
        uint32_t p_tx1,
        uint32_t p_ty0,
        uint32_t p_ty1,
        uint32_t p_max_prec,
        uint32_t p_max_res,
        uint32_t p_dx_min,
        uint32_t p_dy_min)
{
    /* loop*/
    uint32_t pino;
    /* tile coding parameter*/
    grk_tcp_t *l_tcp = nullptr;
    /* current poc being updated*/
    opj_poc_t * l_current_poc = nullptr;
    /* number of pocs*/
    uint32_t l_poc_bound;

    /* preconditions in debug*/
    assert(p_cp != nullptr);
    assert(p_tileno < p_cp->tw * p_cp->th);

    /* initializations*/
    l_tcp = &p_cp->tcps [p_tileno];

    /* number of iterations in the loop */
    l_poc_bound = l_tcp->numpocs+1;

    /* start at first element, and to make sure the compiler will not make a calculation each time in the loop
       store a pointer to the current element to modify rather than l_tcp->pocs[i]*/
    l_current_poc = l_tcp->pocs;

    for (pino = 0; pino < l_poc_bound ; ++pino) {
        l_current_poc->compS = 0;
        l_current_poc->compE = p_num_comps;/*p_image->numcomps;*/
        l_current_poc->resS = 0;
        l_current_poc->resE = p_max_res;
        l_current_poc->layS = 0;
        l_current_poc->layE = l_tcp->numlayers;
        l_current_poc->prg  = l_tcp->prg;
        l_current_poc->prcS = 0;
        l_current_poc->prcE = p_max_prec;
        l_current_poc->txS = p_tx0;
        l_current_poc->txE = p_tx1;
        l_current_poc->tyS = p_ty0;
        l_current_poc->tyE = p_ty1;
        l_current_poc->dx = p_dx_min;
        l_current_poc->dy = p_dy_min;
        ++ l_current_poc;
    }
}

static void grk_pi_update_decode_poc (grk_pi_iterator_t * p_pi,
                                      grk_tcp_t * p_tcp,
                                      uint32_t p_max_precision,
                                      uint32_t p_max_res)
{
    /* loop*/
    uint32_t pino;

    /* encoding prameters to set*/
    uint32_t l_bound;

    grk_pi_iterator_t * l_current_pi = nullptr;
    opj_poc_t* l_current_poc = 0;

    OPJ_ARG_NOT_USED(p_max_res);

    /* preconditions in debug*/
    assert(p_pi != nullptr);
    assert(p_tcp != nullptr);

    /* initializations*/
    l_bound = p_tcp->numpocs+1;
    l_current_pi = p_pi;
    l_current_poc = p_tcp->pocs;

    for	(pino = 0; pino<l_bound; ++pino) {
        l_current_pi->poc.prg = l_current_poc->prg; /* Progression Order #0 */
        l_current_pi->first = 1;

        l_current_pi->poc.resno0 = l_current_poc->resno0; /* Resolution Level Index #0 (Start) */
        l_current_pi->poc.compno0 = l_current_poc->compno0; /* Component Index #0 (Start) */
        l_current_pi->poc.layno0 = 0;
        l_current_pi->poc.precno0 = 0;
        l_current_pi->poc.resno1 = l_current_poc->resno1; /* Resolution Level Index #0 (End) */
        l_current_pi->poc.compno1 = l_current_poc->compno1; /* Component Index #0 (End) */
		l_current_pi->poc.layno1 = grk_min<uint32_t>(l_current_poc->layno1, p_tcp->numlayers); /* Layer Index #0 (End) */
        l_current_pi->poc.precno1 = p_max_precision;
        ++l_current_pi;
        ++l_current_poc;
    }
}

static void grk_pi_update_decode_not_poc (grk_pi_iterator_t * p_pi,
        grk_tcp_t * p_tcp,
        uint32_t p_max_precision,
        uint32_t p_max_res)
{
    /* loop*/
    uint32_t pino;

    /* encoding parameters to set*/
    uint32_t l_bound;

    grk_pi_iterator_t * l_current_pi = nullptr;
    /* preconditions in debug*/
    assert(p_tcp != nullptr);
    assert(p_pi != nullptr);

    /* initializations*/
    l_bound = p_tcp->numpocs+1;
    l_current_pi = p_pi;

    for (pino = 0; pino<l_bound; ++pino) {
        l_current_pi->poc.prg = p_tcp->prg;
        l_current_pi->first = 1;
        l_current_pi->poc.resno0 = 0;
        l_current_pi->poc.compno0 = 0;
        l_current_pi->poc.layno0 = 0;
        l_current_pi->poc.precno0 = 0;
        l_current_pi->poc.resno1 = p_max_res;
        l_current_pi->poc.compno1 = l_current_pi->numcomps;
        l_current_pi->poc.layno1 = p_tcp->numlayers;
        l_current_pi->poc.precno1 = p_max_precision;
        ++l_current_pi;
    }
}



static bool grk_pi_check_next_level(	int32_t pos,
                                        opj_cp_t *cp,
                                        uint32_t tileno,
                                        uint32_t pino,
                                        const char *prog)
{
    uint32_t i;
    grk_tcp_t *tcps =&cp->tcps[tileno];
    opj_poc_t *tcp = &tcps->pocs[pino];

    if(pos>=0) {
        for(i=pos; pos>=0; i--) {
            switch(prog[i]) {
            case 'R':
                if(tcp->res_t==tcp->resE) {
                    if(grk_pi_check_next_level(pos-1,cp,tileno,pino,prog)) {
                        return true;
                    } else {
                        return false;
                    }
                } else {
                    return true;
                }
                break;
            case 'C':
                if(tcp->comp_t==tcp->compE) {
                    if(grk_pi_check_next_level(pos-1,cp,tileno,pino,prog)) {
                        return true;
                    } else {
                        return false;
                    }
                } else {
                    return true;
                }
                break;
            case 'L':
                if(tcp->lay_t==tcp->layE) {
                    if(grk_pi_check_next_level(pos-1,cp,tileno,pino,prog)) {
                        return true;
                    } else {
                        return false;
                    }
                } else {
                    return true;
                }
                break;
            case 'P':
                switch(tcp->prg) {
                case OPJ_LRCP: /* fall through */
                case OPJ_RLCP:
                    if(tcp->prc_t == tcp->prcE) {
                        if(grk_pi_check_next_level(i-1,cp,tileno,pino,prog)) {
                            return true;
                        } else {
                            return false;
                        }
                    } else {
                        return true;
                    }
                    break;
                default:
                    if(tcp->tx0_t == tcp->txE) {
                        /*TY*/
                        if(tcp->ty0_t == tcp->tyE) {
                            if(grk_pi_check_next_level(i-1,cp,tileno,pino,prog)) {
                                return true;
                            } else {
                                return false;
                            }
                        } else {
                            return true;
                        }/*TY*/
                    } else {
                        return true;
                    }
                    break;
                }/*end case P*/
            }/*end switch*/
        }/*end for*/
    }/*end if*/
    return false;
}


/*
==========================================================
   Packet iterator interface
==========================================================
*/
grk_pi_iterator_t *grk_pi_create_decode(opj_image_t *p_image,
                                        opj_cp_t *p_cp,
                                        uint32_t p_tile_no)
{
    /* loop */
    uint32_t pino;
    uint32_t compno, resno;

    /* to store w, h, dx and dy for all components and resolutions */
    uint32_t * l_tmp_data;
    uint32_t ** l_tmp_ptr;

    /* encoding prameters to set */
    uint32_t l_max_res;
    uint32_t l_max_prec;
    uint32_t l_tx0,l_tx1,l_ty0,l_ty1;
    uint32_t l_dx_min,l_dy_min;
    uint32_t l_bound;
    uint32_t l_step_p , l_step_c , l_step_r , l_step_l ;
    uint32_t l_data_stride;

    /* pointers */
    grk_pi_iterator_t *l_pi = nullptr;
    grk_tcp_t *l_tcp = nullptr;
    const opj_tccp_t *l_tccp = nullptr;
    grk_pi_comp_t *l_current_comp = nullptr;
    opj_image_comp_t * l_img_comp = nullptr;
    grk_pi_iterator_t * l_current_pi = nullptr;
    uint32_t * l_encoding_value_ptr = nullptr;

    /* preconditions in debug */
    assert(p_cp != nullptr);
    assert(p_image != nullptr);
    assert(p_tile_no < p_cp->tw * p_cp->th);

    /* initializations */
    l_tcp = &p_cp->tcps[p_tile_no];
    l_bound = l_tcp->numpocs+1;

    l_data_stride = 4 * OPJ_J2K_MAXRLVLS;
    l_tmp_data = (uint32_t*)grk_malloc(l_data_stride * p_image->numcomps * sizeof(uint32_t));
    if (! l_tmp_data) {
        return nullptr;
    }
    l_tmp_ptr = (uint32_t**)grk_malloc(p_image->numcomps * sizeof(uint32_t *));
    if  (! l_tmp_ptr) {
        grk_free(l_tmp_data);
        return nullptr;
    }

    /* memory allocation for pi */
    l_pi = grk_pi_create(p_image, p_cp, p_tile_no);
    if (!l_pi) {
        grk_free(l_tmp_data);
        grk_free(l_tmp_ptr);
        return nullptr;
    }

    l_encoding_value_ptr = l_tmp_data;
    /* update pointer array */
    for
    (compno = 0; compno < p_image->numcomps; ++compno) {
        l_tmp_ptr[compno] = l_encoding_value_ptr;
        l_encoding_value_ptr += l_data_stride;
    }
    /* get encoding parameters */
    opj_get_all_encoding_parameters(p_image,p_cp,p_tile_no,&l_tx0,&l_tx1,&l_ty0,&l_ty1,&l_dx_min,&l_dy_min,&l_max_prec,&l_max_res,l_tmp_ptr);

    /* step calculations */
    l_step_p = 1;
    l_step_c = l_max_prec * l_step_p;
    l_step_r = p_image->numcomps * l_step_c;
    l_step_l = l_max_res * l_step_r;

    /* set values for first packet iterator */
    l_current_pi = l_pi;

    /* memory allocation for include */
	l_current_pi->include = nullptr;
	if (l_step_l && (l_tcp->numlayers < (SIZE_MAX / l_step_l) - 1))		 {
		l_current_pi->include = (int16_t*)grk_calloc(((size_t)l_tcp->numlayers + 1) * l_step_l, sizeof(int16_t));
	}

    if (!l_current_pi->include) {
        grk_free(l_tmp_data);
        grk_free(l_tmp_ptr);
        grk_pi_destroy(l_pi, l_bound);
        return nullptr;
    }

    /* special treatment for the first packet iterator */
    l_current_comp = l_current_pi->comps;
    l_img_comp = p_image->comps;
    l_tccp = l_tcp->tccps;

    l_current_pi->tx0 = l_tx0;
    l_current_pi->ty0 = l_ty0;
    l_current_pi->tx1 = l_tx1;
    l_current_pi->ty1 = l_ty1;

    /*l_current_pi->dx = l_img_comp->dx;*/
    /*l_current_pi->dy = l_img_comp->dy;*/

    l_current_pi->step_p = l_step_p;
    l_current_pi->step_c = l_step_c;
    l_current_pi->step_r = l_step_r;
    l_current_pi->step_l = l_step_l;

    /* allocation for components and number of components has already been calculated by grk_pi_create */
    for
    (compno = 0; compno < l_current_pi->numcomps; ++compno) {
        grk_pi_resolution_t *l_res = l_current_comp->resolutions;
        l_encoding_value_ptr = l_tmp_ptr[compno];

        l_current_comp->dx = l_img_comp->dx;
        l_current_comp->dy = l_img_comp->dy;
        /* resolutions have already been initialized */
        for
        (resno = 0; resno < l_current_comp->numresolutions; resno++) {
            l_res->pdx = *(l_encoding_value_ptr++);
            l_res->pdy = *(l_encoding_value_ptr++);
            l_res->pw =  *(l_encoding_value_ptr++);
            l_res->ph =  *(l_encoding_value_ptr++);
            ++l_res;
        }
        ++l_current_comp;
        ++l_img_comp;
        ++l_tccp;
    }
    ++l_current_pi;

    for (pino = 1 ; pino<l_bound ; ++pino ) {
        l_current_comp = l_current_pi->comps;
        l_img_comp = p_image->comps;
        l_tccp = l_tcp->tccps;

        l_current_pi->tx0 = l_tx0;
        l_current_pi->ty0 = l_ty0;
        l_current_pi->tx1 = l_tx1;
        l_current_pi->ty1 = l_ty1;
        /*l_current_pi->dx = l_dx_min;*/
        /*l_current_pi->dy = l_dy_min;*/
        l_current_pi->step_p = l_step_p;
        l_current_pi->step_c = l_step_c;
        l_current_pi->step_r = l_step_r;
        l_current_pi->step_l = l_step_l;

        /* allocation for components and number of components has already been calculated by grk_pi_create */
        for
        (compno = 0; compno < l_current_pi->numcomps; ++compno) {
            grk_pi_resolution_t *l_res = l_current_comp->resolutions;
            l_encoding_value_ptr = l_tmp_ptr[compno];

            l_current_comp->dx = l_img_comp->dx;
            l_current_comp->dy = l_img_comp->dy;
            /* resolutions have already been initialized */
            for
            (resno = 0; resno < l_current_comp->numresolutions; resno++) {
                l_res->pdx = *(l_encoding_value_ptr++);
                l_res->pdy = *(l_encoding_value_ptr++);
                l_res->pw =  *(l_encoding_value_ptr++);
                l_res->ph =  *(l_encoding_value_ptr++);
                ++l_res;
            }
            ++l_current_comp;
            ++l_img_comp;
            ++l_tccp;
        }
        /* special treatment*/
        l_current_pi->include = (l_current_pi-1)->include;
        ++l_current_pi;
    }
    grk_free(l_tmp_data);
    l_tmp_data = nullptr;
    grk_free(l_tmp_ptr);
    l_tmp_ptr = nullptr;
    if
    (l_tcp->POC) {
        grk_pi_update_decode_poc (l_pi,l_tcp,l_max_prec,l_max_res);
    } else {
        grk_pi_update_decode_not_poc(l_pi,l_tcp,l_max_prec,l_max_res);
    }
    return l_pi;
}



grk_pi_iterator_t *grk_pi_initialise_encode(const opj_image_t *p_image,
        opj_cp_t *p_cp,
        uint32_t p_tile_no,
        J2K_T2_MODE p_t2_mode )
{
    /* loop*/
    uint32_t pino;
    uint32_t compno, resno;

    /* to store w, h, dx and dy for all components and resolutions*/
    uint32_t * l_tmp_data;
    uint32_t ** l_tmp_ptr;

    /* encoding prameters to set*/
    uint32_t l_max_res;
    uint32_t l_max_prec;
    uint32_t l_tx0,l_tx1,l_ty0,l_ty1;
    uint32_t l_dx_min,l_dy_min;
    uint32_t l_bound;
    uint32_t l_step_p , l_step_c , l_step_r , l_step_l ;
    uint32_t l_data_stride;

    /* pointers*/
    grk_pi_iterator_t *l_pi = nullptr;
    grk_tcp_t *l_tcp = nullptr;
    const opj_tccp_t *l_tccp = nullptr;
    grk_pi_comp_t *l_current_comp = nullptr;
    opj_image_comp_t * l_img_comp = nullptr;
    grk_pi_iterator_t * l_current_pi = nullptr;
    uint32_t * l_encoding_value_ptr = nullptr;

    /* preconditions in debug*/
    assert(p_cp != nullptr);
    assert(p_image != nullptr);
    assert(p_tile_no < p_cp->tw * p_cp->th);

    /* initializations*/
    l_tcp = &p_cp->tcps[p_tile_no];
    l_bound = l_tcp->numpocs+1;

    l_data_stride = 4 * OPJ_J2K_MAXRLVLS;
    l_tmp_data = (uint32_t*)grk_malloc(
                     l_data_stride * p_image->numcomps * sizeof(uint32_t));
    if (! l_tmp_data) {
        return nullptr;
    }

    l_tmp_ptr = (uint32_t**)grk_malloc(
                    p_image->numcomps * sizeof(uint32_t *));
    if (! l_tmp_ptr) {
        grk_free(l_tmp_data);
        return nullptr;
    }

    /* memory allocation for pi*/
    l_pi = grk_pi_create(p_image,p_cp,p_tile_no);
    if (!l_pi) {
        grk_free(l_tmp_data);
        grk_free(l_tmp_ptr);
        return nullptr;
    }

    l_encoding_value_ptr = l_tmp_data;
    /* update pointer array*/
    for (compno = 0; compno < p_image->numcomps; ++compno) {
        l_tmp_ptr[compno] = l_encoding_value_ptr;
        l_encoding_value_ptr += l_data_stride;
    }

    /* get encoding parameters*/
    opj_get_all_encoding_parameters(p_image,p_cp,p_tile_no,&l_tx0,&l_tx1,&l_ty0,&l_ty1,&l_dx_min,&l_dy_min,&l_max_prec,&l_max_res,l_tmp_ptr);

    /* step calculations*/
    l_step_p = 1;
    l_step_c = l_max_prec * l_step_p;
    l_step_r = p_image->numcomps * l_step_c;
    l_step_l = l_max_res * l_step_r;

    /* set values for first packet iterator*/
    l_pi->tp_on = (uint8_t)p_cp->m_specific_param.m_enc.m_tp_on;
    l_current_pi = l_pi;

    /* memory allocation for include*/
    l_current_pi->include = (int16_t*) grk_calloc((size_t)l_tcp->numlayers * l_step_l, sizeof(int16_t));
    if (!l_current_pi->include) {
        grk_free(l_tmp_data);
        grk_free(l_tmp_ptr);
        grk_pi_destroy(l_pi, l_bound);
        return nullptr;
    }

    /* special treatment for the first packet iterator*/
    l_current_comp = l_current_pi->comps;
    l_img_comp = p_image->comps;
    l_tccp = l_tcp->tccps;
    l_current_pi->tx0 = l_tx0;
    l_current_pi->ty0 = l_ty0;
    l_current_pi->tx1 = l_tx1;
    l_current_pi->ty1 = l_ty1;
    l_current_pi->dx = l_dx_min;
    l_current_pi->dy = l_dy_min;
    l_current_pi->step_p = l_step_p;
    l_current_pi->step_c = l_step_c;
    l_current_pi->step_r = l_step_r;
    l_current_pi->step_l = l_step_l;

    /* allocation for components and number of components has already been calculated by grk_pi_create */
    for (compno = 0; compno < l_current_pi->numcomps; ++compno) {
        grk_pi_resolution_t *l_res = l_current_comp->resolutions;
        l_encoding_value_ptr = l_tmp_ptr[compno];

        l_current_comp->dx = l_img_comp->dx;
        l_current_comp->dy = l_img_comp->dy;

        /* resolutions have already been initialized */
        for (resno = 0; resno < l_current_comp->numresolutions; resno++) {
            l_res->pdx = *(l_encoding_value_ptr++);
            l_res->pdy = *(l_encoding_value_ptr++);
            l_res->pw =  *(l_encoding_value_ptr++);
            l_res->ph =  *(l_encoding_value_ptr++);
            ++l_res;
        }

        ++l_current_comp;
        ++l_img_comp;
        ++l_tccp;
    }
    ++l_current_pi;

    for (pino = 1 ; pino<l_bound ; ++pino ) {
        l_current_comp = l_current_pi->comps;
        l_img_comp = p_image->comps;
        l_tccp = l_tcp->tccps;

        l_current_pi->tx0 = l_tx0;
        l_current_pi->ty0 = l_ty0;
        l_current_pi->tx1 = l_tx1;
        l_current_pi->ty1 = l_ty1;
        l_current_pi->dx = l_dx_min;
        l_current_pi->dy = l_dy_min;
        l_current_pi->step_p = l_step_p;
        l_current_pi->step_c = l_step_c;
        l_current_pi->step_r = l_step_r;
        l_current_pi->step_l = l_step_l;

        /* allocation for components and number of components has already been calculated by grk_pi_create */
        for (compno = 0; compno < l_current_pi->numcomps; ++compno) {
            grk_pi_resolution_t *l_res = l_current_comp->resolutions;
            l_encoding_value_ptr = l_tmp_ptr[compno];

            l_current_comp->dx = l_img_comp->dx;
            l_current_comp->dy = l_img_comp->dy;
            /* resolutions have already been initialized */
            for (resno = 0; resno < l_current_comp->numresolutions; resno++) {
                l_res->pdx = *(l_encoding_value_ptr++);
                l_res->pdy = *(l_encoding_value_ptr++);
                l_res->pw =  *(l_encoding_value_ptr++);
                l_res->ph =  *(l_encoding_value_ptr++);
                ++l_res;
            }
            ++l_current_comp;
            ++l_img_comp;
            ++l_tccp;
        }

        /* special treatment*/
        l_current_pi->include = (l_current_pi-1)->include;
        ++l_current_pi;
    }

    grk_free(l_tmp_data);
    l_tmp_data = nullptr;
    grk_free(l_tmp_ptr);
    l_tmp_ptr = nullptr;

    if (l_tcp->POC && (OPJ_IS_CINEMA(p_cp->rsiz) || p_t2_mode == FINAL_PASS)) {
        grk_pi_update_encode_poc_and_final(p_cp,p_tile_no,l_tx0,l_tx1,l_ty0,l_ty1,l_max_prec,l_max_res,l_dx_min,l_dy_min);
    } else {
        grk_pi_update_encode_not_poc(p_cp,p_image->numcomps,p_tile_no,l_tx0,l_tx1,l_ty0,l_ty1,l_max_prec,l_max_res,l_dx_min,l_dy_min);
    }

    return l_pi;
}

void grk_pi_init_encode( 	grk_pi_iterator_t *pi,
                            opj_cp_t *cp,
                            uint32_t tileno,
                            uint32_t pino,
                            uint32_t tpnum,
                            uint32_t tppos,
                            J2K_T2_MODE t2_mode)
{
    const char *prog;
    int32_t i;
    uint32_t incr_top=1,resetX=0;
    grk_tcp_t *tcps =&cp->tcps[tileno];
    opj_poc_t *tcp= &tcps->pocs[pino];

    prog =  grk_j2k_convert_progression_order(tcp->prg);

    pi[pino].first = 1;
    pi[pino].poc.prg = tcp->prg;

    if(!(cp->m_specific_param.m_enc.m_tp_on && ((!OPJ_IS_CINEMA(cp->rsiz) && (t2_mode == FINAL_PASS)) || OPJ_IS_CINEMA(cp->rsiz)))) {
        pi[pino].poc.resno0 = tcp->resS;
        pi[pino].poc.resno1 = tcp->resE;
        pi[pino].poc.compno0 = tcp->compS;
        pi[pino].poc.compno1 = tcp->compE;
        pi[pino].poc.layno0 = tcp->layS;
        pi[pino].poc.layno1 = tcp->layE;
        pi[pino].poc.precno0 = tcp->prcS;
        pi[pino].poc.precno1 = tcp->prcE;
        pi[pino].poc.tx0 = tcp->txS;
        pi[pino].poc.ty0 = tcp->tyS;
        pi[pino].poc.tx1 = tcp->txE;
        pi[pino].poc.ty1 = tcp->tyE;
    } else {
        for(i=tppos+1; i<4; i++) {
            switch(prog[i]) {
            case 'R':
                pi[pino].poc.resno0 = tcp->resS;
                pi[pino].poc.resno1 = tcp->resE;
                break;
            case 'C':
                pi[pino].poc.compno0 = tcp->compS;
                pi[pino].poc.compno1 = tcp->compE;
                break;
            case 'L':
                pi[pino].poc.layno0 = tcp->layS;
                pi[pino].poc.layno1 = tcp->layE;
                break;
            case 'P':
                switch(tcp->prg) {
                case OPJ_LRCP:
                case OPJ_RLCP:
                    pi[pino].poc.precno0 = tcp->prcS;
                    pi[pino].poc.precno1 = tcp->prcE;
                    break;
                default:
                    pi[pino].poc.tx0 = tcp->txS;
                    pi[pino].poc.ty0 = tcp->tyS;
                    pi[pino].poc.tx1 = tcp->txE;
                    pi[pino].poc.ty1 = tcp->tyE;
                    break;
                }
                break;
            }
        }

        if(tpnum==0) {
            for(i=tppos; i>=0; i--) {
                switch(prog[i]) {
                case 'C':
                    tcp->comp_t = tcp->compS;
                    pi[pino].poc.compno0 = tcp->comp_t;
                    pi[pino].poc.compno1 = tcp->comp_t+1;
                    tcp->comp_t+=1;
                    break;
                case 'R':
                    tcp->res_t = tcp->resS;
                    pi[pino].poc.resno0 = tcp->res_t;
                    pi[pino].poc.resno1 = tcp->res_t+1;
                    tcp->res_t+=1;
                    break;
                case 'L':
                    tcp->lay_t = tcp->layS;
                    pi[pino].poc.layno0 = tcp->lay_t;
                    pi[pino].poc.layno1 = tcp->lay_t+1;
                    tcp->lay_t+=1;
                    break;
                case 'P':
                    switch(tcp->prg) {
                    case OPJ_LRCP:
                    case OPJ_RLCP:
                        tcp->prc_t = tcp->prcS;
                        pi[pino].poc.precno0 = tcp->prc_t;
                        pi[pino].poc.precno1 = tcp->prc_t+1;
                        tcp->prc_t+=1;
                        break;
                    default:
                        tcp->tx0_t = tcp->txS;
                        tcp->ty0_t = tcp->tyS;
                        pi[pino].poc.tx0 = tcp->tx0_t;
                        pi[pino].poc.tx1 = (tcp->tx0_t + tcp->dx - (tcp->tx0_t % tcp->dx));
                        pi[pino].poc.ty0 = tcp->ty0_t;
                        pi[pino].poc.ty1 = (tcp->ty0_t + tcp->dy - (tcp->ty0_t % tcp->dy));
                        tcp->tx0_t = pi[pino].poc.tx1;
                        tcp->ty0_t = pi[pino].poc.ty1;
                        break;
                    }
                    break;
                }
            }
            incr_top=1;
        } else {
            for(i=tppos; i>=0; i--) {
                switch(prog[i]) {
                case 'C':
                    pi[pino].poc.compno0 = tcp->comp_t-1;
                    pi[pino].poc.compno1 = tcp->comp_t;
                    break;
                case 'R':
                    pi[pino].poc.resno0 = tcp->res_t-1;
                    pi[pino].poc.resno1 = tcp->res_t;
                    break;
                case 'L':
                    pi[pino].poc.layno0 = tcp->lay_t-1;
                    pi[pino].poc.layno1 = tcp->lay_t;
                    break;
                case 'P':
                    switch(tcp->prg) {
                    case OPJ_LRCP:
                    case OPJ_RLCP:
                        pi[pino].poc.precno0 = tcp->prc_t-1;
                        pi[pino].poc.precno1 = tcp->prc_t;
                        break;
                    default:
                        pi[pino].poc.tx0 = (tcp->tx0_t - tcp->dx - (tcp->tx0_t % tcp->dx));
                        pi[pino].poc.tx1 = tcp->tx0_t ;
                        pi[pino].poc.ty0 = (tcp->ty0_t - tcp->dy - (tcp->ty0_t % tcp->dy));
                        pi[pino].poc.ty1 = tcp->ty0_t ;
                        break;
                    }
                    break;
                }
                if(incr_top==1) {
                    switch(prog[i]) {
                    case 'R':
                        if(tcp->res_t==tcp->resE) {
                            if(grk_pi_check_next_level(i-1,cp,tileno,pino,prog)) {
                                tcp->res_t = tcp->resS;
                                pi[pino].poc.resno0 = tcp->res_t;
                                pi[pino].poc.resno1 = tcp->res_t+1;
                                tcp->res_t+=1;
                                incr_top=1;
                            } else {
                                incr_top=0;
                            }
                        } else {
                            pi[pino].poc.resno0 = tcp->res_t;
                            pi[pino].poc.resno1 = tcp->res_t+1;
                            tcp->res_t+=1;
                            incr_top=0;
                        }
                        break;
                    case 'C':
                        if(tcp->comp_t ==tcp->compE) {
                            if(grk_pi_check_next_level(i-1,cp,tileno,pino,prog)) {
                                tcp->comp_t = tcp->compS;
                                pi[pino].poc.compno0 = tcp->comp_t;
                                pi[pino].poc.compno1 = tcp->comp_t+1;
                                tcp->comp_t+=1;
                                incr_top=1;
                            } else {
                                incr_top=0;
                            }
                        } else {
                            pi[pino].poc.compno0 = tcp->comp_t;
                            pi[pino].poc.compno1 = tcp->comp_t+1;
                            tcp->comp_t+=1;
                            incr_top=0;
                        }
                        break;
                    case 'L':
                        if(tcp->lay_t == tcp->layE) {
                            if(grk_pi_check_next_level(i-1,cp,tileno,pino,prog)) {
                                tcp->lay_t = tcp->layS;
                                pi[pino].poc.layno0 = tcp->lay_t;
                                pi[pino].poc.layno1 = tcp->lay_t+1;
                                tcp->lay_t+=1;
                                incr_top=1;
                            } else {
                                incr_top=0;
                            }
                        } else {
                            pi[pino].poc.layno0 = tcp->lay_t;
                            pi[pino].poc.layno1 = tcp->lay_t+1;
                            tcp->lay_t+=1;
                            incr_top=0;
                        }
                        break;
                    case 'P':
                        switch(tcp->prg) {
                        case OPJ_LRCP:
                        case OPJ_RLCP:
                            if(tcp->prc_t == tcp->prcE) {
                                if(grk_pi_check_next_level(i-1,cp,tileno,pino,prog)) {
                                    tcp->prc_t = tcp->prcS;
                                    pi[pino].poc.precno0 = tcp->prc_t;
                                    pi[pino].poc.precno1 = tcp->prc_t+1;
                                    tcp->prc_t+=1;
                                    incr_top=1;
                                } else {
                                    incr_top=0;
                                }
                            } else {
                                pi[pino].poc.precno0 = tcp->prc_t;
                                pi[pino].poc.precno1 = tcp->prc_t+1;
                                tcp->prc_t+=1;
                                incr_top=0;
                            }
                            break;
                        default:
                            if(tcp->tx0_t >= tcp->txE) {
                                if(tcp->ty0_t >= tcp->tyE) {
                                    if(grk_pi_check_next_level(i-1,cp,tileno,pino,prog)) {
                                        tcp->ty0_t = tcp->tyS;
                                        pi[pino].poc.ty0 = tcp->ty0_t;
                                        pi[pino].poc.ty1 = (uint32_t)(tcp->ty0_t + tcp->dy - (tcp->ty0_t % tcp->dy));
                                        tcp->ty0_t = pi[pino].poc.ty1;
                                        incr_top=1;
                                        resetX=1;
                                    } else {
                                        incr_top=0;
                                        resetX=0;
                                    }
                                } else {
                                    pi[pino].poc.ty0 = tcp->ty0_t;
                                    pi[pino].poc.ty1 = (tcp->ty0_t + tcp->dy - (tcp->ty0_t % tcp->dy));
                                    tcp->ty0_t = pi[pino].poc.ty1;
                                    incr_top=0;
                                    resetX=1;
                                }
                                if(resetX==1) {
                                    tcp->tx0_t = tcp->txS;
                                    pi[pino].poc.tx0 = tcp->tx0_t;
                                    pi[pino].poc.tx1 = (uint32_t)(tcp->tx0_t + tcp->dx- (tcp->tx0_t % tcp->dx));
                                    tcp->tx0_t = pi[pino].poc.tx1;
                                }
                            } else {
                                pi[pino].poc.tx0 = tcp->tx0_t;
                                pi[pino].poc.tx1 = (uint32_t)(tcp->tx0_t + tcp->dx- (tcp->tx0_t % tcp->dx));
                                tcp->tx0_t = pi[pino].poc.tx1;
                                incr_top=0;
                            }
                            break;
                        }
                        break;
                    }
                }
            }
        }
    }
}

void grk_pi_destroy(grk_pi_iterator_t *p_pi,
                    uint32_t p_nb_elements)
{
    uint32_t compno, pino;
    grk_pi_iterator_t *l_current_pi = p_pi;
    if (p_pi) {
        if (p_pi->include) {
            grk_free(p_pi->include);
            p_pi->include = nullptr;
        }
        for (pino = 0; pino < p_nb_elements; ++pino) {
            if(l_current_pi->comps) {
                grk_pi_comp_t *l_current_component = l_current_pi->comps;
                for (compno = 0; compno < l_current_pi->numcomps; compno++) {
                    if(l_current_component->resolutions) {
                        grk_free(l_current_component->resolutions);
                        l_current_component->resolutions = nullptr;
                    }

                    ++l_current_component;
                }
                grk_free(l_current_pi->comps);
                l_current_pi->comps = 0;
            }
            ++l_current_pi;
        }
        grk_free(p_pi);
    }
}



void grk_pi_update_encoding_parameters(	const opj_image_t *p_image,
                                        opj_cp_t *p_cp,
                                        uint32_t p_tile_no )
{
    /* encoding parameters to set */
    uint32_t l_max_res;
    uint32_t l_max_prec;
    uint32_t l_tx0,l_tx1,l_ty0,l_ty1;
    uint32_t l_dx_min,l_dy_min;

    /* pointers */
    grk_tcp_t *l_tcp = nullptr;

    /* preconditions */
    assert(p_cp != nullptr);
    assert(p_image != nullptr);
    assert(p_tile_no < p_cp->tw * p_cp->th);

    l_tcp = &(p_cp->tcps[p_tile_no]);

    /* get encoding parameters */
    opj_get_encoding_parameters(p_image,p_cp,p_tile_no,&l_tx0,&l_tx1,&l_ty0,&l_ty1,&l_dx_min,&l_dy_min,&l_max_prec,&l_max_res);

    if (l_tcp->POC) {
        grk_pi_update_encode_poc_and_final(p_cp,p_tile_no,l_tx0,l_tx1,l_ty0,l_ty1,l_max_prec,l_max_res,l_dx_min,l_dy_min);
    } else {
        grk_pi_update_encode_not_poc(p_cp,p_image->numcomps,p_tile_no,l_tx0,l_tx1,l_ty0,l_ty1,l_max_prec,l_max_res,l_dx_min,l_dy_min);
    }
}

bool grk_pi_next(grk_pi_iterator_t * pi)
{
    switch (pi->poc.prg) {
    case OPJ_LRCP:
        return grk_pi_next_lrcp(pi);
    case OPJ_RLCP:
        return grk_pi_next_rlcp(pi);
    case OPJ_RPCL:
        return grk_pi_next_rpcl(pi);
    case OPJ_PCRL:
        return grk_pi_next_pcrl(pi);
    case OPJ_CPRL:
        return grk_pi_next_cprl(pi);
    case OPJ_PROG_UNKNOWN:
        return false;
    }

    return false;
}
