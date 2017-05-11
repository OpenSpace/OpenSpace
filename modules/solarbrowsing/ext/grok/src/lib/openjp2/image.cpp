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

#include "grk_includes.h"

opj_image_t* opj_image_create0(void)
{
    return (opj_image_t*)grk_calloc(1, sizeof(opj_image_t));
}

opj_image_t* OPJ_CALLCONV opj_image_create(uint32_t numcmpts, opj_image_cmptparm_t *cmptparms, OPJ_COLOR_SPACE clrspc)
{
    uint32_t compno;
    opj_image_t *image = NULL;

    image = (opj_image_t*) grk_calloc(1, sizeof(opj_image_t));
    if(image) {
        image->color_space = clrspc;
        image->numcomps = numcmpts;
        /* allocate memory for the per-component information */
        image->comps = (opj_image_comp_t*)grk_calloc(1,image->numcomps * sizeof(opj_image_comp_t));
        if(!image->comps) {
            /* TODO replace with event manager, breaks API */
            /* fprintf(stderr,"Unable to allocate memory for image.\n"); */
            opj_image_destroy(image);
            return NULL;
        }
        /* create the individual image components */
        for(compno = 0; compno < numcmpts; compno++) {
            opj_image_comp_t *comp = &image->comps[compno];
            comp->dx = cmptparms[compno].dx;
            comp->dy = cmptparms[compno].dy;
            comp->w = cmptparms[compno].w;
            comp->h = cmptparms[compno].h;
            comp->x0 = cmptparms[compno].x0;
            comp->y0 = cmptparms[compno].y0;
            comp->prec = cmptparms[compno].prec;
            comp->sgnd = cmptparms[compno].sgnd;
            if(!opj_image_single_component_data_alloc(comp)) {
                /* TODO replace with event manager, breaks API */
                /* fprintf(stderr,"Unable to allocate memory for image.\n"); */
                opj_image_destroy(image);
                return NULL;
            }
        }
    }

    return image;
}

void OPJ_CALLCONV opj_image_destroy(opj_image_t *image)
{
    if(image) {
        if(image->comps) {
            opj_image_all_components_data_free(image);
            grk_free(image->comps);
        }

        if(image->icc_profile_buf) {
            grk_free(image->icc_profile_buf);
			image->icc_profile_buf = nullptr;
        }

		if (image->iptc_buf) {
			grk_free(image->iptc_buf);
			image->iptc_buf = nullptr;
		}

		if (image->xmp_buf) {
			grk_free(image->xmp_buf);
			image->xmp_buf = nullptr;
		}

        grk_free(image);
    }
}

/**
 * Updates the components characteristics of the image from the coding parameters.
 *
 * @param p_image_header	the image header to update.
 * @param p_cp				the coding parameters from which to update the image.
 */
void opj_image_comp_header_update(opj_image_t * p_image_header, const struct opj_cp * p_cp)
{
    uint32_t i, l_width, l_height;
    uint32_t l_x0, l_y0, l_x1, l_y1;
    uint32_t l_comp_x0, l_comp_y0, l_comp_x1, l_comp_y1;
    opj_image_comp_t* l_img_comp = NULL;

    l_x0 = grk_max<uint32_t>(p_cp->tx0 , p_image_header->x0);
    l_y0 = grk_max<uint32_t>(p_cp->ty0 , p_image_header->y0);
    l_x1 = p_cp->tx0 + (p_cp->tw - 1U) * p_cp->tdx; /* validity of p_cp members used here checked in grk_j2k_read_siz. Can't overflow. */
    l_y1 = p_cp->ty0 + (p_cp->th - 1U) * p_cp->tdy; /* can't overflow */
    l_x1 = grk_min<uint32_t>(grk_uint_adds(l_x1, p_cp->tdx), p_image_header->x1); /* use add saturated to prevent overflow */
    l_y1 = grk_min<uint32_t>(grk_uint_adds(l_y1, p_cp->tdy), p_image_header->y1); /* use add saturated to prevent overflow */

    l_img_comp = p_image_header->comps;
    for	(i = 0; i < p_image_header->numcomps; ++i) {
        l_comp_x0 = grk_uint_ceildiv(l_x0, l_img_comp->dx);
        l_comp_y0 = grk_uint_ceildiv(l_y0, l_img_comp->dy);
        l_comp_x1 = grk_uint_ceildiv(l_x1, l_img_comp->dx);
        l_comp_y1 = grk_uint_ceildiv(l_y1, l_img_comp->dy);
        l_width   = grk_uint_ceildivpow2(l_comp_x1 - l_comp_x0, l_img_comp->decodeScaleFactor);
        l_height  = grk_uint_ceildivpow2(l_comp_y1 - l_comp_y0, l_img_comp->decodeScaleFactor);
        l_img_comp->w = l_width;
        l_img_comp->h = l_height;
        l_img_comp->x0 = l_comp_x0;
        l_img_comp->y0 = l_comp_y0;
        ++l_img_comp;
    }
}


/**
 * Copy only header of image and its component header (no data are copied)
 * if dest image have data, they will be freed
 *
 * @param	p_image_src		the src image
 * @param	p_image_dest	the dest image
 *
 */
void opj_copy_image_header(const opj_image_t* p_image_src, opj_image_t* p_image_dest)
{
    uint32_t compno;

    /* preconditions */
    assert(p_image_src != nullptr);
    assert(p_image_dest != nullptr);

    p_image_dest->x0 = p_image_src->x0;
    p_image_dest->y0 = p_image_src->y0;
    p_image_dest->x1 = p_image_src->x1;
    p_image_dest->y1 = p_image_src->y1;

    if (p_image_dest->comps) {
        opj_image_all_components_data_free(p_image_dest);
        grk_free(p_image_dest->comps);
        p_image_dest->comps = NULL;
    }

    p_image_dest->numcomps = p_image_src->numcomps;

    p_image_dest->comps = (opj_image_comp_t*) grk_malloc(p_image_dest->numcomps * sizeof(opj_image_comp_t));
    if (!p_image_dest->comps) {
        p_image_dest->comps = NULL;
        p_image_dest->numcomps = 0;
        return;
    }

    for (compno=0; compno < p_image_dest->numcomps; compno++) {
        memcpy( &(p_image_dest->comps[compno]),
                &(p_image_src->comps[compno]),
                sizeof(opj_image_comp_t));
        p_image_dest->comps[compno].data = NULL;
    }

    p_image_dest->color_space = p_image_src->color_space;
    p_image_dest->icc_profile_len = p_image_src->icc_profile_len;

    if (p_image_dest->icc_profile_len) {
        p_image_dest->icc_profile_buf = (uint8_t*)grk_malloc(p_image_dest->icc_profile_len);
        if (!p_image_dest->icc_profile_buf) {
            p_image_dest->icc_profile_buf = NULL;
            p_image_dest->icc_profile_len = 0;
            return;
        }
        memcpy( p_image_dest->icc_profile_buf,
                p_image_src->icc_profile_buf,
                p_image_src->icc_profile_len);
    } else
        p_image_dest->icc_profile_buf = NULL;

    return;
}

opj_image_t* OPJ_CALLCONV opj_image_tile_create(uint32_t numcmpts, opj_image_cmptparm_t *cmptparms, OPJ_COLOR_SPACE clrspc)
{
    uint32_t compno;
    opj_image_t *image = nullptr;

    image = (opj_image_t*) grk_calloc(1,sizeof(opj_image_t));
    if (image) {

        image->color_space = clrspc;
        image->numcomps = numcmpts;

        /* allocate memory for the per-component information */
        image->comps = (opj_image_comp_t*)grk_calloc(image->numcomps, sizeof(opj_image_comp_t));
        if (!image->comps) {
            opj_image_destroy(image);
            return nullptr;
        }

        /* create the individual image components */
        for(compno = 0; compno < numcmpts; compno++) {
            opj_image_comp_t *comp = &image->comps[compno];
            comp->dx = cmptparms[compno].dx;
            comp->dy = cmptparms[compno].dy;
            comp->w = cmptparms[compno].w;
            comp->h = cmptparms[compno].h;
            comp->x0 = cmptparms[compno].x0;
            comp->y0 = cmptparms[compno].y0;
            comp->prec = cmptparms[compno].prec;
            comp->sgnd = cmptparms[compno].sgnd;
            comp->data = 0;
        }
    }

    return image;
}
