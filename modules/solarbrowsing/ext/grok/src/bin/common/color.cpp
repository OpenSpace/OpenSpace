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

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>

extern "C" {

#include "opj_apps_config.h"
#include "openjpeg.h"
#include "color.h"

#ifdef OPJ_HAVE_LIBLCMS
#include <lcms2.h>
#endif

}

static opj_image_t*  image_create(uint32_t numcmpts, uint32_t w, uint32_t h, uint32_t prec)
{
	if (!numcmpts)
		return NULL;

    opj_image_cmptparm_t* cmptparms = (opj_image_cmptparm_t*)calloc(numcmpts, sizeof(opj_image_cmptparm_t));
	if (!cmptparms)
		return NULL;
    opj_image_t* img = NULL;
    uint32_t compno=0U;
    for (compno = 0U; compno < numcmpts; ++compno) {
        memset(cmptparms + compno, 0, sizeof(opj_image_cmptparm_t));
        cmptparms[compno].dx = 1;
        cmptparms[compno].dy = 1;
        cmptparms[compno].w = w;
        cmptparms[compno].h = h;
        cmptparms[compno].x0 = 0U;
        cmptparms[compno].y0 = 0U;
        cmptparms[compno].prec = prec;
        cmptparms[compno].sgnd = 0U;
    }
    img = opj_image_create(numcmpts, (opj_image_cmptparm_t *)cmptparms, OPJ_CLRSPC_SRGB);
    free(cmptparms);
    return img;

}


static bool all_components_equal_subsampling(opj_image_t *image) {
	if (image->numcomps == 0)
		return true;

	uint32_t i;
	for (i = 1U; i < image->numcomps; ++i) {
		if (image->comps[0].dx != image->comps[i].dx) {
			break;
		}
		if (image->comps[0].dy != image->comps[i].dy) {
			break;
		}
	}
	if (i != image->numcomps) {
		fprintf(stderr, "Color conversion: all components must have the same subsampling.\n");
		return false;
	}
	return true;
}
/*--------------------------------------------------------
Matrix for sYCC, Amendment 1 to IEC 61966-2-1

Y :   0.299   0.587    0.114   :R
Cb:  -0.1687 -0.3312   0.5     :G
Cr:   0.5    -0.4187  -0.0812  :B

Inverse:

R: 1        -3.68213e-05    1.40199      :Y
G: 1.00003  -0.344125      -0.714128     :Cb - 2^(prec - 1)
B: 0.999823  1.77204       -8.04142e-06  :Cr - 2^(prec - 1)

-----------------------------------------------------------*/
static void sycc_to_rgb(int offset, int upb, int y, int cb, int cr,
                        int *out_r, int *out_g, int *out_b)
{
    int r, g, b;

    cb -= offset;
    cr -= offset;
    r = y + (int)(1.402 * (float)cr);
    if(r < 0) r = 0;
    else if(r > upb) r = upb;
    *out_r = r;

    g = y - (int)(0.344 * (float)cb + 0.714 * (float)cr);
    if(g < 0) g = 0;
    else if(g > upb) g = upb;
    *out_g = g;

    b = y + (int)(1.772 * (float)cb);
    if(b < 0) b = 0;
    else if(b > upb) b = upb;
    *out_b = b;
}

static void sycc444_to_rgb(opj_image_t *img)
{
	int *d0, *d1, *d2, *r, *g, *b;
	const int *y, *cb, *cr;
	size_t maxw, maxh, max, i;
	int offset, upb;
	opj_image_t* new_image = image_create(3, img->comps[0].w, img->comps[0].h, img->comps[0].prec);
	if (!new_image)
		return;

	upb = (int)img->comps[0].prec;
	offset = 1 << (upb - 1); upb = (1 << upb) - 1;

	maxw = (size_t)img->comps[0].w;
	maxh = (size_t)img->comps[0].h;
	max = maxw * maxh;

	y = img->comps[0].data;
	cb = img->comps[1].data;
	cr = img->comps[2].data;

	d0 = r = new_image->comps[0].data;
	d1 = g = new_image->comps[1].data;
	d2 = b = new_image->comps[2].data;

	new_image->comps[0].data = NULL;
	new_image->comps[1].data = NULL;
	new_image->comps[2].data = NULL;

	opj_image_destroy(new_image);
	new_image = NULL;
	
	for (i = 0U; i < max; ++i)
	{
		sycc_to_rgb(offset, upb, *y, *cb, *cr, r, g, b);
		++y; ++cb; ++cr; ++r; ++g; ++b;
	}
	opj_image_all_components_data_free(img);
	img->comps[0].data = d0;
	img->comps[1].data = d1;
	img->comps[2].data = d2;
	img->color_space = OPJ_CLRSPC_SRGB;
	return;
}/* sycc444_to_rgb() */


static void sycc422_to_rgb(opj_image_t *img)
{
	int *d0, *d1, *d2, *r, *g, *b;
	const int *y, *cb, *cr;
	size_t maxw, maxh, max, offx, loopmaxw;
	int offset, upb;
	size_t i;

	opj_image_t* new_image = image_create(3, img->comps[0].w, img->comps[0].h, img->comps[0].prec);
	if (!new_image)
		return;


	upb = (int)img->comps[0].prec;
	offset = 1 << (upb - 1); upb = (1 << upb) - 1;

	maxw = (size_t)img->comps[0].w; maxh = (size_t)img->comps[0].h;
	max = maxw * maxh;

	y = img->comps[0].data;
	cb = img->comps[1].data;
	cr = img->comps[2].data;

	d0 = r = new_image->comps[0].data;
	d1 = g = new_image->comps[1].data;
	d2 = b = new_image->comps[2].data;

	new_image->comps[0].data = NULL;
	new_image->comps[1].data = NULL;
	new_image->comps[2].data = NULL;

	opj_image_destroy(new_image);
	new_image = NULL;


	/* if img->x0 is odd, then first column shall use Cb/Cr = 0 */
	offx = img->x0 & 1U;
	loopmaxw = maxw - offx;

	for (i = 0U; i < maxh; ++i)
	{
		size_t j;

		if (offx > 0U) {
			sycc_to_rgb(offset, upb, *y, 0, 0, r, g, b);
			++y; ++r; ++g; ++b;
		}

		for (j = 0U; j < (loopmaxw & ~(size_t)1U); j += 2U)
		{
			sycc_to_rgb(offset, upb, *y, *cb, *cr, r, g, b);
			++y; ++r; ++g; ++b;
			sycc_to_rgb(offset, upb, *y, *cb, *cr, r, g, b);
			++y; ++r; ++g; ++b; ++cb; ++cr;
		}
		if (j < loopmaxw) {
			sycc_to_rgb(offset, upb, *y, *cb, *cr, r, g, b);
			++y; ++r; ++g; ++b; ++cb; ++cr;
		}
	}
	opj_image_all_components_data_free(img);

	img->comps[0].data = d0;
	img->comps[1].data = d1;
	img->comps[2].data = d2;

	img->comps[1].w = img->comps[2].w = img->comps[0].w;
	img->comps[1].h = img->comps[2].h = img->comps[0].h;
	img->comps[1].dx = img->comps[2].dx = img->comps[0].dx;
	img->comps[1].dy = img->comps[2].dy = img->comps[0].dy;
	img->color_space = OPJ_CLRSPC_SRGB;
	return;

}/* sycc422_to_rgb() */


static void sycc420_to_rgb(opj_image_t *img)
{
	int *d0, *d1, *d2, *r, *g, *b, *nr, *ng, *nb;
	const int *y, *cb, *cr, *ny;
	size_t maxw, maxh, max, offx, loopmaxw, offy, loopmaxh;
	int offset, upb;
	size_t i;
	opj_image_t* new_image = image_create(3, img->comps[0].w, img->comps[0].h, img->comps[0].prec);
	if (!new_image)
		return;

	upb = (int)img->comps[0].prec;
	offset = 1 << (upb - 1); upb = (1 << upb) - 1;

	maxw = (size_t)img->comps[0].w;
	maxh = (size_t)img->comps[0].h;
	max = maxw * maxh;

	y = img->comps[0].data;
	cb = img->comps[1].data;
	cr = img->comps[2].data;

	d0 = r = new_image->comps[0].data;
	d1 = g = new_image->comps[1].data;
	d2 = b = new_image->comps[2].data;

	new_image->comps[0].data = NULL;
	new_image->comps[1].data = NULL;
	new_image->comps[2].data = NULL;

	opj_image_destroy(new_image);
	new_image = NULL;

	/* if img->x0 is odd, then first column shall use Cb/Cr = 0 */
	offx = img->x0 & 1U;
	loopmaxw = maxw - offx;
	/* if img->y0 is odd, then first line shall use Cb/Cr = 0 */
	offy = img->y0 & 1U;
	loopmaxh = maxh - offy;

	if (offy > 0U) {
		size_t j;

		for (j = 0U; j < maxw; ++j)
		{
			sycc_to_rgb(offset, upb, *y, 0, 0, r, g, b);
			++y; ++r; ++g; ++b;
		}
	}

	for (i = 0U; i < (loopmaxh & ~(size_t)1U); i += 2U)
	{
		size_t j;

		ny = y + maxw;
		nr = r + maxw; ng = g + maxw; nb = b + maxw;

		if (offx > 0U) {
			sycc_to_rgb(offset, upb, *y, 0, 0, r, g, b);
			++y; ++r; ++g; ++b;
			sycc_to_rgb(offset, upb, *ny, *cb, *cr, nr, ng, nb);
			++ny; ++nr; ++ng; ++nb;
		}

		for (j = 0U; j < (loopmaxw & ~(size_t)1U); j += 2U)
		{
			sycc_to_rgb(offset, upb, *y, *cb, *cr, r, g, b);
			++y; ++r; ++g; ++b;
			sycc_to_rgb(offset, upb, *y, *cb, *cr, r, g, b);
			++y; ++r; ++g; ++b;

			sycc_to_rgb(offset, upb, *ny, *cb, *cr, nr, ng, nb);
			++ny; ++nr; ++ng; ++nb;
			sycc_to_rgb(offset, upb, *ny, *cb, *cr, nr, ng, nb);
			++ny; ++nr; ++ng; ++nb; ++cb; ++cr;
		}
		if (j < loopmaxw)
		{
			sycc_to_rgb(offset, upb, *y, *cb, *cr, r, g, b);
			++y; ++r; ++g; ++b;

			sycc_to_rgb(offset, upb, *ny, *cb, *cr, nr, ng, nb);
			++ny; ++nr; ++ng; ++nb; ++cb; ++cr;
		}
		y += maxw; r += maxw; g += maxw; b += maxw;
	}
	if (i < loopmaxh)
	{
		size_t j;

		for (j = 0U; j < (maxw & ~(size_t)1U); j += 2U)
		{
			sycc_to_rgb(offset, upb, *y, *cb, *cr, r, g, b);

			++y; ++r; ++g; ++b;

			sycc_to_rgb(offset, upb, *y, *cb, *cr, r, g, b);

			++y; ++r; ++g; ++b; ++cb; ++cr;
		}
		if (j < maxw)
		{
			sycc_to_rgb(offset, upb, *y, *cb, *cr, r, g, b);
		}
	}

	opj_image_all_components_data_free(img);
	img->comps[0].data = d0;
	img->comps[1].data = d1;
	img->comps[2].data = d2;

	img->comps[1].w = img->comps[2].w = img->comps[0].w;
	img->comps[1].h = img->comps[2].h = img->comps[0].h;
	img->comps[1].dx = img->comps[2].dx = img->comps[0].dx;
	img->comps[1].dy = img->comps[2].dy = img->comps[0].dy;
	img->color_space = OPJ_CLRSPC_SRGB;
	return;

}/* sycc420_to_rgb() */

void color_sycc_to_rgb(opj_image_t *img)
{
    if(img->numcomps < 3) {
        img->color_space = OPJ_CLRSPC_GRAY;
        return;
    }

    if((img->comps[0].dx == 1)
            && (img->comps[1].dx == 2)
            && (img->comps[2].dx == 2)
            && (img->comps[0].dy == 1)
            && (img->comps[1].dy == 2)
            && (img->comps[2].dy == 2)) { /* horizontal and vertical sub-sample */
        sycc420_to_rgb(img);
    } else if((img->comps[0].dx == 1)
              && (img->comps[1].dx == 2)
              && (img->comps[2].dx == 2)
              && (img->comps[0].dy == 1)
              && (img->comps[1].dy == 1)
              && (img->comps[2].dy == 1)) { /* horizontal sub-sample only */
        sycc422_to_rgb(img);
    } else if((img->comps[0].dx == 1)
              && (img->comps[1].dx == 1)
              && (img->comps[2].dx == 1)
              && (img->comps[0].dy == 1)
              && (img->comps[1].dy == 1)
              && (img->comps[2].dy == 1)) { /* no sub-sample */
        sycc444_to_rgb(img);
    } else {
        fprintf(stderr,"%s:%d:color_sycc_to_rgb\n\tCAN NOT CONVERT\n", __FILE__,__LINE__);
        return;
    }
    img->color_space = OPJ_CLRSPC_SRGB;

}/* color_sycc_to_rgb() */

#if defined(OPJ_HAVE_LIBLCMS)

/*#define DEBUG_PROFILE*/
void color_apply_icc_profile(opj_image_t *image, bool forceRGB)
{
	cmsHPROFILE in_prof = nullptr , out_prof=nullptr;
    cmsHTRANSFORM transform = NULL;
    cmsColorSpaceSignature in_space, out_space;
    cmsUInt32Number intent, in_type, out_type, nr_samples;
    int prec, i, max, max_w, max_h;
    OPJ_COLOR_SPACE oldspace;
    opj_image_t* new_image = NULL;

	if (image->numcomps == 0 || !all_components_equal_subsampling(image))
		return;

    in_prof = cmsOpenProfileFromMem(image->icc_profile_buf, image->icc_profile_len);
#ifdef DEBUG_PROFILE
    FILE *icm = fopen("debug.icm","wb");
    fwrite( image->icc_profile_buf,1, image->icc_profile_len,icm);
    fclose(icm);
#endif

    if(in_prof == NULL)
		return;

    in_space = cmsGetPCS(in_prof);
    out_space = cmsGetColorSpace(in_prof);
    intent = cmsGetHeaderRenderingIntent(in_prof);


    max_w = (int)image->comps[0].w;
    max_h = (int)image->comps[0].h;

	if (!max_w || !max_h)
		goto cleanup;

    prec = (int)image->comps[0].prec;
    oldspace = image->color_space;

    if(out_space == cmsSigRgbData) { /* enumCS 16 */
		unsigned int i, nr_comp = image->numcomps;

		if (nr_comp > 4) {
			nr_comp = 4;
		}
		for (i = 1; i < nr_comp; ++i) { 
			if (image->comps[0].dx != image->comps[i].dx) 
				break;
			if (image->comps[0].dy != image->comps[i].dy) 
				break;
			if (image->comps[0].prec != image->comps[i].prec)
				break;
			if (image->comps[0].sgnd != image->comps[i].sgnd)
				break;
		}
		if (i != nr_comp)
			goto cleanup;

		if( prec <= 8 ) {
            in_type = TYPE_RGB_8;
            out_type = TYPE_RGB_8;
        } else {
            in_type = TYPE_RGB_16;
            out_type = TYPE_RGB_16;
        }
        out_prof = cmsCreate_sRGBProfile();
        image->color_space = OPJ_CLRSPC_SRGB;
    } else if(out_space == cmsSigGrayData) { /* enumCS 17 */
        in_type = TYPE_GRAY_8;
        out_type = TYPE_RGB_8;
        out_prof = cmsCreate_sRGBProfile();
		if (forceRGB)
			image->color_space = OPJ_CLRSPC_SRGB;
		else 
			image->color_space = OPJ_CLRSPC_GRAY;
    } else if(out_space == cmsSigYCbCrData) { /* enumCS 18 */
        in_type = TYPE_YCbCr_16;
        out_type = TYPE_RGB_16;
        out_prof = cmsCreate_sRGBProfile();
        image->color_space = OPJ_CLRSPC_SRGB;
    } else {
#ifdef DEBUG_PROFILE
        fprintf(stderr,"%s:%d: color_apply_icc_profile\n\tICC Profile has unknown "
                "output colorspace(%#x)(%c%c%c%c)\n\tICC Profile ignored.\n",
                __FILE__,__LINE__,out_space,
                (out_space>>24) & 0xff,(out_space>>16) & 0xff,
                (out_space>>8) & 0xff, out_space & 0xff);
#endif
        return;
    }

#ifdef DEBUG_PROFILE
    fprintf(stderr,"%s:%d:color_apply_icc_profile\n\tchannels(%d) prec(%d) w(%d) h(%d)"
            "\n\tprofile: in(%p) out(%p)\n",__FILE__,__LINE__,image->numcomps,prec,
            max_w,max_h, (void*)in_prof,(void*)out_prof);

    fprintf(stderr,"\trender_intent (%u)\n\t"
            "color_space: in(%#x)(%c%c%c%c)   out:(%#x)(%c%c%c%c)\n\t"
            "       type: in(%u)              out:(%u)\n",
            intent,
            in_space,
            (in_space>>24) & 0xff,(in_space>>16) & 0xff,
            (in_space>>8) & 0xff, in_space & 0xff,

            out_space,
            (out_space>>24) & 0xff,(out_space>>16) & 0xff,
            (out_space>>8) & 0xff, out_space & 0xff,

            in_type,out_type
           );
#else
    (void)prec;
    (void)in_space;
#endif /* DEBUG_PROFILE */

    transform = cmsCreateTransform(in_prof, in_type,
                                   out_prof, out_type, intent, 0);

    cmsCloseProfile(in_prof);
	in_prof = nullptr;
    cmsCloseProfile(out_prof);
	out_prof = nullptr;


    if(transform == NULL) {
#ifdef DEBUG_PROFILE
        fprintf(stderr,"%s:%d:color_apply_icc_profile\n\tcmsCreateTransform failed. "
                "ICC Profile ignored.\n",__FILE__,__LINE__);
#endif
        image->color_space = oldspace;
        return;
    }

    if(image->numcomps > 2) { /* RGB, RGBA */
        if( prec <= 8 ) {
			int *r=nullptr, *g=nullptr, *b=nullptr;
            unsigned char *inbuf=nullptr, *outbuf=nullptr, *in=nullptr, *out=nullptr;
            max = max_w * max_h;
            nr_samples = (cmsUInt32Number)max * 3 * (cmsUInt32Number)sizeof(unsigned char);
            in = inbuf = (unsigned char*)malloc(nr_samples);
			if (!in) {
				goto cleanup;
			}
            out = outbuf = (unsigned char*)malloc(nr_samples);
			if (!out) {
				free(inbuf);
				goto cleanup;
			}

            r = image->comps[0].data;
            g = image->comps[1].data;
            b = image->comps[2].data;

            for(i = 0; i < max; ++i) {
                *in++ = (unsigned char)*r++;
                *in++ = (unsigned char)*g++;
                *in++ = (unsigned char)*b++;
            }

            cmsDoTransform(transform, inbuf, outbuf, (cmsUInt32Number)max);

            r = image->comps[0].data;
            g = image->comps[1].data;
            b = image->comps[2].data;

            for(i = 0; i < max; ++i) {
                *r++ = (int)*out++;
                *g++ = (int)*out++;
                *b++ = (int)*out++;
            }
			free(inbuf);
			free(outbuf);
        } else {
			int *r = nullptr, *g = nullptr, *b = nullptr;
            unsigned short *inbuf=nullptr, *outbuf=nullptr, *in=nullptr, *out=nullptr;
            max = max_w * max_h;
            nr_samples = (cmsUInt32Number)max * 3 * (cmsUInt32Number)sizeof(unsigned short);
            in = inbuf = (unsigned short*)malloc(nr_samples);
			if (!in)
				goto cleanup;
            out = outbuf = (unsigned short*)malloc(nr_samples);
			if (!out) {
				free(inbuf);
				goto cleanup;
			}

            r = image->comps[0].data;
            g = image->comps[1].data;
            b = image->comps[2].data;

            for(i = 0; i < max; ++i) {
                *in++ = (unsigned short)*r++;
                *in++ = (unsigned short)*g++;
                *in++ = (unsigned short)*b++;
            }

            cmsDoTransform(transform, inbuf, outbuf, (cmsUInt32Number)max);

            r = image->comps[0].data;
            g = image->comps[1].data;
            b = image->comps[2].data;

            for(i = 0; i < max; ++i) {
                *r++ = (int)*out++;
                *g++ = (int)*out++;
                *b++ = (int)*out++;
            }
            free(inbuf);
            free(outbuf);
        }
    } else { /* GRAY, GRAYA */
		int *r = nullptr;
		int *g = nullptr;
		int *b = nullptr;
		unsigned char *in = nullptr;
		unsigned char * inbuf = nullptr;
		unsigned char *out = nullptr;
		unsigned char *outbuf = nullptr;

        max = max_w * max_h;
        nr_samples = (cmsUInt32Number)max * 3 * sizeof(unsigned char);
		opj_image_comp_t *comps = (opj_image_comp_t*)realloc(image->comps, (image->numcomps + 2) * sizeof(opj_image_comp_t));
		if (!comps)
			goto cleanup;
		image->comps = comps;

        in = inbuf = (unsigned char*)malloc(nr_samples);
		if (!in)
			goto cleanup;
        out = outbuf = (unsigned char*)malloc(nr_samples);
		if (!out) {
			free(inbuf);
			goto cleanup;
		}

        new_image = image_create(2, image->comps[0].w, image->comps[0].h, image->comps[0].prec);
		if (!new_image) {
			free(inbuf);
			free(outbuf);
			goto cleanup;
		}

        if(image->numcomps == 2)
            image->comps[3] = image->comps[1];

        image->comps[1] = image->comps[0];
        image->comps[2] = image->comps[0];

        image->comps[1].data = new_image->comps[0].data;
        image->comps[2].data = new_image->comps[1].data;

        new_image->comps[0].data= NULL;
        new_image->comps[1].data = NULL;

        opj_image_destroy(new_image);
        new_image = NULL;

		if (forceRGB)
			image->numcomps += 2;

        r = image->comps[0].data;
        for(i = 0; i < max; ++i) {
            *in++ = (unsigned char)*r++;
        }
        cmsDoTransform(transform, inbuf, outbuf, (cmsUInt32Number)max);

        r = image->comps[0].data;
        g = image->comps[1].data;
        b = image->comps[2].data;

        for(i = 0; i < max; ++i) {
            *r++ = (int)*out++;
			if (forceRGB) {
				*g++ = (int)*out++;
				*b++ = (int)*out++;
			}
			else { //just skip green and blue channels
				out += 2;
			}
        }
        free(inbuf);
        free(outbuf);

    }/* if(image->numcomps */
cleanup:
	if (in_prof)
		cmsCloseProfile(in_prof);
	if (out_prof)
		cmsCloseProfile(out_prof);
	if (transform)
		cmsDeleteTransform(transform);
}/* color_apply_icc_profile() */

void color_cielab_to_rgb(opj_image_t *image)
{
    int *row;
    int enumcs, numcomps;

    image->color_space = OPJ_CLRSPC_SRGB;

    numcomps = (int)image->numcomps;

    if(numcomps != 3) {
        fprintf(stderr,"%s:%d:\n\tnumcomps %d not handled. Quitting.\n",
                __FILE__,__LINE__,numcomps);
        return;
    }

	if (image->numcomps == 0 || !all_components_equal_subsampling(image))
		return;

    row = (int*)image->icc_profile_buf;
    enumcs = row[0];

    if(enumcs == 14) { /* CIELab */
        int *L, *a, *b, *red, *green, *blue;
        int *src0, *src1, *src2, *dst0, *dst1, *dst2;
        double rl, ol, ra, oa, rb, ob, prec0, prec1, prec2;
        double minL, maxL, mina, maxa, minb, maxb;
        unsigned int default_type;
        unsigned int i, max;
        cmsHPROFILE in, out;
        cmsHTRANSFORM transform;
        cmsUInt16Number RGB[3];
        cmsCIELab Lab;
        opj_image_t* new_image = image_create(3, image->comps[0].w, image->comps[0].h, image->comps[0].prec);

        in = cmsCreateLab4Profile(NULL);
        out = cmsCreate_sRGBProfile();

        transform = cmsCreateTransform(in, TYPE_Lab_DBL, out, TYPE_RGB_16, INTENT_PERCEPTUAL, 0);

        cmsCloseProfile(in);
        cmsCloseProfile(out);

        if(transform == NULL) {
            return;
        }
        prec0 = (double)image->comps[0].prec;
        prec1 = (double)image->comps[1].prec;
        prec2 = (double)image->comps[2].prec;

        default_type = (unsigned int)row[1];

        if(default_type == 0x44454600) { /* DEF : default */
            rl = 100;
            ra = 170;
            rb = 200;
            ol = 0;
            oa = pow(2, prec1 - 1);
            ob = pow(2, prec2 - 2) +  pow(2, prec2 - 3);
        } else {
            rl = row[2];
            ra = row[4];
            rb = row[6];
            ol = row[3];
            oa = row[5];
            ob = row[7];
        }

        L = src0 = image->comps[0].data;
        a = src1 = image->comps[1].data;
        b = src2 = image->comps[2].data;

        max = image->comps[0].w * image->comps[0].h;

        red = dst0	= new_image->comps[0].data;
        green = dst1 = new_image->comps[1].data;
        blue = dst2	 = new_image->comps[2].data;

        new_image->comps[0].data=NULL;
        new_image->comps[1].data=NULL;
        new_image->comps[2].data=NULL;

        opj_image_destroy(new_image);
        new_image = NULL;

        minL = -(rl * ol) / (pow(2, prec0) - 1);
        maxL = minL + rl;

        mina = -(ra * oa)/(pow(2, prec1)-1);
        maxa = mina + ra;

        minb = -(rb * ob)/(pow(2, prec2)-1);
        maxb = minb + rb;

        for(i = 0; i < max; ++i) {
            Lab.L = minL + (double)(*L) * (maxL - minL)/(pow(2, prec0)-1);
            ++L;
            Lab.a = mina + (double)(*a) * (maxa - mina)/(pow(2, prec1)-1);
            ++a;
            Lab.b = minb + (double)(*b) * (maxb - minb)/(pow(2, prec2)-1);
            ++b;

            cmsDoTransform(transform, &Lab, RGB, 1);

            *red++ = RGB[0];
            *green++ = RGB[1];
            *blue++ = RGB[2];
        }
        cmsDeleteTransform(transform);
        opj_image_all_components_data_free(image);
        image->comps[0].data = dst0;
        image->comps[1].data = dst1;
        image->comps[2].data = dst2;

        image->color_space = OPJ_CLRSPC_SRGB;
        image->comps[0].prec = 16;
        image->comps[1].prec = 16;
        image->comps[2].prec = 16;

        return;
    }

    fprintf(stderr,"%s:%d:\n\tenumCS %d not handled. Ignoring.\n", __FILE__,__LINE__, enumcs);
}/* color_apply_conversion() */

#endif /* OPJ_HAVE_LIBLCMS */


int color_cmyk_to_rgb(opj_image_t *image)
{
    float C, M, Y, K;
    float sC, sM, sY, sK;
    unsigned int w, h, max, i;

    w = image->comps[0].w;
    h = image->comps[0].h;

    if( (image->numcomps < 4)  || !all_components_equal_subsampling(image))
		return 1;


    max = w * h;

    sC = 1.0F / (float)((1 << image->comps[0].prec) - 1);
    sM = 1.0F / (float)((1 << image->comps[1].prec) - 1);
    sY = 1.0F / (float)((1 << image->comps[2].prec) - 1);
    sK = 1.0F / (float)((1 << image->comps[3].prec) - 1);

    for(i = 0; i < max; ++i) {
        /* CMYK values from 0 to 1 */
        C = (float)(image->comps[0].data[i]) * sC;
        M = (float)(image->comps[1].data[i]) * sM;
        Y = (float)(image->comps[2].data[i]) * sY;
        K = (float)(image->comps[3].data[i]) * sK;

        /* Invert all CMYK values */
        C = 1.0F - C;
        M = 1.0F - M;
        Y = 1.0F - Y;
        K = 1.0F - K;

        /* CMYK -> RGB : RGB results from 0 to 255 */
        image->comps[0].data[i] = (int)(255.0F * C * K); /* R */
        image->comps[1].data[i] = (int)(255.0F * M * K); /* G */
        image->comps[2].data[i] = (int)(255.0F * Y * K); /* B */
    }

    opj_image_single_component_data_free(image->comps + 3);
    image->comps[0].prec = 8;
    image->comps[1].prec = 8;
    image->comps[2].prec = 8;
    image->numcomps -= 1;
    image->color_space = OPJ_CLRSPC_SRGB;

    for (i = 3; i < image->numcomps; ++i) {
        memcpy(&(image->comps[i]), &(image->comps[i+1]), sizeof(image->comps[i]));
    }

	return 0;

}/* color_cmyk_to_rgb() */

/*
 * This code has been adopted from sjpx_openjpeg.c of ghostscript
 */
int color_esycc_to_rgb(opj_image_t *image)
{
    int y, cb, cr, sign1, sign2, val;
    unsigned int w, h, max, i;
    int flip_value = (1 << (image->comps[0].prec-1));
    int max_value = (1 << image->comps[0].prec) - 1;

    if( (image->numcomps < 3)  || !all_components_equal_subsampling(image))
		return 1;
	
	w = image->comps[0].w;
    h = image->comps[0].h;

    sign1 = (int)image->comps[1].sgnd;
    sign2 = (int)image->comps[2].sgnd;

    max = w * h;

    for(i = 0; i < max; ++i) {

        y = image->comps[0].data[i];
        cb = image->comps[1].data[i];
        cr = image->comps[2].data[i];

        if( !sign1) cb -= flip_value;
        if( !sign2) cr -= flip_value;

        val = (int)
              ((float)y - (float)0.0000368 * (float)cb
               + (float)1.40199 * (float)cr + (float)0.5);

        if(val > max_value) val = max_value;
        else if(val < 0) val = 0;
        image->comps[0].data[i] = val;

        val = (int)
              ((float)1.0003 * (float)y - (float)0.344125 * (float)cb
               - (float)0.7141128 * (float)cr + (float)0.5);

        if(val > max_value) val = max_value;
        else if(val < 0) val = 0;
        image->comps[1].data[i] = val;

        val = (int)
              ((float)0.999823 * (float)y + (float)1.77204 * (float)cb
               - (float)0.000008 *(float)cr + (float)0.5);

        if(val > max_value) val = max_value;
        else if(val < 0) val = 0;
        image->comps[2].data[i] = val;
    }
    image->color_space = OPJ_CLRSPC_SRGB;
	return 0;

}/* color_esycc_to_rgb() */
