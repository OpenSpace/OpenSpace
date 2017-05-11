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


#define INV_MASK_16 0xFFFF
#define INV_MASK_15 ((1<<15)-1)
#define INV_MASK_14 ((1<<14)-1)
#define INV_MASK_13 ((1<<13)-1)
#define INV_MASK_12 ((1<<12)-1)
#define INV_MASK_11 ((1<<11)-1)
#define INV_MASK_10 ((1<<10)-1)
#define INV_MASK_9 ((1<<9)-1)
#define INV_MASK_8 0xFF
#define INV_MASK_7 ((1<<7)-1)
#define INV_MASK_6 ((1<<6)-1)
#define INV_MASK_5 ((1<<5)-1)
#define INV_MASK_4 ((1<<4)-1)
#define INV_MASK_3 ((1<<3)-1)
#define INV_MASK_2 ((1<<2)-1)


#define INV(val, mask,invert)  ((invert) ? ((val)^(mask)) : (val))



extern "C" {

	/* Component precision clipping */
	void clip_component(opj_image_comp_t* component, uint32_t precision);
	/* Component precision scaling */
	void scale_component(opj_image_comp_t* component, uint32_t precision);

	/* planar / interleaved conversions */
	typedef void(*convert_32s_CXPX)(const int32_t* pSrc, int32_t* const* pDst, size_t length);
	extern const convert_32s_CXPX convert_32s_CXPX_LUT[5];
	typedef void(*convert_32s_PXCX)(int32_t const* const* pSrc, int32_t* pDst, size_t length, int32_t adjust);
	extern const convert_32s_PXCX convert_32s_PXCX_LUT[5];
	/* bit depth conversions */
	typedef void(*convert_XXx32s_C1R)(const uint8_t* pSrc, int32_t* pDst, size_t length,bool invert);
	extern const convert_XXx32s_C1R convert_XXu32s_C1R_LUT[9]; /* up to 8bpp */
	typedef void(*convert_32sXXx_C1R)(const int32_t* pSrc, uint8_t* pDst, size_t length);
	extern const convert_32sXXx_C1R convert_32sXXu_C1R_LUT[9]; /* up to 8bpp */


	/* TGA conversion */
	opj_image_t* tgatoimage(const char *filename, opj_cparameters_t *parameters);
	int imagetotga(opj_image_t * image, const char *outfile);

	/* BMP conversion */
	opj_image_t* bmptoimage(const char *filename, opj_cparameters_t *parameters);
	int imagetobmp(opj_image_t *image, const char *outfile);

	/* TIFF conversion*/
	opj_image_t* tiftoimage(const char *filename, opj_cparameters_t *parameters, bool applyICC);
	int imagetotif(opj_image_t *image, const char *outfile, uint32_t compression, bool verbose);
	void tiffSetErrorAndWarningHandlers(bool verbose);
	/**
	Load a single image component encoded in PGX file format
	@param filename Name of the PGX file to load
	@param parameters *List ?*
	@return Returns a greyscale image if successful, returns NULL otherwise
	*/
	opj_image_t* pgxtoimage(const char *filename, opj_cparameters_t *parameters);
	int imagetopgx(opj_image_t *image, const char *outfile);

	opj_image_t* pnmtoimage(const char *filename, opj_cparameters_t *parameters);
	int imagetopnm(opj_image_t *image, const char *outfile, int force_split);

	/* RAW conversion */
	int imagetoraw(opj_image_t * image, const char *outfile);
	int imagetorawl(opj_image_t * image, const char *outfile);
	opj_image_t* rawtoimage(const char *filename, opj_cparameters_t *parameters);
	opj_image_t* rawltoimage(const char *filename, opj_cparameters_t *parameters);

	/* PNG conversion*/
	extern int imagetopng(opj_image_t *image, const char *write_idf, int32_t compressionLevel);
	extern opj_image_t* pngtoimage(const char *filename, opj_cparameters_t *parameters);

}



