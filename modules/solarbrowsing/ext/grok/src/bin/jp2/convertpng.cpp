// /*
// *    Copyright (C) 2016-2017 Grok Image Compression Inc.
// *
// *    This source code is free software: you can redistribute it and/or  modify
// *    it under the terms of the GNU Affero General Public License, version 3,
// *    as published by the Free Software Foundation.
// *
// *    This source code is distributed in the hope that it will be useful,
// *    but WITHOUT ANY WARRANTY; without even the implied warranty of
// *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// *    GNU Affero General Public License for more details.
// *
// *    You should have received a copy of the GNU Affero General Public License
// *    along with this program.  If not, see <http://www.gnu.org/licenses/>.
// *
// *
// *    This source code incorporates work covered by the following copyright and
// *    permission notice:
// *
//  * The copyright in this software is being made available under the 2-clauses
//  * BSD License, included below. This software may be subject to other third
//  * party and contributor rights, including patent rights, and no such rights
//  * are granted under this license.
//  *
//  * Copyright (c) 2002-2014, Universite catholique de Louvain (UCL), Belgium
//  * Copyright (c) 2002-2014, Professor Benoit Macq
//  * Copyright (c) 2001-2003, David Janssens
//  * Copyright (c) 2002-2003, Yannick Verschueren
//  * Copyright (c) 2003-2007, Francois-Olivier Devaux
//  * Copyright (c) 2003-2014, Antonin Descampe
//  * Copyright (c) 2005, Herve Drolon, FreeImage Team
//  * Copyright (c) 2006-2007, Parvatha Elangovan
//  * Copyright (c) 2015, Matthieu Darbois
//  * All rights reserved.
//  *
//  * Redistribution and use in source and binary forms, with or without
//  * modification, are permitted provided that the following conditions
//  * are met:
//  * 1. Redistributions of source code must retain the above copyright
//  *    notice, this list of conditions and the following disclaimer.
//  * 2. Redistributions in binary form must reproduce the above copyright
//  *    notice, this list of conditions and the following disclaimer in the
//  *    documentation and/or other materials provided with the distribution.
//  *
//  * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS `AS IS'
//  * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
//  * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
//  * ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
//  * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
//  * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
//  * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
//  * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
//  * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
//  * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
//  * POSSIBILITY OF SUCH DAMAGE.
//  */
// #include "opj_apps_config.h"

// #include <stdio.h>
// #include <stdlib.h>
// #include <string.h>
// #include <ctype.h>

// #include <zlib.h>
// #include <png.h>

// extern "C" {
// #include "openjpeg.h"
// #include "convert.h"
// #include "color.h"

// #ifdef OPJ_HAVE_LIBLCMS
// #include <lcms2.h>
// #endif

// }

// #include <memory>
// #include <iostream>
// #include <string>
// #include <codecvt>
// #include <cassert>
// #include <locale>

// #define PNG_MAGIC "\x89PNG\x0d\x0a\x1a\x0a"
// #define MAGIC_SIZE 8
// /* PNG allows bits per sample: 1, 2, 4, 8, 16 */


// static void convert_16u32s_C1R(const uint8_t* pSrc, int32_t* pDst, size_t length, bool invert)
// {
//     size_t i;
//     for (i = 0; i < length; i++) {
//         int32_t val0 = *pSrc++;
//         int32_t val1 = *pSrc++;
//         pDst[i] = INV(val0 << 8 | val1, 0xFFFF, invert);
//     }
// }

// opj_image_t *pngtoimage(const char *read_idf, opj_cparameters_t * params)
// {
//     png_structp  png = nullptr;
//     png_infop    info = nullptr;
//     int bit_depth, interlace_type,compression_type, filter_type;
//     uint32_t i;
//     png_uint_32  width=0U, height = 0U;
//     int color_type;
//     FILE *reader = nullptr;
//     uint8_t** rows = nullptr;
//     int32_t* row32s = nullptr;
//     /* j2k: */
//     opj_image_t *image = nullptr;
//     opj_image_cmptparm_t cmptparm[4];
//     uint32_t nr_comp;
//     uint8_t sigbuf[8];
//     convert_XXx32s_C1R cvtXXTo32s = NULL;
//     convert_32s_CXPX cvtCxToPx = NULL;
//     int32_t* planes[4];

//     if((reader = fopen(read_idf, "rb")) == NULL) {
//         fprintf(stderr,"pngtoimage: can not open %s\n",read_idf);
//         return NULL;
//     }

//     if(fread(sigbuf, 1, MAGIC_SIZE, reader) != MAGIC_SIZE
//             || memcmp(sigbuf, PNG_MAGIC, MAGIC_SIZE) != 0) {
//         fprintf(stderr,"pngtoimage: %s is no valid PNG file\n",read_idf);
//         goto fin;
//     }

//     if((png = png_create_read_struct(PNG_LIBPNG_VER_STRING,
//                                      NULL, NULL, NULL)) == NULL)
//         goto fin;

// 	// allow Microsoft/HP 3144-byte sRGB profile, normally skipped by library 
// 	// because it deems it broken. (a controversial decision)
// 	png_set_option(png, PNG_SKIP_sRGB_CHECK_PROFILE, PNG_OPTION_ON);


//     if((info = png_create_info_struct(png)) == NULL)
//         goto fin;

//     if(setjmp(png_jmpbuf(png)))
//         goto fin;



//     png_init_io(png, reader);
//     png_set_sig_bytes(png, MAGIC_SIZE);

//     png_read_info(png, info);

//     if(png_get_IHDR(png, info, &width, &height,
//                     &bit_depth, &color_type, &interlace_type,
//                     &compression_type, &filter_type) == 0)
//         goto fin;

// 	if (!width || !height)
// 		goto fin;

//     /* png_set_expand():
//      * expand paletted images to RGB, expand grayscale images of
//      * less than 8-bit depth to 8-bit depth, and expand tRNS chunks
//      * to alpha channels.
//      */
//     if(color_type == PNG_COLOR_TYPE_PALETTE) {
//         png_set_expand(png);
//     }

//     if(png_get_valid(png, info, PNG_INFO_tRNS)) {
//         png_set_expand(png);
//     }
//     /* We might want to expand background */
//     /*
//     if(png_get_valid(png, info, PNG_INFO_bKGD)) {
//     	png_color_16p bgnd;
//     	png_get_bKGD(png, info, &bgnd);
//     	png_set_background(png, bgnd, PNG_BACKGROUND_GAMMA_FILE, 1, 1.0);
//     }
//     */

	
// 	// See if iCCP chunk is present
// 	if (png_get_valid(png, info, PNG_INFO_iCCP))
// 	{
// 		uint32_t ProfileLen;
// 		png_bytep ProfileData;
// 		int  Compression;
// 		png_charp ProfileName;

// 		if (png_get_iCCP(png,
// 			info,
// 			&ProfileName,
// 			&Compression,
// 			&ProfileData,
// 			&ProfileLen) == PNG_INFO_iCCP) {
// 				image->icc_profile_len = ProfileLen;
// 				image->icc_profile_buf = (uint8_t*)malloc(ProfileLen);
// 				if (!image->icc_profile_buf)
// 					return NULL;
// 				memcpy(image->icc_profile_buf, ProfileData, ProfileLen);
// 		}
// 	}
// 	else {
// 		double fileGamma=0.0;
// 		if (png_get_gAMA(png, info, &fileGamma)) {
// 			fprintf(stdout, "Warning: input PNG contains gamma value of %f; this will not be stored in compressed image.\n", fileGamma);
// 		}
// 	}

//     png_read_update_info(png, info);
//     color_type = png_get_color_type(png, info);

//     switch (color_type) {
//     case PNG_COLOR_TYPE_GRAY:
//         nr_comp = 1;
//         break;
//     case PNG_COLOR_TYPE_GRAY_ALPHA:
//         nr_comp = 2;
//         break;
//     case PNG_COLOR_TYPE_RGB:
//         nr_comp = 3;
//         break;
//     case PNG_COLOR_TYPE_RGB_ALPHA:
//         nr_comp = 4;
//         break;
//     default:
//         fprintf(stderr,"pngtoimage: colortype %d is not supported\n", color_type);
//         goto fin;
//     }
//     cvtCxToPx = convert_32s_CXPX_LUT[nr_comp];
//     bit_depth = png_get_bit_depth(png, info);

//     switch (bit_depth) {
//     case 1:
//     case 2:
//     case 4:
//     case 8:
//         cvtXXTo32s = convert_XXu32s_C1R_LUT[bit_depth];
//         break;
//     case 16: /* 16 bpp is specific to PNG */
//         cvtXXTo32s = convert_16u32s_C1R;
//         break;
//     default:
//         fprintf(stderr,"pngtoimage: bit depth %d is not supported\n", bit_depth);
//         goto fin;
//     }


//     rows = (uint8_t**)calloc(height, sizeof(uint8_t*));
// 	if (rows == NULL) {
// 		fprintf(stderr, "pngtoimage: out of memory\n");
// 		goto fin;
// 	}
// 	for (i = 0; i < height; ++i) {
// 		rows[i] = (uint8_t*)malloc(png_get_rowbytes(png, info));
// 		if (!rows[i]) {
// 			fprintf(stderr, "pngtoimage: out of memory\n");
// 			goto fin;
// 		}
// 	}

//     png_read_image(png, rows);

//     /* Create image */
//     memset(cmptparm, 0, sizeof(cmptparm));
//     for(i = 0; i < nr_comp; ++i) {
//         cmptparm[i].prec = bit_depth;
//         cmptparm[i].sgnd = 0;
//         cmptparm[i].dx = params->subsampling_dx;
//         cmptparm[i].dy = params->subsampling_dy;
//         cmptparm[i].w = width;
//         cmptparm[i].h = height;
//     }

//     image = opj_image_create(nr_comp, &cmptparm[0], (nr_comp > 2U) ? OPJ_CLRSPC_SRGB : OPJ_CLRSPC_GRAY);
//     if(image == NULL) goto fin;
//     image->x0 = params->image_offset_x0;
//     image->y0 = params->image_offset_y0;
//     image->x1 = (image->x0 + (width  - 1) * params->subsampling_dx + 1 + image->x0);
//     image->y1 = (image->y0 + (height - 1) * params->subsampling_dy + 1 + image->y0);

//     row32s = (int32_t *)malloc((size_t)width * nr_comp * sizeof(int32_t));
//     if(row32s == NULL)
// 		goto fin;

//     /* Set alpha channel. Only non-premultiplied alpha is supported */
//     image->comps[nr_comp-1U].alpha = 1U - (nr_comp & 1U);

//     for(i = 0; i < nr_comp; i++) {
//         planes[i] = image->comps[i].data;
//     }

//     for(i = 0; i < height; ++i) {
//         cvtXXTo32s(rows[i], row32s, (size_t)width * nr_comp,false);
//         cvtCxToPx(row32s, planes, width);
//         planes[0] += width;
//         planes[1] += width;
//         planes[2] += width;
//         planes[3] += width;
//     }
// fin:
//     if(rows) {
// 		for (i = 0; i < height; ++i) {
// 			if (rows[i])
// 				free(rows[i]);
// 		}
//         free(rows);
//     }
//     if (row32s) {
//         free(row32s);
//     }
//     if(png)
//         png_destroy_read_struct(&png, &info, NULL);

//     fclose(reader);

//     return image;

// }/* pngtoimage() */


// static void convert_32s16u_C1R(const int32_t* pSrc, uint8_t* pDst, size_t length)
// {
//     size_t i;
//     for (i = 0; i < length; i++) {
//         uint32_t val = (uint32_t)pSrc[i];
//         *pDst++ = (uint8_t)(val >> 8);
//         *pDst++ = (uint8_t)val;
//     }
// }
// int imagetopng(opj_image_t * image, const char *write_idf, int32_t compressionLevel)
// {
//     FILE * volatile writer = NULL;
//     png_structp png = NULL;
//     png_infop info = NULL;
//     png_bytep volatile row_buf = NULL;
//     int nr_comp, color_type;
//     volatile int prec;
//     png_color_8 sig_bit;
//     int32_t const* planes[4];
//     int i;
//     int32_t* volatile buffer32s = NULL;

//     volatile int fails = 1;

//     memset(&sig_bit, 0, sizeof(sig_bit));
//     prec = (int)image->comps[0].prec;
//     planes[0] = image->comps[0].data;
//     nr_comp = (int)image->numcomps;

//     if (nr_comp > 4) {
//         nr_comp = 4;
//     }
//     for (i = 1; i < nr_comp; ++i) {
//         if (image->comps[0].dx != image->comps[i].dx) {
//             break;
//         }
//         if (image->comps[0].dy != image->comps[i].dy) {
//             break;
//         }
//         if (image->comps[0].prec != image->comps[i].prec) {
//             break;
//         }
//         if (image->comps[0].sgnd != image->comps[i].sgnd) {
//             break;
//         }
//         planes[i] = image->comps[i].data;
//     }
//     if (i != nr_comp) {
//         fprintf(stderr,"imagetopng: All components shall have the same subsampling, same bit depth, same sign.\n");
//         fprintf(stderr,"\tAborting\n");
//         return 1;
//     }
//     for (i = 0; i < nr_comp; ++i) {
//         clip_component(&(image->comps[i]), image->comps[0].prec);
//     }
//     if(prec > 8 && prec < 16) {
//         for (i = 0; i < nr_comp; ++i) {
//             scale_component(&(image->comps[i]), 16);
//         }
//         prec = 16;
//     } else if(prec < 8 && nr_comp > 1) { /* GRAY_ALPHA, RGB, RGB_ALPHA */
//         for (i = 0; i < nr_comp; ++i) {
//             scale_component(&(image->comps[i]), 8);
//         }
//         prec = 8;
//     } else if((prec > 1) && (prec < 8) && ((prec == 6) || ((prec & 1)==1))) { /* GRAY with non native precision */
//         if ((prec == 5) || (prec == 6)) {
//             prec = 8;
//         } else {
//             prec++;
//         }
//         for (i = 0; i < nr_comp; ++i) {
//             scale_component(&(image->comps[i]), (uint32_t)prec);
//         }
//     }

//     if(prec != 1 && prec != 2 && prec != 4 && prec != 8 && prec != 16) {
//         fprintf(stderr,"imagetopng: can not create %s\n\twrong bit_depth %d\n", write_idf, prec);
//         return fails;
//     }

//     writer = fopen(write_idf, "wb");

//     if(writer == NULL) return fails;

//     /* Create and initialize the png_struct with the desired error handler
//      * functions.  If you want to use the default stderr and longjump method,
//      * you can supply NULL for the last three parameters.  We also check that
//      * the library version is compatible with the one used at compile time,
//      * in case we are using dynamically linked libraries.  REQUIRED.
//      */
//     png = png_create_write_struct(PNG_LIBPNG_VER_STRING,
//                                   NULL, NULL, NULL);

// 	// allow Microsoft/HP 3144-byte sRGB profile, normally skipped by library 
// 	// because it deems it broken. (a controversial decision)
// 	png_set_option(png, PNG_SKIP_sRGB_CHECK_PROFILE, PNG_OPTION_ON);

//     if(png == NULL) goto fin;

//     /* Allocate/initialize the image information data.  REQUIRED
//      */
//     info = png_create_info_struct(png);

//     if(info == NULL) goto fin;

//     /* Set error handling.  REQUIRED if you are not supplying your own
//      * error handling functions in the png_create_write_struct() call.
//      */
//     if(setjmp(png_jmpbuf(png))) goto fin;

//     /* I/O initialization functions is REQUIRED
//      */
//     png_init_io(png, writer);

//     /* Set the image information here.  Width and height are up to 2^31,
//      * bit_depth is one of 1, 2, 4, 8, or 16, but valid values also depend on
//      * the color_type selected. color_type is one of PNG_COLOR_TYPE_GRAY,
//      * PNG_COLOR_TYPE_GRAY_ALPHA, PNG_COLOR_TYPE_PALETTE, PNG_COLOR_TYPE_RGB,
//      * or PNG_COLOR_TYPE_RGB_ALPHA.  interlace is either PNG_INTERLACE_NONE or
//      * PNG_INTERLACE_ADAM7, and the compression_type and filter_type MUST
//      * currently be PNG_COMPRESSION_TYPE_BASE and PNG_FILTER_TYPE_BASE.
//      * REQUIRED
//      *
//      * ERRORS:
//      *
//      * color_type == PNG_COLOR_TYPE_PALETTE && bit_depth > 8
//      * color_type == PNG_COLOR_TYPE_RGB && bit_depth < 8
//      * color_type == PNG_COLOR_TYPE_GRAY_ALPHA && bit_depth < 8
//      * color_type == PNG_COLOR_TYPE_RGB_ALPHA) && bit_depth < 8
//      *
//      */
//     png_set_compression_level(png, (compressionLevel == DECOMPRESS_COMPRESSION_LEVEL_DEFAULT) ? Z_BEST_COMPRESSION : compressionLevel);

//     if(nr_comp >= 3) { /* RGB(A) */
//         color_type = PNG_COLOR_TYPE_RGB;
//         sig_bit.red = sig_bit.green = sig_bit.blue = (png_byte)prec;
//     } else { /* GRAY(A) */
//         color_type = PNG_COLOR_TYPE_GRAY;
//         sig_bit.gray = (png_byte)prec;
//     }
//     if((nr_comp & 1) == 0) { /* ALPHA */
//         color_type |= PNG_COLOR_MASK_ALPHA;
//         sig_bit.alpha = (png_byte)prec;
//     }

//     png_set_IHDR(png, info, image->comps[0].w, image->comps[0].h, prec, color_type,
//                  PNG_INTERLACE_NONE, PNG_COMPRESSION_TYPE_BASE,  PNG_FILTER_TYPE_BASE);

//     png_set_sBIT(png, info, &sig_bit);
//     /* png_set_gamma(png, 2.2, 1./2.2); */
//     /* png_set_sRGB(png, info, PNG_sRGB_INTENT_PERCEPTUAL); */

// 	// Set iCCP chunk
// 	if (image->icc_profile_buf && image->icc_profile_len)
// 	{
// 		bool iccWasStored = false;
// 		auto in_prof = cmsOpenProfileFromMem(image->icc_profile_buf, image->icc_profile_len);
// 		if (in_prof) {
// 			cmsUInt32Number bufferSize = cmsGetProfileInfo(in_prof, cmsInfoDescription, cmsNoLanguage, cmsNoCountry, nullptr, 0);
// 			if (bufferSize) {
// 				size_t numWideChars = (bufferSize + 1) / 2;
// 				std::unique_ptr<wchar_t[]> description(new wchar_t[numWideChars]);
// 				cmsUInt32Number result = cmsGetProfileInfo(in_prof, cmsInfoDescription, cmsNoLanguage, cmsNoCountry, description.get(), bufferSize);
// 				cmsCloseProfile(in_prof);
// 				if (result) {
// 					std::string u8_conv = std::wstring_convert<std::codecvt_utf8_utf16<wchar_t>, wchar_t>{}.to_bytes(description.get());
// 					png_set_iCCP(png,
// 						info,
// 						(png_const_charp)(description.get()),
// 						PNG_COMPRESSION_TYPE_BASE,
// 						image->icc_profile_buf,
// 						image->icc_profile_len);
// 					iccWasStored = true;
// 				}
// 			}
// 		}
// 		if (!iccWasStored)
// 			fprintf(stdout, "imagetopng: Failed to store ICC profile.\n");

// 	}

// 	// handle libpng errors
// 	if (setjmp(png_jmpbuf(png))) {
// 		goto fin;
// 	}

//     png_write_info(png, info);

//     /* setup conversion */
//     {
//         size_t rowStride;
//         png_size_t png_row_size;

//         png_row_size = png_get_rowbytes(png, info);
//         rowStride = ((size_t)image->comps[0].w * (size_t)nr_comp * (size_t)prec + 7U) / 8U;
//         if (rowStride != (size_t)png_row_size) {
//             fprintf(stderr, "Invalid PNG row size\n");
//             goto fin;
//         }
//         row_buf = (png_bytep)malloc(png_row_size);
//         if (row_buf == NULL) {
//             fprintf(stderr, "Can't allocate memory for PNG row\n");
//             goto fin;
//         }
//         buffer32s = (int32_t*)malloc((size_t)image->comps[0].w * (size_t)nr_comp * sizeof(int32_t));
//         if (buffer32s == NULL) {
//             fprintf(stderr, "Can't allocate memory for interleaved 32s row\n");
//             goto fin;
//         }
//     }

//     /* convert */
//     {
//         size_t width= image->comps[0].w;
//         uint32_t y;
//         convert_32s_PXCX cvtPxToCx = convert_32s_PXCX_LUT[nr_comp];
//         convert_32sXXx_C1R cvt32sToPack = NULL;
//         int32_t adjust = image->comps[0].sgnd ? 1 << (prec - 1) : 0;
//         png_bytep row_buf_cpy = row_buf;
//         int32_t* buffer32s_cpy = buffer32s;

//         switch (prec) {
//         case 1:
//         case 2:
//         case 4:
//         case 8:
//             cvt32sToPack = convert_32sXXu_C1R_LUT[prec];
//             break;
//         case 16:
//             cvt32sToPack = convert_32s16u_C1R;
//             break;
//         default:
//             /* never here */
// 			return 1;
//             break;
//         }

//         for(y = 0; y < image->comps[0].h; ++y) {
//             cvtPxToCx(planes, buffer32s_cpy, width, adjust);
//             cvt32sToPack(buffer32s_cpy, row_buf_cpy, width * (size_t)nr_comp);
//             png_write_row(png, row_buf_cpy);
//             planes[0] += width;
//             planes[1] += width;
//             planes[2] += width;
//             planes[3] += width;
//         }
//     }

//     png_write_end(png, info);

//     fails = 0;

// fin:
//     if(png) {
//         png_destroy_write_struct(&png, &info);
//     }
//     if(row_buf) {
//         free(row_buf);
//     }
//     if(buffer32s) {
//         free(buffer32s);
//     }
//     fclose(writer);

//     if(fails) (void)remove(write_idf); /* ignore return value */

//     return fails;
// }/* imagetopng() */
