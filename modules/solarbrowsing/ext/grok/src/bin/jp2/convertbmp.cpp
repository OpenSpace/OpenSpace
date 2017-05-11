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
#include "opj_apps_config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "openjpeg.h"
#include "convert.h"

typedef struct {
    uint16_t bfType;      /* 'BM' for Bitmap (19776) */
    uint32_t bfSize;      /* Size of the file        */
    uint16_t bfReserved1; /* Reserved : 0            */
    uint16_t bfReserved2; /* Reserved : 0            */
    uint32_t bfOffBits;   /* Offset                  */
} OPJ_BITMAPFILEHEADER;

typedef struct {
    uint32_t biSize;             /* Size of the structure in bytes */
    uint32_t biWidth;            /* Width of the image in pixels */
    uint32_t biHeight;           /* Heigth of the image in pixels */
    uint16_t biPlanes;           /* 1 */
    uint16_t biBitCount;         /* Number of color bits by pixels */
    uint32_t biCompression;      /* Type of encoding 0: none 1: RLE8 2: RLE4 */
    uint32_t biSizeImage;        /* Size of the image in bytes */
    uint32_t biXpelsPerMeter;    /* Horizontal (X) resolution in pixels/meter */
    uint32_t biYpelsPerMeter;    /* Vertical (Y) resolution in pixels/meter */
    uint32_t biClrUsed;          /* Number of color used in the image (0: ALL) */
    uint32_t biClrImportant;     /* Number of important color (0: ALL) */
    uint32_t biRedMask;          /* Red channel bit mask */
    uint32_t biGreenMask;        /* Green channel bit mask */
    uint32_t biBlueMask;         /* Blue channel bit mask */
    uint32_t biAlphaMask;        /* Alpha channel bit mask */
    uint32_t biColorSpaceType;   /* Color space type */
    uint8_t  biColorSpaceEP[36]; /* Color space end points */
    uint32_t biRedGamma;         /* Red channel gamma */
    uint32_t biGreenGamma;       /* Green channel gamma */
    uint32_t biBlueGamma;        /* Blue channel gamma */
    uint32_t biIntent;           /* Intent */
    uint32_t biIccProfileData;   /* ICC profile data */
    uint32_t biIccProfileSize;   /* ICC profile size */
    uint32_t biReserved;         /* Reserved */
} OPJ_BITMAPINFOHEADER;

static void opj_applyLUT8u_8u32s_C1R(
    uint8_t const* pSrc, int32_t srcStride,
    int32_t* pDst, int32_t dstStride,
    uint8_t const* pLUT,
    uint32_t width, uint32_t height)
{
    uint32_t y;

    for (y = height; y != 0U; --y) {
        uint32_t x;

        for(x = 0; x < width; x++) {
            pDst[x] = (int32_t)pLUT[pSrc[x]];
        }
        pSrc += srcStride;
        pDst += dstStride;
    }
}

static void opj_applyLUT8u_8u32s_C1P3R(
    uint8_t const* pSrc, int32_t srcStride,
    int32_t* const* pDst, int32_t const* pDstStride,
    uint8_t const* const* pLUT,
    uint32_t width, uint32_t height)
{
    uint32_t y;
    int32_t* pR = pDst[0];
    int32_t* pG = pDst[1];
    int32_t* pB = pDst[2];
    uint8_t const* pLUT_R = pLUT[0];
    uint8_t const* pLUT_G = pLUT[1];
    uint8_t const* pLUT_B = pLUT[2];

    for (y = height; y != 0U; --y) {
        uint32_t x;

        for(x = 0; x < width; x++) {
            uint8_t idx = pSrc[x];
            pR[x] = (int32_t)pLUT_R[idx];
            pG[x] = (int32_t)pLUT_G[idx];
            pB[x] = (int32_t)pLUT_B[idx];
        }
        pSrc += srcStride;
        pR += pDstStride[0];
        pG += pDstStride[1];
        pB += pDstStride[2];
    }
}

static void bmp24toimage(const uint8_t* pData, uint32_t stride, opj_image_t* image)
{
    int index;
    uint32_t width, height;
    uint32_t x, y;
    const uint8_t *pSrc = NULL;

    width  = image->comps[0].w;
    height = image->comps[0].h;

    index = 0;
    pSrc = pData + (height - 1U) * stride;
    for(y = 0; y < height; y++) {
        for(x = 0; x < width; x++) {
            image->comps[0].data[index] = (int32_t)pSrc[3*x+2];	/* R */
            image->comps[1].data[index] = (int32_t)pSrc[3*x+1];	/* G */
            image->comps[2].data[index] = (int32_t)pSrc[3*x+0];	/* B */
            index++;
        }
        pSrc -= stride;
    }
}

static void bmp_mask_get_shift_and_prec(uint32_t mask, uint32_t* shift, uint32_t* prec)
{
    uint32_t l_shift, l_prec;

    l_shift = l_prec = 0U;

    if (mask != 0U) {
        while ((mask & 1U) == 0U) {
            mask >>= 1;
            l_shift++;
        }
        while (mask & 1U) {
            mask >>= 1;
            l_prec++;
        }
    }
    *shift = l_shift;
    *prec = l_prec;
}

static void bmpmask32toimage(const uint8_t* pData, uint32_t stride, opj_image_t* image, uint32_t redMask, uint32_t greenMask, uint32_t blueMask, uint32_t alphaMask)
{
    int index;
    uint32_t width, height;
    uint32_t x, y;
    const uint8_t *pSrc = NULL;
    bool hasAlpha = false;
    uint32_t redShift,   redPrec;
    uint32_t greenShift, greenPrec;
    uint32_t blueShift,  bluePrec;
    uint32_t alphaShift, alphaPrec;

    width  = image->comps[0].w;
    height = image->comps[0].h;

    hasAlpha = image->numcomps > 3U;

    bmp_mask_get_shift_and_prec(redMask,   &redShift,   &redPrec);
    bmp_mask_get_shift_and_prec(greenMask, &greenShift, &greenPrec);
    bmp_mask_get_shift_and_prec(blueMask,  &blueShift,  &bluePrec);
    bmp_mask_get_shift_and_prec(alphaMask, &alphaShift, &alphaPrec);

    image->comps[0].prec = redPrec;
    image->comps[1].prec = greenPrec;
    image->comps[2].prec = bluePrec;
    if (hasAlpha) {
        image->comps[3].prec = alphaPrec;
    }

    index = 0;
    pSrc = pData + (height - 1U) * stride;
    for(y = 0; y < height; y++) {
        for(x = 0; x < width; x++) {
            uint32_t value = 0U;

            value |= ((uint32_t)pSrc[4*x+0]) <<  0;
            value |= ((uint32_t)pSrc[4*x+1]) <<  8;
            value |= ((uint32_t)pSrc[4*x+2]) << 16;
            value |= ((uint32_t)pSrc[4*x+3]) << 24;

            image->comps[0].data[index] = (int32_t)((value & redMask)   >> redShift);   /* R */
            image->comps[1].data[index] = (int32_t)((value & greenMask) >> greenShift); /* G */
            image->comps[2].data[index] = (int32_t)((value & blueMask)  >> blueShift);  /* B */
            if (hasAlpha) {
                image->comps[3].data[index] = (int32_t)((value & alphaMask)  >> alphaShift);  /* A */
            }
            index++;
        }
        pSrc -= stride;
    }
}

static void bmpmask16toimage(const uint8_t* pData, uint32_t stride, opj_image_t* image, uint32_t redMask, uint32_t greenMask, uint32_t blueMask, uint32_t alphaMask)
{
    int index;
    uint32_t width, height;
    uint32_t x, y;
    const uint8_t *pSrc = NULL;
    bool hasAlpha = false;
    uint32_t redShift,   redPrec;
    uint32_t greenShift, greenPrec;
    uint32_t blueShift,  bluePrec;
    uint32_t alphaShift, alphaPrec;

    width  = image->comps[0].w;
    height = image->comps[0].h;

    hasAlpha = image->numcomps > 3U;

    bmp_mask_get_shift_and_prec(redMask,   &redShift,   &redPrec);
    bmp_mask_get_shift_and_prec(greenMask, &greenShift, &greenPrec);
    bmp_mask_get_shift_and_prec(blueMask,  &blueShift,  &bluePrec);
    bmp_mask_get_shift_and_prec(alphaMask, &alphaShift, &alphaPrec);

    image->comps[0].prec = redPrec;
    image->comps[1].prec = greenPrec;
    image->comps[2].prec = bluePrec;
    if (hasAlpha) {
        image->comps[3].prec = alphaPrec;
    }

    index = 0;
    pSrc = pData + (height - 1U) * stride;
    for(y = 0; y < height; y++) {
        for(x = 0; x < width; x++) {
            uint32_t value = 0U;

            value |= ((uint32_t)pSrc[2*x+0]) <<  0;
            value |= ((uint32_t)pSrc[2*x+1]) <<  8;

            image->comps[0].data[index] = (int32_t)((value & redMask)   >> redShift);   /* R */
            image->comps[1].data[index] = (int32_t)((value & greenMask) >> greenShift); /* G */
            image->comps[2].data[index] = (int32_t)((value & blueMask)  >> blueShift);  /* B */
            if (hasAlpha) {
                image->comps[3].data[index] = (int32_t)((value & alphaMask)  >> alphaShift);  /* A */
            }
            index++;
        }
        pSrc -= stride;
    }
}

static opj_image_t* bmp8toimage(const uint8_t* pData, uint32_t stride, opj_image_t* image, uint8_t const* const* pLUT)
{
    uint32_t width, height;
    const uint8_t *pSrc = NULL;

    width  = image->comps[0].w;
    height = image->comps[0].h;

    pSrc = pData + (height - 1U) * stride;
    if (image->numcomps == 1U) {
        opj_applyLUT8u_8u32s_C1R(pSrc, -(int32_t)stride, image->comps[0].data, (int32_t)width, pLUT[0], width, height);
    } else {
        int32_t* pDst[3];
        int32_t  pDstStride[3];

        pDst[0] = image->comps[0].data;
        pDst[1] = image->comps[1].data;
        pDst[2] = image->comps[2].data;
        pDstStride[0] = (int32_t)width;
        pDstStride[1] = (int32_t)width;
        pDstStride[2] = (int32_t)width;
        opj_applyLUT8u_8u32s_C1P3R(pSrc, -(int32_t)stride, pDst, pDstStride, pLUT, width, height);
    }
    return image;
}

static bool bmp_read_file_header(FILE* IN, OPJ_BITMAPFILEHEADER* header)
{
    header->bfType  = (uint16_t)getc(IN);
    header->bfType |= (uint16_t)((uint32_t)getc(IN) << 8);

    if (header->bfType != 19778) {
        fprintf(stderr,"Error, not a BMP file!\n");
        return false;
    }

    /* FILE HEADER */
    /* ------------- */
    header->bfSize  = (uint32_t)getc(IN);
    header->bfSize |= (uint32_t)getc(IN) << 8;
    header->bfSize |= (uint32_t)getc(IN) << 16;
    header->bfSize |= (uint32_t)getc(IN) << 24;

    header->bfReserved1  = (uint16_t)getc(IN);
    header->bfReserved1 |= (uint16_t)((uint32_t)getc(IN) << 8);

    header->bfReserved2  = (uint16_t)getc(IN);
    header->bfReserved2 |= (uint16_t)((uint32_t)getc(IN) << 8);

    header->bfOffBits  = (uint32_t)getc(IN);
    header->bfOffBits |= (uint32_t)getc(IN) << 8;
    header->bfOffBits |= (uint32_t)getc(IN) << 16;
    header->bfOffBits |= (uint32_t)getc(IN) << 24;
    return true;
}
static bool bmp_read_info_header(FILE* IN, OPJ_BITMAPINFOHEADER* header)
{
    memset(header, 0, sizeof(*header));
    /* INFO HEADER */
    /* ------------- */
    header->biSize  = (uint32_t)getc(IN);
    header->biSize |= (uint32_t)getc(IN) << 8;
    header->biSize |= (uint32_t)getc(IN) << 16;
    header->biSize |= (uint32_t)getc(IN) << 24;

    switch (header->biSize) {
    case 12U:  /* BITMAPCOREHEADER */
    case 40U:  /* BITMAPINFOHEADER */
    case 52U:  /* BITMAPV2INFOHEADER */
    case 56U:  /* BITMAPV3INFOHEADER */
    case 108U: /* BITMAPV4HEADER */
    case 124U: /* BITMAPV5HEADER */
        break;
    default:
        fprintf(stderr,"Error, unknown BMP header size %d\n", header->biSize);
        return false;
    }

    header->biWidth  = (uint32_t)getc(IN);
    header->biWidth |= (uint32_t)getc(IN) << 8;
    header->biWidth |= (uint32_t)getc(IN) << 16;
    header->biWidth |= (uint32_t)getc(IN) << 24;

    header->biHeight  = (uint32_t)getc(IN);
    header->biHeight |= (uint32_t)getc(IN) << 8;
    header->biHeight |= (uint32_t)getc(IN) << 16;
    header->biHeight |= (uint32_t)getc(IN) << 24;

    header->biPlanes  = (uint16_t)getc(IN);
    header->biPlanes |= (uint16_t)((uint32_t)getc(IN) << 8);

    header->biBitCount  = (uint16_t)getc(IN);
    header->biBitCount |= (uint16_t)((uint32_t)getc(IN) << 8);

    if(header->biSize >= 40U) {
        header->biCompression  = (uint32_t)getc(IN);
        header->biCompression |= (uint32_t)getc(IN) << 8;
        header->biCompression |= (uint32_t)getc(IN) << 16;
        header->biCompression |= (uint32_t)getc(IN) << 24;

        header->biSizeImage  = (uint32_t)getc(IN);
        header->biSizeImage |= (uint32_t)getc(IN) << 8;
        header->biSizeImage |= (uint32_t)getc(IN) << 16;
        header->biSizeImage |= (uint32_t)getc(IN) << 24;

        header->biXpelsPerMeter  = (uint32_t)getc(IN);
        header->biXpelsPerMeter |= (uint32_t)getc(IN) << 8;
        header->biXpelsPerMeter |= (uint32_t)getc(IN) << 16;
        header->biXpelsPerMeter |= (uint32_t)getc(IN) << 24;

        header->biYpelsPerMeter  = (uint32_t)getc(IN);
        header->biYpelsPerMeter |= (uint32_t)getc(IN) << 8;
        header->biYpelsPerMeter |= (uint32_t)getc(IN) << 16;
        header->biYpelsPerMeter |= (uint32_t)getc(IN) << 24;

        header->biClrUsed  = (uint32_t)getc(IN);
        header->biClrUsed |= (uint32_t)getc(IN) << 8;
        header->biClrUsed |= (uint32_t)getc(IN) << 16;
        header->biClrUsed |= (uint32_t)getc(IN) << 24;

        header->biClrImportant  = (uint32_t)getc(IN);
        header->biClrImportant |= (uint32_t)getc(IN) << 8;
        header->biClrImportant |= (uint32_t)getc(IN) << 16;
        header->biClrImportant |= (uint32_t)getc(IN) << 24;
    }

    if(header->biSize >= 56U) {
        header->biRedMask  = (uint32_t)getc(IN);
        header->biRedMask |= (uint32_t)getc(IN) << 8;
        header->biRedMask |= (uint32_t)getc(IN) << 16;
        header->biRedMask |= (uint32_t)getc(IN) << 24;

        header->biGreenMask  = (uint32_t)getc(IN);
        header->biGreenMask |= (uint32_t)getc(IN) << 8;
        header->biGreenMask |= (uint32_t)getc(IN) << 16;
        header->biGreenMask |= (uint32_t)getc(IN) << 24;

        header->biBlueMask  = (uint32_t)getc(IN);
        header->biBlueMask |= (uint32_t)getc(IN) << 8;
        header->biBlueMask |= (uint32_t)getc(IN) << 16;
        header->biBlueMask |= (uint32_t)getc(IN) << 24;

        header->biAlphaMask  = (uint32_t)getc(IN);
        header->biAlphaMask |= (uint32_t)getc(IN) << 8;
        header->biAlphaMask |= (uint32_t)getc(IN) << 16;
        header->biAlphaMask |= (uint32_t)getc(IN) << 24;
    }

    if(header->biSize >= 108U) {
        header->biColorSpaceType  = (uint32_t)getc(IN);
        header->biColorSpaceType |= (uint32_t)getc(IN) << 8;
        header->biColorSpaceType |= (uint32_t)getc(IN) << 16;
        header->biColorSpaceType |= (uint32_t)getc(IN) << 24;

        if (fread(&(header->biColorSpaceEP), 1U, sizeof(header->biColorSpaceEP), IN) != sizeof(header->biColorSpaceEP)) {
            fprintf(stderr,"Error, can't  read BMP header\n");
            return false;
        }

        header->biRedGamma  = (uint32_t)getc(IN);
        header->biRedGamma |= (uint32_t)getc(IN) << 8;
        header->biRedGamma |= (uint32_t)getc(IN) << 16;
        header->biRedGamma |= (uint32_t)getc(IN) << 24;

        header->biGreenGamma  = (uint32_t)getc(IN);
        header->biGreenGamma |= (uint32_t)getc(IN) << 8;
        header->biGreenGamma |= (uint32_t)getc(IN) << 16;
        header->biGreenGamma |= (uint32_t)getc(IN) << 24;

        header->biBlueGamma  = (uint32_t)getc(IN);
        header->biBlueGamma |= (uint32_t)getc(IN) << 8;
        header->biBlueGamma |= (uint32_t)getc(IN) << 16;
        header->biBlueGamma |= (uint32_t)getc(IN) << 24;
    }

    if(header->biSize >= 124U) {
        header->biIntent  = (uint32_t)getc(IN);
        header->biIntent |= (uint32_t)getc(IN) << 8;
        header->biIntent |= (uint32_t)getc(IN) << 16;
        header->biIntent |= (uint32_t)getc(IN) << 24;

        header->biIccProfileData  = (uint32_t)getc(IN);
        header->biIccProfileData |= (uint32_t)getc(IN) << 8;
        header->biIccProfileData |= (uint32_t)getc(IN) << 16;
        header->biIccProfileData |= (uint32_t)getc(IN) << 24;

        header->biIccProfileSize  = (uint32_t)getc(IN);
        header->biIccProfileSize |= (uint32_t)getc(IN) << 8;
        header->biIccProfileSize |= (uint32_t)getc(IN) << 16;
        header->biIccProfileSize |= (uint32_t)getc(IN) << 24;

        header->biReserved  = (uint32_t)getc(IN);
        header->biReserved |= (uint32_t)getc(IN) << 8;
        header->biReserved |= (uint32_t)getc(IN) << 16;
        header->biReserved |= (uint32_t)getc(IN) << 24;
    }
    return true;
}

static bool bmp_read_raw_data(FILE* IN, uint8_t* pData, uint32_t stride, uint32_t width, uint32_t height)
{
    OPJ_ARG_NOT_USED(width);

    if ( fread(pData, sizeof(uint8_t), stride * height, IN) != (stride * height) ) {
        fprintf(stderr, "\nError: fread return a number of element different from the expected.\n");
        return false;
    }
    return true;
}

static bool bmp_read_rle8_data(FILE* IN, uint8_t* pData, uint32_t stride, uint32_t width, uint32_t height)
{
    uint32_t x, y;
    uint8_t *pix;
    const uint8_t *beyond;

    beyond = pData + stride * height;
    pix = pData;

    x = y = 0U;
    while (y < height) {
        int c = getc(IN);

        if (c) {
            int j;
            uint8_t c1 = (uint8_t)getc(IN);

            for (j = 0; (j < c) && (x < width) && ((size_t)pix < (size_t)beyond); j++, x++, pix++) {
                *pix = c1;
            }
        } else {
            c = getc(IN);
            if (c == 0x00) { /* EOL */
                x = 0;
                ++y;
                pix = pData + y * stride + x;
            } else if (c == 0x01) { /* EOP */
                break;
            } else if (c == 0x02) { /* MOVE by dxdy */
                c = getc(IN);
                x += (uint32_t)c;
                c = getc(IN);
                y += (uint32_t)c;
                pix = pData + y * stride + x;
            } else { /* 03 .. 255 */
                int j;
                for (j = 0; (j < c) && (x < width) && ((size_t)pix < (size_t)beyond); j++, x++, pix++) {
                    uint8_t c1 = (uint8_t)getc(IN);
                    *pix = c1;
                }
                if ((uint32_t)c & 1U) { /* skip padding byte */
					if (getc(IN) == EOF)
					return false;
                }
            }
        }
    }/* while() */
    return true;
}

static bool bmp_read_rle4_data(FILE* IN, uint8_t* pData, uint32_t stride, uint32_t width, uint32_t height)
{
    uint32_t x, y;
    uint8_t *pix;
    const uint8_t *beyond;

    beyond = pData + stride * height;
    pix = pData;
    x = y = 0U;
    while(y < height) {
        int c = getc(IN);
        if(c == EOF) break;

        if(c) {/* encoded mode */
            int j;
            uint8_t c1 = (uint8_t)getc(IN);

            for (j = 0; (j < c) && (x < width) && ((size_t)pix < (size_t)beyond); j++, x++, pix++) {
                *pix = (uint8_t)((j&1) ? (c1 & 0x0fU) : ((c1>>4)&0x0fU));
            }
        } else { /* absolute mode */
            c = getc(IN);
            if(c == EOF) break;

            if(c == 0x00) { /* EOL */
                x = 0;
                y++;
                pix = pData + y * stride;
            } else if(c == 0x01) { /* EOP */
                break;
            } else if(c == 0x02) { /* MOVE by dxdy */
                c = getc(IN);
                x += (uint32_t)c;
                c = getc(IN);
                y += (uint32_t)c;
                pix = pData + y * stride + x;
            } else { /* 03 .. 255 : absolute mode */
                int j;
                uint8_t c1 = 0U;

                for (j = 0; (j < c) && (x < width) && ((size_t)pix < (size_t)beyond); j++, x++, pix++) {
                    if((j&1) == 0) {
                        c1 = (uint8_t)getc(IN);
                    }
                    *pix =  (uint8_t)((j&1) ? (c1 & 0x0fU) : ((c1>>4)&0x0fU));
                }
                if(((c&3) == 1) || ((c&3) == 2)) { /* skip padding byte */
					if (getc(IN) == EOF)
						return false;
                }
            }
        }
    }  /* while(y < height) */
    return true;
}

opj_image_t* bmptoimage(const char *filename, opj_cparameters_t *parameters)
{
    opj_image_cmptparm_t cmptparm[4];	/* maximum of 4 components */
    uint8_t lut_R[256], lut_G[256], lut_B[256];
    uint8_t const* pLUT[3];
    opj_image_t * image = NULL;
    FILE *IN;
    OPJ_BITMAPFILEHEADER File_h;
    OPJ_BITMAPINFOHEADER Info_h;
    uint32_t i, palette_len, numcmpts = 1U;
    bool l_result = false;
    uint8_t* pData = NULL;
    uint32_t stride;

    pLUT[0] = lut_R;
    pLUT[1] = lut_G;
    pLUT[2] = lut_B;

    IN = fopen(filename, "rb");
    if (!IN) {
        fprintf(stderr, "Failed to open %s for reading !!\n", filename);
        return NULL;
    }

    if (!bmp_read_file_header(IN, &File_h)) {
        fclose(IN);
        return NULL;
    }
    if (!bmp_read_info_header(IN, &Info_h)) {
        fclose(IN);
        return NULL;
    }

    /* Load palette */
    if (Info_h.biBitCount <= 8U) {
        memset(&lut_R[0], 0, sizeof(lut_R));
        memset(&lut_G[0], 0, sizeof(lut_G));
        memset(&lut_B[0], 0, sizeof(lut_B));

        palette_len = Info_h.biClrUsed;
        if((palette_len == 0U) && (Info_h.biBitCount <= 8U)) {
            palette_len = (1U << Info_h.biBitCount);
        }
        if (palette_len > 256U) {
            palette_len = 256U;
        }
        if (palette_len > 0U) {
            uint8_t has_color = 0U;
            for (i = 0U; i < palette_len; i++) {
                lut_B[i] = (uint8_t)getc(IN);
                lut_G[i] = (uint8_t)getc(IN);
                lut_R[i] = (uint8_t)getc(IN);
                (void)getc(IN); /* padding */
                has_color |= (lut_B[i] ^ lut_G[i]) | (lut_G[i] ^ lut_R[i]);
            }
            if(has_color) {
                numcmpts = 3U;
            }
        }
    } else {
        numcmpts = 3U;
        if ((Info_h.biCompression == 3) && (Info_h.biAlphaMask != 0U)) {
            numcmpts++;
        }
    }

	if (Info_h.biWidth == 0 || Info_h.biHeight == 0) {
		fclose(IN);
		return NULL;
	}
	
	if (Info_h.biBitCount > (((OPJ_UINT32)-1) - 31) / Info_h.biWidth) {
		fclose(IN);
		return NULL;
	}
    stride = ((Info_h.biWidth * Info_h.biBitCount + 31U) / 32U) * 4U; /* rows are aligned on 32bits */
    if (Info_h.biBitCount == 4 && Info_h.biCompression == 2) { /* RLE 4 gets decoded as 8 bits data for now... */
		if (8 > (((OPJ_UINT32)-1) - 31) / Info_h.biWidth) {
			fclose(IN);
			return NULL;
		}
        stride = ((Info_h.biWidth * 8U + 31U) / 32U) * 4U;
	}
	
	if (stride > ((OPJ_UINT32)-1) / sizeof(OPJ_UINT8) / Info_h.biHeight) {
		fclose(IN);
		return NULL;
    }
    pData = (uint8_t *) calloc(1, stride * Info_h.biHeight * sizeof(uint8_t));
    if (pData == NULL) {
        fclose(IN);
        return NULL;
    }
    /* Place the cursor at the beginning of the image information */
    fseek(IN, 0, SEEK_SET);
    fseek(IN, (long)File_h.bfOffBits, SEEK_SET);

    switch (Info_h.biCompression) {
    case 0:
    case 3:
        /* read raw data */
        l_result = bmp_read_raw_data(IN, pData, stride, Info_h.biWidth, Info_h.biHeight);
        break;
    case 1:
        /* read rle8 data */
        l_result = bmp_read_rle8_data(IN, pData, stride, Info_h.biWidth, Info_h.biHeight);
        break;
    case 2:
        /* read rle4 data */
        l_result = bmp_read_rle4_data(IN, pData, stride, Info_h.biWidth, Info_h.biHeight);
        break;
    default:
        fprintf(stderr, "Unsupported BMP compression\n");
        l_result = false;
        break;
    }
    if (!l_result) {
        free(pData);
        fclose(IN);
        return NULL;
    }

    /* create the image */
    memset(&cmptparm[0], 0, sizeof(cmptparm));
    for(i = 0; i < 4U; i++) {
        cmptparm[i].prec = 8;
        cmptparm[i].sgnd = 0;
        cmptparm[i].dx   = parameters->subsampling_dx;
        cmptparm[i].dy   = parameters->subsampling_dy;
        cmptparm[i].w    = Info_h.biWidth;
        cmptparm[i].h    = Info_h.biHeight;
    }

    image = opj_image_create(numcmpts, &cmptparm[0], (numcmpts == 1U) ? OPJ_CLRSPC_GRAY : OPJ_CLRSPC_SRGB);
    if(!image) {
        fclose(IN);
        free(pData);
        return NULL;
    }
    if (numcmpts == 4U) {
        image->comps[3].alpha = 1;
    }

    /* set image offset and reference grid */
    image->x0 = parameters->image_offset_x0;
    image->y0 = parameters->image_offset_y0;
    image->x1 =	image->x0 + (Info_h.biWidth  - 1U) * parameters->subsampling_dx + 1U;
    image->y1 = image->y0 + (Info_h.biHeight - 1U) * parameters->subsampling_dy + 1U;

    /* Read the data */
    if (Info_h.biBitCount == 24 && Info_h.biCompression == 0) { /*RGB */
        bmp24toimage(pData, stride, image);
    } else if (Info_h.biBitCount == 8 && Info_h.biCompression == 0) { /* RGB 8bpp Indexed */
        bmp8toimage(pData, stride, image, pLUT);
    } else if (Info_h.biBitCount == 8 && Info_h.biCompression == 1) { /*RLE8*/
        bmp8toimage(pData, stride, image, pLUT);
    } else if (Info_h.biBitCount == 4 && Info_h.biCompression == 2) { /*RLE4*/
        bmp8toimage(pData, stride, image, pLUT); /* RLE 4 gets decoded as 8 bits data for now */
    } else if (Info_h.biBitCount == 32 && Info_h.biCompression == 0) { /* RGBX */
        bmpmask32toimage(pData, stride, image, 0x00FF0000U, 0x0000FF00U, 0x000000FFU, 0x00000000U);
    } else if (Info_h.biBitCount == 32 && Info_h.biCompression == 3) { /* bitmask */
        bmpmask32toimage(pData, stride, image, Info_h.biRedMask, Info_h.biGreenMask, Info_h.biBlueMask, Info_h.biAlphaMask);
    } else if (Info_h.biBitCount == 16 && Info_h.biCompression == 0) { /* RGBX */
        bmpmask16toimage(pData, stride, image, 0x7C00U, 0x03E0U, 0x001FU, 0x0000U);
    } else if (Info_h.biBitCount == 16 && Info_h.biCompression == 3) { /* bitmask */
        if ((Info_h.biRedMask == 0U) && (Info_h.biGreenMask == 0U) && (Info_h.biBlueMask == 0U)) {
            Info_h.biRedMask   = 0xF800U;
            Info_h.biGreenMask = 0x07E0U;
            Info_h.biBlueMask  = 0x001FU;
        }
        bmpmask16toimage(pData, stride, image, Info_h.biRedMask, Info_h.biGreenMask, Info_h.biBlueMask, Info_h.biAlphaMask);
    } else {
        opj_image_destroy(image);
        image = NULL;
        fprintf(stderr, "Other system than 24 bits/pixels or 8 bits (no RLE coding) is not yet implemented [%d]\n", Info_h.biBitCount);
    }
    free(pData);
    fclose(IN);
    return image;
}

int imagetobmp(opj_image_t * image, const char *outfile)
{
    int w, h;
    int i, pad;
    FILE *fdest = NULL;
    int adjustR, adjustG, adjustB;

    if (image->comps[0].prec < 8) {
        fprintf(stderr, "Unsupported precision: %d\n", image->comps[0].prec);
        return 1;
    }
    if (image->numcomps >= 3 && image->comps[0].dx == image->comps[1].dx
            && image->comps[1].dx == image->comps[2].dx
            && image->comps[0].dy == image->comps[1].dy
            && image->comps[1].dy == image->comps[2].dy
            && image->comps[0].prec == image->comps[1].prec
            && image->comps[1].prec == image->comps[2].prec
			&& image->comps[0].sgnd == image->comps[1].sgnd
			&& image->comps[1].sgnd == image->comps[2].sgnd) {


        /* -->> -->> -->> -->>
        24 bits color
        <<-- <<-- <<-- <<-- */

        fdest = fopen(outfile, "wb");
        if (!fdest) {
            fprintf(stderr, "ERROR -> failed to open %s for writing\n", outfile);
            return 1;
        }

        w = (int)image->comps[0].w;
        h = (int)image->comps[0].h;

        fprintf(fdest, "BM");

        /* FILE HEADER */
        /* ------------- */
        fprintf(fdest, "%c%c%c%c",
                (uint8_t) (h * w * 3 + 3 * h * (w % 2) + 54) & 0xff,
                (uint8_t) ((h * w * 3 + 3 * h * (w % 2) + 54)	>> 8) & 0xff,
                (uint8_t) ((h * w * 3 + 3 * h * (w % 2) + 54)	>> 16) & 0xff,
                (uint8_t) ((h * w * 3 + 3 * h * (w % 2) + 54)	>> 24) & 0xff);
        fprintf(fdest, "%c%c%c%c", (0) & 0xff, ((0) >> 8) & 0xff, ((0) >> 16) & 0xff, ((0) >> 24) & 0xff);
        fprintf(fdest, "%c%c%c%c", (54) & 0xff, ((54) >> 8) & 0xff,((54) >> 16) & 0xff, ((54) >> 24) & 0xff);

        /* INFO HEADER   */
        /* ------------- */
        fprintf(fdest, "%c%c%c%c", (40) & 0xff, ((40) >> 8) & 0xff,	((40) >> 16) & 0xff, ((40) >> 24) & 0xff);
        fprintf(fdest, "%c%c%c%c", (uint8_t) ((w) & 0xff),
                (uint8_t) ((w) >> 8) & 0xff,
                (uint8_t) ((w) >> 16) & 0xff,
                (uint8_t) ((w) >> 24) & 0xff);
        fprintf(fdest, "%c%c%c%c", (uint8_t) ((h) & 0xff),
                (uint8_t) ((h) >> 8) & 0xff,
                (uint8_t) ((h) >> 16) & 0xff,
                (uint8_t) ((h) >> 24) & 0xff);
        fprintf(fdest, "%c%c", (1) & 0xff, ((1) >> 8) & 0xff);
        fprintf(fdest, "%c%c", (24) & 0xff, ((24) >> 8) & 0xff);
        fprintf(fdest, "%c%c%c%c", (0) & 0xff, ((0) >> 8) & 0xff, ((0) >> 16) & 0xff, ((0) >> 24) & 0xff);
        fprintf(fdest, "%c%c%c%c", (uint8_t) (3 * h * w + 3 * h * (w % 2)) & 0xff,
                (uint8_t) ((h * w * 3 + 3 * h * (w % 2)) >> 8) & 0xff,
                (uint8_t) ((h * w * 3 + 3 * h * (w % 2)) >> 16) & 0xff,
                (uint8_t) ((h * w * 3 + 3 * h * (w % 2)) >> 24) & 0xff);
        fprintf(fdest, "%c%c%c%c", (7834) & 0xff, ((7834) >> 8) & 0xff, ((7834) >> 16) & 0xff, ((7834) >> 24) & 0xff);
        fprintf(fdest, "%c%c%c%c", (7834) & 0xff, ((7834) >> 8) & 0xff,	((7834) >> 16) & 0xff, ((7834) >> 24) & 0xff);
        fprintf(fdest, "%c%c%c%c", (0) & 0xff, ((0) >> 8) & 0xff, ((0) >> 16) & 0xff, ((0) >> 24) & 0xff);
        fprintf(fdest, "%c%c%c%c", (0) & 0xff, ((0) >> 8) & 0xff, ((0) >> 16) & 0xff, ((0) >> 24) & 0xff);

        if (image->comps[0].prec > 8) {
            adjustR = (int)image->comps[0].prec - 8;
            printf("BMP CONVERSION: Truncating component 0 from %d bits to 8 bits\n", image->comps[0].prec);
        } else
            adjustR = 0;
        if (image->comps[1].prec > 8) {
            adjustG = (int)image->comps[1].prec - 8;
            printf("BMP CONVERSION: Truncating component 1 from %d bits to 8 bits\n", image->comps[1].prec);
        } else
            adjustG = 0;
        if (image->comps[2].prec > 8) {
            adjustB = (int)image->comps[2].prec - 8;
            printf("BMP CONVERSION: Truncating component 2 from %d bits to 8 bits\n", image->comps[2].prec);
        } else
            adjustB = 0;

        for (i = 0; i < w * h; i++) {
            uint8_t rc, gc, bc;
            int r, g, b;

            r = image->comps[0].data[w * h - ((i) / (w) + 1) * w + (i) % (w)];
            r += (image->comps[0].sgnd ? 1 << (image->comps[0].prec - 1) : 0);
            r = ((r >> adjustR)+((r >> (adjustR-1))%2));
            if(r > 255) r = 255;
            else if(r < 0) r = 0;
            rc = (uint8_t)r;

            g = image->comps[1].data[w * h - ((i) / (w) + 1) * w + (i) % (w)];
            g += (image->comps[1].sgnd ? 1 << (image->comps[1].prec - 1) : 0);
            g = ((g >> adjustG)+((g >> (adjustG-1))%2));
            if(g > 255) g = 255;
            else if(g < 0) g = 0;
            gc = (uint8_t)g;

            b = image->comps[2].data[w * h - ((i) / (w) + 1) * w + (i) % (w)];
            b += (image->comps[2].sgnd ? 1 << (image->comps[2].prec - 1) : 0);
            b = ((b >> adjustB)+((b >> (adjustB-1))%2));
            if(b > 255) b = 255;
            else if(b < 0) b = 0;
            bc = (uint8_t)b;

            fprintf(fdest, "%c%c%c", bc, gc, rc);

            if ((i + 1) % w == 0) {
                for (pad = (3 * w) % 4 ? 4 - (3 * w) % 4 : 0; pad > 0; pad--)	/* ADD */
                    fprintf(fdest, "%c", 0);
            }
        }
        fclose(fdest);
    } else {			/* Gray-scale */

        /* -->> -->> -->> -->>
        8 bits non code (Gray scale)
        <<-- <<-- <<-- <<-- */

        fdest = fopen(outfile, "wb");
        if (!fdest) {
            fprintf(stderr, "ERROR -> failed to open %s for writing\n", outfile);
            return 1;
        }
        w = (int)image->comps[0].w;
        h = (int)image->comps[0].h;

        fprintf(fdest, "BM");

        /* FILE HEADER */
        /* ------------- */
        fprintf(fdest, "%c%c%c%c", (uint8_t) (h * w + 54 + 1024 + h * (w % 2)) & 0xff,
                (uint8_t) ((h * w + 54 + 1024 + h * (w % 2)) >> 8) & 0xff,
                (uint8_t) ((h * w + 54 + 1024 + h * (w % 2)) >> 16) & 0xff,
                (uint8_t) ((h * w + 54 + 1024 + w * (w % 2)) >> 24) & 0xff);
        fprintf(fdest, "%c%c%c%c", (0) & 0xff, ((0) >> 8) & 0xff, ((0) >> 16) & 0xff, ((0) >> 24) & 0xff);
        fprintf(fdest, "%c%c%c%c", (54 + 1024) & 0xff, ((54 + 1024) >> 8) & 0xff,
                ((54 + 1024) >> 16) & 0xff,
                ((54 + 1024) >> 24) & 0xff);

        /* INFO HEADER */
        /* ------------- */
        fprintf(fdest, "%c%c%c%c", (40) & 0xff, ((40) >> 8) & 0xff,	((40) >> 16) & 0xff, ((40) >> 24) & 0xff);
        fprintf(fdest, "%c%c%c%c", (uint8_t) ((w) & 0xff),
                (uint8_t) ((w) >> 8) & 0xff,
                (uint8_t) ((w) >> 16) & 0xff,
                (uint8_t) ((w) >> 24) & 0xff);
        fprintf(fdest, "%c%c%c%c", (uint8_t) ((h) & 0xff),
                (uint8_t) ((h) >> 8) & 0xff,
                (uint8_t) ((h) >> 16) & 0xff,
                (uint8_t) ((h) >> 24) & 0xff);
        fprintf(fdest, "%c%c", (1) & 0xff, ((1) >> 8) & 0xff);
        fprintf(fdest, "%c%c", (8) & 0xff, ((8) >> 8) & 0xff);
        fprintf(fdest, "%c%c%c%c", (0) & 0xff, ((0) >> 8) & 0xff, ((0) >> 16) & 0xff, ((0) >> 24) & 0xff);
        fprintf(fdest, "%c%c%c%c", (uint8_t) (h * w + h * (w % 2)) & 0xff,
                (uint8_t) ((h * w + h * (w % 2)) >> 8) &	0xff,
                (uint8_t) ((h * w + h * (w % 2)) >> 16) &	0xff,
                (uint8_t) ((h * w + h * (w % 2)) >> 24) & 0xff);
        fprintf(fdest, "%c%c%c%c", (7834) & 0xff, ((7834) >> 8) & 0xff,	((7834) >> 16) & 0xff, ((7834) >> 24) & 0xff);
        fprintf(fdest, "%c%c%c%c", (7834) & 0xff, ((7834) >> 8) & 0xff,	((7834) >> 16) & 0xff, ((7834) >> 24) & 0xff);
        fprintf(fdest, "%c%c%c%c", (256) & 0xff, ((256) >> 8) & 0xff, ((256) >> 16) & 0xff, ((256) >> 24) & 0xff);
        fprintf(fdest, "%c%c%c%c", (256) & 0xff, ((256) >> 8) & 0xff, ((256) >> 16) & 0xff, ((256) >> 24) & 0xff);

        if (image->comps[0].prec > 8) {
            adjustR = (int)image->comps[0].prec - 8;
            printf("BMP CONVERSION: Truncating component 0 from %d bits to 8 bits\n", image->comps[0].prec);
        } else
            adjustR = 0;

        for (i = 0; i < 256; i++) {
            fprintf(fdest, "%c%c%c%c", i, i, i, 0);
        }

        for (i = 0; i < w * h; i++) {
            int r;

            r = image->comps[0].data[w * h - ((i) / (w) + 1) * w + (i) % (w)];
            r += (image->comps[0].sgnd ? 1 << (image->comps[0].prec - 1) : 0);
            r = ((r >> adjustR)+((r >> (adjustR-1))%2));
            if(r > 255) r = 255;
            else if(r < 0) r = 0;

            fprintf(fdest, "%c", (uint8_t)r);

            if ((i + 1) % w == 0) {
                for (pad = w % 4 ? 4 - w % 4 : 0; pad > 0; pad--)	/* ADD */
                    fprintf(fdest, "%c", 0);
            }
        }
        fclose(fdest);
    }

    return 0;
}
