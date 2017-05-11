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

/**
@file opj_intmath.h
@brief Implementation of operations on integers (INT)

The functions in OPJ_INTMATH.H have for goal to realize operations on integers.
*/

/** @defgroup OPJ_INTMATH OPJ_INTMATH - Implementation of operations on integers */
/*@{*/

/** @name Exported functions (see also openjpeg.h) */
/*@{*/
/* ----------------------------------------------------------------------- */


template<typename T> T grk_min(T a, T b) {
	return ((a < b) ? a : b);
}
template<typename T> T grk_max(T a, T b) {
	return ((a > b) ? a : b);
}


/**
 Get the saturated sum of two unsigned integers
 @return Returns saturated sum of a+b
 */
static inline uint32_t grk_uint_adds(uint32_t a, uint32_t b)
{
    uint64_t sum = (uint64_t)a + (uint64_t)b;
    return (uint32_t)(-(int32_t)(sum >> 32)) | (uint32_t)sum;
}

/**
Clamp an integer inside an interval
@return
<ul>
<li>Returns a if (min < a < max)
<li>Returns max if (a > max)
<li>Returns min if (a < min)
</ul>
*/
static inline int32_t grk_int_clamp(int32_t a, int32_t min, int32_t max)
{
    if (a < min)
        return min;
    if (a > max)
        return max;
    return a;
}

/**
Divide an integer and round upwards
@return Returns a divided by b
*/
static inline uint32_t  grk_uint_ceildiv(uint32_t  a, uint32_t  b)
{
    assert(b);
    return (uint32_t)((a + (uint64_t)b - 1) / b);
}

/**
Divide an integer and round upwards
@return Returns a divided by b
*/
static inline uint32_t  grk_uint64_ceildiv(uint64_t  a, uint64_t  b)
{
	assert(b);
	return (uint32_t)((a + b - 1) / b);
}

/**
Divide an integer by a power of 2 and round upwards
@return Returns a divided by 2^b
*/
static inline int32_t grk_int_ceildivpow2(int32_t a, int32_t b)
{
    return (int32_t)((a + ((int64_t)1 << b) - 1) >> b);
}

/**
 Divide a 64bits integer by a power of 2 and round upwards
 @return Returns a divided by 2^b
 */
static inline int32_t grk_int64_ceildivpow2(int64_t a, int32_t b)
{
    return (int32_t)((a + ((int64_t)1 << b) - 1) >> b);
}

/**
Divide a 64bits integer by a power of 2 and round upwards
@return Returns a divided by 2^b
*/
static inline uint32_t grk_uint64_ceildivpow2(uint64_t a, uint32_t b)
{
	return (uint32_t)((a + ((uint64_t)1 << b) - 1) >> b);
}


/**
 Divide an integer by a power of 2 and round upwards
 @return Returns a divided by 2^b
 */
static inline uint32_t grk_uint_ceildivpow2(uint32_t a, uint32_t b)
{
    return (uint32_t)((a + ((uint64_t)1U << b) - 1U) >> b);
}

/**
Divide an integer by a power of 2 and round downwards
@return Returns a divided by 2^b
*/
static inline int32_t grk_int_floordivpow2(int32_t a, int32_t b)
{
    return a >> b;
}

/**
Divide an unsigned integer by a power of 2 and round downwards
@return Returns a divided by 2^b
*/
static inline int32_t grk_uint_floordivpow2(uint32_t a, uint32_t b)
{
	return a >> b;
}

/**
Get logarithm of an integer and round downwards
@return Returns log2(a)
*/
static inline int32_t grk_int_floorlog2(int32_t a)
{
    int32_t l;
    for (l = 0; a > 1; l++) {
        a >>= 1;
    }
    return l;
}
/**
Get logarithm of an integer and round downwards
@return Returns log2(a)
*/
static inline uint32_t  grk_uint_floorlog2(uint32_t  a)
{
    uint32_t  l;
    for (l = 0; a > 1; ++l) {
        a >>= 1;
    }
    return l;
}

/**
Multiply two fixed-precision rational numbers.
@param a
@param b
@return Returns a * b
*/
static inline int32_t grk_int_fix_mul(int32_t a, int32_t b)
{
#if defined(_MSC_VER) && (_MSC_VER >= 1400) && !defined(__INTEL_COMPILER) && defined(_M_IX86)
    int64_t temp = __emul(a, b);
#else
    int64_t temp = (int64_t) a * (int64_t) b ;
#endif
    temp += 4096;
    assert((temp >> 13) <= (int64_t)0x7FFFFFFF);
    assert((temp >> 13) >= (-(int64_t)0x7FFFFFFF - (int64_t)1));
    return (int32_t) (temp >> 13);
}

static inline int32_t grk_int_fix_mul_t1(int32_t a, int32_t b)
{
#if defined(_MSC_VER) && (_MSC_VER >= 1400) && !defined(__INTEL_COMPILER) && defined(_M_IX86)
    int64_t temp = __emul(a, b);
#else
    int64_t temp = (int64_t) a * (int64_t) b ;
#endif
    temp += 4096;
    assert((temp >> (13 + 11 - T1_NMSEDEC_FRACBITS)) <= (int64_t)0x7FFFFFFF);
    assert((temp >> (13 + 11 - T1_NMSEDEC_FRACBITS)) >= (-(int64_t)0x7FFFFFFF - (int64_t)1));
    return (int32_t) (temp >> (13 + 11 - T1_NMSEDEC_FRACBITS)) ;
}

/* ----------------------------------------------------------------------- */
/*@}*/

/*@}*/

