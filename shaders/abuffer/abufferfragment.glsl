/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014 - 2016                                                             *
 *                                                                                       *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this  *
 * software and associated documentation files (the "Software"), to deal in the Software *
 * without restriction, including without limitation the rights to use, copy, modify,    *
 * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    *
 * permit persons to whom the Software is furnished to do so, subject to the following   *
 * conditions:                                                                           *
 *                                                                                       *
 * The above copyright notice and this permission notice shall be included in all copies *
 * or substantial portions of the Software.                                              *
 *                                                                                       *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   *
 * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         *
 * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    *
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  *
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  *
 * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         *
 ****************************************************************************************/

#ifndef _ABUFFERFRAGMENT_GLSL_
#define _ABUFFERFRAGMENT_GLSL_

struct ABufferFragment {
    uint rgba;
    uint depth;
    uint data;
    uint composition;
};

// Values stored in abuffer:
// -------RGBA--------
// r            8 bits
// g            8 bits
// b            8 bits
// a            8 bits
// -------DEPTH-------
// depth       32 bits 
// -------DATA--------
// type         8 bits (signed char) 0: geometry, >0: volume entry, <0: volume exit
// msaa         8 bits 
// reserved    16 bits
// ----COMPOSITION----
// reserved     4 bits (may be suitable for blend modes)
// next        28 bits
// -------------------
// in total: 16 + 4 = 20 reserved bits for future use.

const uint mask_1 = uint(1);
const uint mask_8 = uint(255);
const uint mask_16 = uint(65535);
const uint mask_24 = uint(16777215);
const uint mask_28 = uint(268435455);
const uint mask_31 = uint(2147483647);
const uint mask_32 = uint(4294967295);

const uint mask_type   = mask_32 - mask_24;
const uint shift_type  = 24;

const uint mask_msaa   = mask_24 - mask_16;
const uint shift_msaa  = 16;

const uint mask_blend  = mask_32 - mask_31;
const uint shift_blend = 31;

const uint mask_next   = mask_28;
const uint shift_next  = 0;

void bitinsert(inout uint pack, uint val, uint mask, uint shift) {
	pack &= ~mask;
	pack |= (val << shift) & mask;
}
uint bitextract(in uint pack, uint mask, uint shift) {
	return (pack >> shift) & (mask >> shift);
}

/**
 * Color
 */
void _color_(inout ABufferFragment frag, vec4 color) {
    frag.rgba = packUnorm4x8(color);
}

vec4 _color_(ABufferFragment frag) {
    return unpackUnorm4x8(frag.rgba);
}

/**
 * Depth
 */
void _depth_(inout ABufferFragment frag, float depth) {
    frag.depth = floatBitsToUint(depth);
}

float _depth_(ABufferFragment frag) {
    return uintBitsToFloat(frag.depth);
}

/**
 * Type
 */
void _type_(inout ABufferFragment frag, int type) {
    uint val;
    if (type < 0) {
        val = uint(-type) + 128;
    } else {
        val = type;
    }
    bitinsert(frag.data, val, mask_type, shift_type);
}

int _type_(ABufferFragment frag) {
    uint val = bitextract(frag.data, mask_type, shift_type);
    if (val > 127) {
        return 128 - int(val);
    } else {
        return int(val);
    }
}

/**
 * Msaa
 */
void _msaa_(inout ABufferFragment frag, int type) {
    uint val = uint(type);
    bitinsert(frag.data, val, mask_msaa, shift_msaa);
}

int _msaa_(ABufferFragment frag) {
    uint val = bitextract(frag.data, mask_msaa, shift_msaa);
    return int(val);
}

/**
 * Next
 */
void _next_(inout ABufferFragment frag, uint val) {
    bitinsert(frag.composition, val, mask_next, shift_next);
}

uint _next_(ABufferFragment frag) {
    uint val = bitextract(frag.composition, mask_next, shift_next);
    return val;
}

/**
 * Raw data
 */
void _raw_(inout ABufferFragment frag, uvec4 raw) {
    frag.rgba = raw.x;
    frag.depth = raw.y;
    frag.data = raw.z;
    frag.composition = raw.w;
}

uvec4 _raw_(inout ABufferFragment frag) {
    return uvec4(frag.rgba, frag.depth, frag.data, frag.composition);
}

#endif
