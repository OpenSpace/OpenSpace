/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#ifndef _BLENDING_GLSL_
#define _BLENDING_GLSL_

/**
 * Blend in src behind dst using normal blending
 * dst is premultiplied
 * src is expressed in straight RGBA
 */
void normalBlend(inout vec4 dst, vec4 src) {
  dst.rgb = dst.rgb + (1.f - dst.a) * src.a * src.rgb;
  dst.a = dst.a + (1.f - dst.a) * src.a;
}

/**
 * Blend in src behind dst using additive blending
 * dst is premultiplied
 * src is expressed in straight RGBA
 */
void additiveBlend(inout vec4 dst, vec4 src) {
  dst.rgb = dst.rgb + (1.f - dst.a) * src.a * src.rgb;
}


/**
 * Blend in src behind dst using normal blending
 * dst is premultiplied
 * src is expressed in straight RGBA
 * stepSize = 0: alpha becomes 0
 * stepSize = 1: alpha becomes src.a
 */
void normalBlendStep(inout vec4 dst, vec4 src, float stepSize) {
  src.a = 1.f - pow(1.f - src.a, stepSize);
  normalBlend(dst, src);
}

/**
 * Blend in src behind dst using addivtive blending
 * dst is premultiplied
 * src is expressed in straight RGBA
 * stepSize = 0: alpha becomes 0
 * stepSize = 1: alpha becomes src.a
 */
void additiveBlendStep(inout vec4 dst, vec4 src, float stepSize) {
  src.a = 1.0 - pow(1.0 - src.a, stepSize);
  additiveBlend(dst, src);
}

/**
 * Blend in src behind dst
 * dst is premultiplied
 * src is expressed in straight RGBA
 */
void blend(inout vec4 dst, vec4 src) {
  dst.rgb = dst.rgb + (1.f - dst.a) * src.a * src.rgb;
  dst.a = dst.a + (1.f - dst.a) * src.a;
}

/**
 * Blend in src behind dst
 * dst is premultiplied
 * src is expressed in straight RGBA
 * stepSize = 0: alpha becomes 0
 * stepSize = 1: alpha becomes src.a
 */
void blendStep(inout vec4 dst, vec4 src, float stepSize) {
  src.a = 1.0 - pow(1.f - src.a, stepSize);
  blend(dst, src);
}

#endif // _BLENDING_GLSL_
