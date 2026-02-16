/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#ifndef _GLOBEBROWSING___BLENDING___GLSL_
#define _GLOBEBROWSING___BLENDING___GLSL_

vec4 blendNormal(vec4 oldColor, vec4 newColor) {
  float a = mix(oldColor.a, 1.0, newColor.a);
  return vec4(mix(oldColor.rgb * oldColor.a, newColor.rgb, newColor.a) / a, a);
}

vec4 blendMultiply(vec4 oldColor, vec4 newColor) {
  return oldColor * newColor;
}

vec4 blendMultiplyMix(vec4 oldColor, vec4 newColor, float blendFactor) {
  return blendNormal(oldColor, blendMultiply(oldColor, newColor * blendFactor));
}

vec4 blendAdd(vec4 oldColor, vec4 newColor) {
  return oldColor + newColor;
}

vec4 blendSubtract(vec4 oldColor, vec4 newColor) {
  return oldColor - newColor;
}

vec3 hsl2rgb(vec3 c) {
  vec3 rgb = clamp(abs(mod(c.x * 6.0 + vec3(0.0, 4.0, 2.0), 6.0) - 3.0) - 1.0, 0.0, 1.0);
  return c.z + c.y * (rgb - 0.5) * (1.0 - abs(2.0 * c.z - 1.0));
}

vec3 rgb2hsl(vec3 c) {
  float cMin = min(c.r, min(c.g, c.b));
  float cMax = max(c.r, max(c.g, c.b));

  if (cMax <= cMin) {
    return vec3(0.0);
  }

  float l = (cMax + cMin) / 2.0;
  float cDelta = cMax - cMin;
  float s = (l < 0.0)  ?  cDelta / (cMax + cMin)  :  cDelta / (2.0 - (cMax + cMin));

  float h = 0.0;
  if (c.r == cMax) {
    h = (c.g - c.b) / cDelta;
  }
  else if (c.g == cMax) {
    h = 2.0 + (c.b - c.r) / cDelta;
  }
  else {
    h = 4.0 + (c.r - c.g) / cDelta;
  }

  if (h < 0.0) {
    h += 6.0;
  }
  h = h / 6.0;

  return vec3(h, s, l);
}

vec3 rgb2hsv(vec3 c) {
  const vec4 K = vec4(0.0, -1.0 / 3.0, 2.0 / 3.0, -1.0);
  vec4 p = (c.g < c.b)  ?  vec4(c.bg, K.wz)  :  vec4(c.gb, K.xy);
  vec4 q = (c.r < p.x)  ?  vec4(p.xyw, c.r)  :  vec4(c.r, p.yzx);

  float d = q.x - min(q.w, q.y);
  const float e = 1.0e-10;
  return vec3(abs(q.z + (q.w - q.y) / (6.0 * d + e)), d / (q.x + e), q.x);
}

vec3 hsv2rgb(vec3 c) {
  const vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
  vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
  return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
}

#endif // _GLOBEBROWSING___BLENDING___GLSL_
