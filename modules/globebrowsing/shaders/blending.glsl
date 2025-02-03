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

#ifndef BLENDING_HGLSL
#define BLENDING_HGLSL

vec4 blendNormal(vec4 oldColor, vec4 newColor) {
  vec4 toReturn;
  toReturn.a = mix(oldColor.a, 1.0, newColor.a);
  toReturn.rgb =
    (newColor.rgb * newColor.a + oldColor.rgb * oldColor.a * (1 - newColor.a)) / toReturn.a;
  return toReturn;
}

vec4 blendMultiply(vec4 oldColor, vec4 newColor) {
  return oldColor * newColor;
}

vec4 blendAdd(vec4 oldColor, vec4 newColor) {
  return oldColor + newColor;
}

vec4 blendSubtract(vec4 oldColor, vec4 newColor) {
  return oldColor - newColor;
}

vec3 hsl2rgb(in vec3 c) {
  vec3 rgb = clamp(abs(mod(c.x * 6.0 + vec3(0.0, 4.0, 2.0), 6.0) - 3.0) - 1.0, 0.0, 1.0);

  return c.z + c.y * (rgb - 0.5) * (1.0 - abs(2.0 * c.z - 1.0));
}

vec3 HueShift(in vec3 color, in float shift) {
  vec3 P = vec3(0.55735) * dot(vec3(0.55735), color);
  vec3 U = color - P;
  vec3 V = cross(vec3(0.55735), U);
  vec3 c = U * cos(shift*6.2832) + V * sin(shift*6.2832) + P;
  return c;
}

vec3 rgb2hsl(in vec3 c) {
  float r = c.r;
  float g = c.g;
  float b = c.b;
  float cMin = min(r, min(g, b));
  float cMax = max(r, max(g, b));

  if (cMax > cMin) {
    float l = (cMax + cMin) / 2.0;
    float cDelta = cMax - cMin;
    float s = (l < 0.0) ? cDelta / (cMax + cMin) : cDelta / (2.0 - (cMax + cMin));

    float h = 0.0;
    if (r == cMax) {
      h = (g - b) / cDelta;
    }
    else if (g == cMax) {
      h = 2.0 + (b - r) / cDelta;
    }
    else {
      h = 4.0 + (r - g) / cDelta;
    }

    if (h < 0.0) {
      h += 6.0;
    }
    h = h / 6.0;

    return vec3(h, s, l);
  }
  else {
    return vec3(0.0);
  }
}

vec3 rgb2hsv(vec3 c) {
  vec4 K = vec4(0.0, -1.0 / 3.0, 2.0 / 3.0, -1.0);
  vec4 p = (c.g < c.b) ? vec4(c.bg, K.wz) : vec4(c.gb, K.xy);
  vec4 q = (c.r < p.x) ? vec4(p.xyw, c.r) : vec4(c.r, p.yzx);

  float d = q.x - min(q.w, q.y);
  float e = 1.0e-10;
  return vec3(abs(q.z + (q.w - q.y) / (6.0 * d + e)), d / (q.x + e), q.x);
}

vec3 hsv2rgb(vec3 c) {
  vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
  vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
  return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
}

#endif // BLENDING_HGLSL
