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

const float HCV_EPSILON = 1e-7;
const float HSL_EPSILON = 1e-7;
const float HCY_EPSILON = 1e-7;

// White given by D65
const mat3 RGB2XYZ = mat3(
  vec3(0.4124, 0.2126, 0.0193),
  vec3(0.3576, 0.7152, 0.1192),
  vec3(0.1805, 0.0722, 0.9505)
);

const mat3 XYZ2RGB = mat3(
  vec3(3.2406, -0.9689, 0.0557),
  vec3(-1.5372, 1.8758, -0.2040),
  vec3(-0.4986, 0.0415, 1.0570)
);

// Gamma correction for linear RGB to sRGB
// See wiki: https://en.wikipedia.org/wiki/SRGB#The_sRGB_transfer_function_.28.22gamma.22.29
float gammaF(float u) {
  if (u < 0.0031308) {
    return 12.92 * u;
  }
  else {
    return 1.055 * pow(u, 1.0 / 2.4) - 0.055;
  }
}

float invgammaF(float u) {
  if (u < 0.04045) {
    return u / 12.92;
  }
  else {
    return pow((u + 0.055) / 1.055, 2.4);
  }
}

vec3 rgbToSRGB(vec3 rgb) {
  return vec3(gammaF(rgb.r), gammaF(rgb.g), gammaF(rgb.b));
}

vec3 srgbToRGB(vec3 srgb) {
  return vec3(invgammaF(srgb.r), invgammaF(srgb.g), invgammaF(srgb.b));
}

vec3 srgbToXYZ(vec3 srgb) {
  //return RGB2XYZ * srgb;
  vec3 rgb = srgbToRGB(srgb);
  return RGB2XYZ * rgb;
}

vec3 XYZToSRGB(vec3 XYZ) {
  vec3 rgb = XYZ2RGB * XYZ;
  return rgbToSRGB(rgb);
}

// HSV code taken from http://lolengine.net/blog/2013/07/27/rgb-to-hsv-in-glsl.
vec3 rgb2hsv(vec3 c) {
  c = clamp(c, 0.0, 1.0);
  const vec4 K = vec4(0.0, -1.0 / 3.0, 2.0 / 3.0, -1.0);
  vec4 p = mix(vec4(c.bg, K.wz), vec4(c.gb, K.xy), step(c.b, c.g));
  vec4 q = mix(vec4(p.xyw, c.r), vec4(c.r, p.yzx), step(p.x, c.r));

  float d = q.x - min(q.w, q.y);
  float e = 1.0e-10;
  return vec3(abs(q.z + (q.w - q.y) / (6.0 * d + e)), d / (q.x + e), q.x);
}

// All components are in the range [0…1], including hue.
vec3 hsv2rgb(vec3 c) {
  const vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
  vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
  return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
}

// Code to convert from rgb to hsl is licensed by:
/*
GLSL Color Space Utility Functions
(c) 2015 tobspr
-------------------------------------------------------------------------------
The MIT License (MIT)
Copyright (c) 2015

See top of the file for full license terms.
*/

// Converts from pure Hue to linear RGB
vec3 hue2rgb(float hue) {
  float R = abs(hue * 6 - 3) - 1;
  float G = 2 - abs(hue * 6 - 2);
  float B = 2 - abs(hue * 6 - 4);
  return clamp(vec3(R,G,B), 0, 1);
}

// Converts a value from linear RGB to HCV (Hue, Chroma, Value)
vec3 rgb2hcv(vec3 rgb) {
  // Based on work by Sam Hocevar and Emil Persson
  vec4 P = (rgb.g < rgb.b) ? vec4(rgb.bg, -1.0, 2.0/3.0) : vec4(rgb.gb, 0.0, -1.0/3.0);
  vec4 Q = (rgb.r < P.x) ? vec4(P.xyw, rgb.r) : vec4(rgb.r, P.yzx);
  float C = Q.x - min(Q.w, Q.y);
  float H = abs((Q.w - Q.y) / (6 * C + HCV_EPSILON) + Q.z);
  return vec3(H, C, Q.x);
}

// Converts from HSL to linear RGB
vec3 hsl2rgb(vec3 hsl) {
  vec3 rgb = hue2rgb(hsl.x);
  float C = (1 - abs(2 * hsl.z - 1)) * hsl.y;
  return (rgb - 0.5) * C + hsl.z;
}

// Converts from linear rgb to HSL
vec3 rgb2hsl(vec3 rgb) {
  vec3 HCV = rgb2hcv(rgb);
  float L = HCV.z - HCV.y * 0.5;
  float S = HCV.y / (1 - abs(L * 2 - 1) + HSL_EPSILON);
  return vec3(HCV.x, S, L);
}

vec3 exponentialToneMapping(vec3 color, float exposure, float gamma) {
  color *= exposure;

  color.r = color.r < 1.413 ? pow(color.r * 0.38317, 1.0 / gamma) : 1.0 - exp2(-exposure * color.r);
  color.g = color.g < 1.413 ? pow(color.g * 0.38317, 1.0 / gamma) : 1.0 - exp2(-exposure * color.g);
  color.b = color.b < 1.413 ? pow(color.b * 0.38317, 1.0 / gamma) : 1.0 - exp2(-exposure * color.b);

  return color;
}

vec3 toneMappingOperator(vec3 color, float exposure) {
  return 1.0 - exp2(-exposure * color);
}

vec3 gammaCorrection(vec3 color, float gamma) {
  return pow(color, vec3(1.0f / gamma));
}
