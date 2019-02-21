/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

// The next defines must being synchronized with the enum defined in file renderengine.h
#define EXPONENTIAL 0
#define LINEAR 1
#define SIMPLE_REINHARD 2
#define LUM_BASED_REINHARD 3
#define WHITE_PRESERVING 4
#define ROM_BIN_DA_HOUSE 5
#define FILMIC 6
#define UNCHARTED 7
#define COSTA 8
#define ADAPTIVE 9
#define GLOBAL 10
#define PHOTOGRAPHIC_REINHARD 11
#define MIPMAPPING 12

const float HCV_EPSILON = 1e-10;
const float HSL_EPSILON = 1e-10;
const float HCY_EPSILON = 1e-10;

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
const float gammaF(const float u) {
    if (u < 0.0031308) {
        return 12.92 * u;
    } else {
        return 1.055 * pow(u, 1.0/2.4) - 0.055;
    }
}

float invgammaF(const float u) {
    if (u < 0.04045) {
        return u / 12.92;
    } else {
        return pow((u+0.055)/1.055, 2.4);
    }
}

vec3 rgbToSRGB(const vec3 rgb) {
    return vec3(gammaF(rgb.r), gammaF(rgb.g), gammaF(rgb.b));
}

vec3 srgbToRGB(const vec3 srgb) {
    return vec3(invgammaF(srgb.r), invgammaF(srgb.g), invgammaF(srgb.b));
}

vec3 srgbToXYZ(const vec3 srgb) {
    //return RGB2XYZ * srgb;
    vec3 rgb = srgbToRGB(srgb);
    return RGB2XYZ * rgb;
}

vec3 XYZToSRGB(const vec3 XYZ) {
    vec3 rgb = XYZ2RGB * XYZ;
    return rgbToSRGB(rgb);
}

// HSV code taken from http://lolengine.net/blog/2013/07/27/rgb-to-hsv-in-glsl.
vec3 rgb2hsv(const vec3 c)
{
    vec4 K = vec4(0.0, -1.0 / 3.0, 2.0 / 3.0, -1.0);
    vec4 p = mix(vec4(c.bg, K.wz), vec4(c.gb, K.xy), step(c.b, c.g));
    vec4 q = mix(vec4(p.xyw, c.r), vec4(c.r, p.yzx), step(p.x, c.r));

    float d = q.x - min(q.w, q.y);
    float e = 1.0e-10;
    return vec3(abs(q.z + (q.w - q.y) / (6.0 * d + e)), d / (q.x + e), q.x);
}

// All components are in the range [0…1], including hue.
vec3 hsv2rgb(const vec3 c)
{
    vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
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
vec3 hue2rgb(float hue)
{
    float R = abs(hue * 6 - 3) - 1;
    float G = 2 - abs(hue * 6 - 2);
    float B = 2 - abs(hue * 6 - 4);
    return clamp(vec3(R,G,B), 0, 1);
}
// Converts a value from linear RGB to HCV (Hue, Chroma, Value)
vec3 rgb2hcv(vec3 rgb)
{
    // Based on work by Sam Hocevar and Emil Persson
    vec4 P = (rgb.g < rgb.b) ? vec4(rgb.bg, -1.0, 2.0/3.0) : vec4(rgb.gb, 0.0, -1.0/3.0);
    vec4 Q = (rgb.r < P.x) ? vec4(P.xyw, rgb.r) : vec4(rgb.r, P.yzx);
    float C = Q.x - min(Q.w, Q.y);
    float H = abs((Q.w - Q.y) / (6 * C + HCV_EPSILON) + Q.z);
    return vec3(H, C, Q.x);
}

// Converts from HSL to linear RGB
vec3 hsl2rgb(vec3 hsl)
{
    vec3 rgb = hue2rgb(hsl.x);
    float C = (1 - abs(2 * hsl.z - 1)) * hsl.y;
    return (rgb - 0.5) * C + hsl.z;
}

// Converts from linear rgb to HSL
vec3 rgb2hsl(vec3 rgb)
{
    vec3 HCV = rgb2hcv(rgb);
    float L = HCV.z - HCV.y * 0.5;
    float S = HCV.y / (1 - abs(L * 2 - 1) + HSL_EPSILON);
    return vec3(HCV.x, S, L);
}

vec3 globalToneMappingOperatorRTR(vec3 color, const float exposure, const float maxWhite, const float aveLum) {
  // Convert color to XYZ
  vec3 xyzCol = RGB2XYZ * color;

  // Convert from XYZ to xyY
  float xyzSum = xyzCol.x + xyzCol.y + xyzCol.z;
  vec3 xyYCol = vec3( xyzCol.x / xyzSum, xyzCol.y / xyzSum, xyzCol.y);

  // Apply the tone mapping operation to the luminance (xyYCol.z or xyzCol.y)
  float L = (exposure * xyYCol.z) / aveLum;
  L = (L * ( 1 + L / (maxWhite * maxWhite) )) / ( 1 + L );

  // Using the new luminance, convert back to XYZ
  xyzCol.x = (L * xyYCol.x) / (xyYCol.y);
  xyzCol.y = L;
  xyzCol.z = (L * (1 - xyYCol.x - xyYCol.y))/xyYCol.y;

  // Convert back to RGB and send to output buffer
  return XYZ2RGB * xyzCol;
}

vec3 exponentialToneMapping(vec3 color, const float exposure, const float gamma) {
  color *= exposure;
  
  color.r = color.r < 1.413 ? pow(color.r * 0.38317, 1.0 / gamma) : 1.0 - exp(-color.r);
  color.g = color.g < 1.413 ? pow(color.g * 0.38317, 1.0 / gamma) : 1.0 - exp(-color.g);
  color.b = color.b < 1.413 ? pow(color.b * 0.38317, 1.0 / gamma) : 1.0 - exp(-color.b);
    
  return color;
}

vec3 linearToneMapping(vec3 color, const float exposure) {
  color = clamp(exposure * color, 0.f, 1.f);
  return color;
}

vec3 simpleReinhardToneMapping(vec3 color, const float exposure) {
  color *= exposure/(1.f + color / exposure);
  return color;
}

vec3 lumaBasedReinhardToneMapping(vec3 color) {
  
  float luma = dot(color, vec3(0.2126f, 0.7152f, 0.0722f));
  float toneMappedLuma = luma / (1.f + luma);
  color *= toneMappedLuma / luma;
  return color;
}

vec3 photographicReinhardToneMapping(vec3 color) {
  return color / (color + vec3(1.0));
}

vec3 whitePreservingLumaBasedReinhardToneMapping(vec3 color, const float maxWhite) {
  //float luma = dot(color, vec3(0.2126f, 0.7152f, 0.0722f));
  float luma = dot(color, vec3(0.4126f, 0.9152f, 0.2722f));
  float toneMappedLuma = luma * (1.f + luma / (maxWhite * maxWhite)) / (1.f + luma);
  color *= toneMappedLuma / luma;
  return color;
}

vec3 RomBinDaHouseToneMapping(vec3 color) {
  color = exp( -1.f / ( 2.72f * color + 0.15f ) );
  return color;
}

vec3 filmicToneMapping(vec3 color)
{
  color = max(vec3(0.f), color - vec3(0.04f));
  color = (color * (6.2f * color + 0.5f)) / (color * (6.2f * color + 20.f) + 0.06f);
  return color;
}

vec3 Uncharted2ToneMapping(vec3 color, const float exposure) {
  float A = 0.15f;
  float B = 0.5f;
  float C = 0.1f;
  float D = 0.2f;
  float E = 0.02f;
  float F = 0.3f;
  float W = 11.2f;
  color *= exposure;
  color = ((color * (A * color + C * B) + D * E) / (color * (A * color + B) + D * F)) - E / F;
  float white = ((W * (A * W + C * B) + D * E) / (W * (A * W + B) + D * F)) - E / F;
  color /= white;
  return color;
}

vec3 jToneMapping(vec3 color, const float exposure) {
  return 1.0 - exp(-exposure * color);
}

vec3 gammaCorrection(vec3 color, const float gamma) {
  return pow(color, vec3(1.0f / gamma));
}