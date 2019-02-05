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

const mat3 rgb2xyz = mat3( 
  0.4124564, 0.2126729, 0.0193339,
  0.3575761, 0.7151522, 0.1191920,
  0.1804375, 0.0721750, 0.9503041 );

const mat3 xyz2rgb = mat3(
  3.2404542, -0.9692660, 0.0556434,
  -1.5371385, 1.8760108, -0.2040259,
  -0.4985314, 0.0415560, 1.0572252 );

vec3 globalToneMappingOperatorRTR(vec3 color, float exposure, float maxWhite, float aveLum) {
  // Convert color to XYZ
  vec3 xyzCol = rgb2xyz * color;

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
  return xyz2rgb * xyzCol;
}

vec3 exponentialToneMapping(vec3 color, float exposure, float gamma) {
  color *= exposure;
  
  color.r = color.r < 1.413 ? pow(color.r * 0.38317, 1.0 / gamma) : 1.0 - exp(-color.r);
  color.g = color.g < 1.413 ? pow(color.g * 0.38317, 1.0 / gamma) : 1.0 - exp(-color.g);
  color.b = color.b < 1.413 ? pow(color.b * 0.38317, 1.0 / gamma) : 1.0 - exp(-color.b);
    
  return color;
}

vec3 linearToneMapping(vec3 color, float exposure) {
  float tExposure = 0.08f;
  color = clamp(tExposure * color, 0.f, 1.f);
  return color;
}

vec3 simpleReinhardToneMapping(vec3 color, float exposure) {
  float tExposure = exposure;
  color *= tExposure/(1.f + color / tExposure);
  return color;
}

vec3 lumaBasedReinhardToneMapping(vec3 color, float exposure) {
  float luma = dot(color, vec3(0.2126f, 0.7152f, 0.0722f));
  float toneMappedLuma = luma / (1.f + luma);
  color *= toneMappedLuma / luma;
  return color;
}

vec3 photographicReinhardToneMapping(vec3 color, float exposure) {
  return color / (color + vec3(1.0));
}

vec3 whitePreservingLumaBasedReinhardToneMapping(vec3 color, float exposure, float maxWhite) {
  //float luma = dot(color, vec3(0.2126f, 0.7152f, 0.0722f));
  float luma = dot(color, vec3(0.4126f, 0.9152f, 0.2722f));
  float toneMappedLuma = luma * (1.f + luma / (maxWhite * maxWhite)) / (1.f + luma);
  color *= toneMappedLuma / luma;
  return color;
}

vec3 RomBinDaHouseToneMapping(vec3 color, float exposure) {
  color = exp( -1.f / ( 2.72f * color + 0.15f ) );
  return color;
}

vec3 filmicToneMapping(vec3 color, float exposure)
{
  color = max(vec3(0.f), color - vec3(0.04f));
  color = (color * (6.2f * color + 0.5f)) / (color * (6.2f * color + 20.f) + 0.06f);
  return color;
}

vec3 Uncharted2ToneMapping(vec3 color, float exposure) {
  float A = 0.15f;
  float B = 0.5f;
  float C = 0.1f;
  float D = 0.2f;
  float E = 0.02f;
  float F = 0.3f;
  float W = 11.2f;
  float tExposure = 0.4f;
  color *= tExposure;
  color = ((color * (A * color + C * B) + D * E) / (color * (A * color + B) + D * F)) - E / F;
  float white = ((W * (A * W + C * B) + D * E) / (W * (A * W + B) + D * F)) - E / F;
  color /= white;
  return color;
}

vec3 jToneMapping(vec3 color, float exposure) {
  return 1.0 - exp(-exposure * color);
}

vec3 gammaCorrection(vec3 color, float gamma) {
  return pow(color, vec3(1.0f / gamma));
}

vec3 HDR(vec3 color, float exposure) {
  //return exponentialToneMapping(color, exposure);  
  //return linearToneMapping(color, exposure);
  //return simpleReinhardToneMapping(color, exposure);
  //return lumaBasedReinhardToneMapping(color, exposure);
  //return whitePreservingLumaBasedReinhardToneMapping(color, exposure);
  //return RomBinDaHouseToneMapping(color, exposure);		
  return filmicToneMapping(color, exposure);
  //return Uncharted2ToneMapping(color, exposure);
  //return jToneMapping(color, exposure); 
}
