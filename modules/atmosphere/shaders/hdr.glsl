/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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
 
uniform float exposure;
uniform float gamma;

vec3 exponentialToneMapping(vec3 color) {
  color *= exposure;
    
  color.r = color.r < 1.413 ? pow(color.r * 0.38317, 1.0 / gamma) : 1.0 - exp(-color.r);
  color.g = color.g < 1.413 ? pow(color.g * 0.38317, 1.0 / gamma) : 1.0 - exp(-color.g);
  color.b = color.b < 1.413 ? pow(color.b * 0.38317, 1.0 / gamma) : 1.0 - exp(-color.b);
    
  return color;

}

vec3 linearToneMapping(vec3 color)
{
  float tExposure = 1.0f;
  color = clamp(tExposure * color, 0.0f, 1.0f);
  color = pow(color, vec3(1.0f / gamma));
  return color;
}

vec3 simpleReinhardToneMapping(vec3 color)
{
  float tExposure = 1.5f;
  color *= tExposure/(1.0f + color / tExposure);
  color = pow(color, vec3(1. / gamma));
  return color;
}

vec3 lumaBasedReinhardToneMapping(vec3 color)
{
  float luma = dot(color, vec3(0.2126f, 0.7152f, 0.0722f));
  float toneMappedLuma = luma / (1.0f + luma);
  color *= toneMappedLuma / luma;
  color = pow(color, vec3(1.0f / gamma));
  return color;
}

vec3 whitePreservingLumaBasedReinhardToneMapping(vec3 color)
{
  float white = 4.0f;
  //float luma = dot(color, vec3(0.2126f, 0.7152f, 0.0722f));
  float luma = dot(color, vec3(0.4126f, 0.9152f, 0.2722f));
  float toneMappedLuma = luma * (1.0f + luma / (white * white)) / (1.0f + luma);
  color *= toneMappedLuma / luma;
  color = pow(color, vec3(1.0f / gamma));
  return color;
}

vec3 RomBinDaHouseToneMapping(vec3 color)
{
  color = exp( -1.0f / ( 2.72f * color + 0.15f ) );
  color = pow(color, vec3(1.7 / gamma));
  return color;
}

vec3 filmicToneMapping(vec3 color)
{
  color = max(vec3(0.0f), color - vec3(0.04f));
  color = (color * (6.2f * color + 0.5f)) / (color * (6.2f * color + 20.0f) + 0.06f);
  return color;
}

vec3 Uncharted2ToneMapping(vec3 color)
{
  float A = 0.15f;
  float B = 0.50f;
  float C = 0.10f;
  float D = 0.20f;
  float E = 0.02f;
  float F = 0.30f;
  float W = 11.2f;
  float tExposure = 0.4f;
  color *= tExposure;
  color = ((color * (A * color + C * B) + D * E) / (color * (A * color + B) + D * F)) - E / F;
  float white = ((W * (A * W + C * B) + D * E) / (W * (A * W + B) + D * F)) - E / F;
  color /= white;
  color = pow(color, vec3(1.0f / gamma));
  return color;
}

vec3 HDR(vec3 color) {
  return exponentialToneMapping(color);  
  //return linearToneMapping(color);
  //return simpleReinhardToneMapping(color);
  //return lumaBasedReinhardToneMapping(color);
  //return whitePreservingLumaBasedReinhardToneMapping(color);
  //return RomBinDaHouseToneMapping(color);		
  //return filmicToneMapping(color);
  //return Uncharted2ToneMapping(color);
  
}
