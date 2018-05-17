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
 
uniform float gamma;

vec3 exponentialToneMapping(vec3 color, float exposure) {
  color *= exposure;
  
  color.r = color.r < 1.413 ? pow(color.r * 0.38317, 1.0 / gamma) : 1.0 - exp(-color.r);
  color.g = color.g < 1.413 ? pow(color.g * 0.38317, 1.0 / gamma) : 1.0 - exp(-color.g);
  color.b = color.b < 1.413 ? pow(color.b * 0.38317, 1.0 / gamma) : 1.0 - exp(-color.b);
    
  return color;
}

vec3 linearToneMapping(vec3 color, float exposure) {
  float tExposure = 0.08f;
  color = clamp(tExposure * color, 0.f, 1.f);
  color = pow(color, vec3(1.f / gamma));
  return color;
}

vec3 simpleReinhardToneMapping(vec3 color, float exposure) {
  float tExposure = 1.5f;
  color *= tExposure/(1.f + color / tExposure);
  color = pow(color, vec3(1.f / gamma));
  return color;
}

vec3 lumaBasedReinhardToneMapping(vec3 color, float exposure) {
  float luma = dot(color, vec3(0.2126f, 0.7152f, 0.0722f));
  float toneMappedLuma = luma / (1.f + luma);
  color *= toneMappedLuma / luma;
  color = pow(color, vec3(1.f / gamma));
  return color;
}

vec3 whitePreservingLumaBasedReinhardToneMapping(vec3 color, float exposure) {
  float white = 4.f;
  //float luma = dot(color, vec3(0.2126f, 0.7152f, 0.0722f));
  float luma = dot(color, vec3(0.4126f, 0.9152f, 0.2722f));
  float toneMappedLuma = luma * (1.f + luma / (white * white)) / (1.f + luma);
  color *= toneMappedLuma / luma;
  color = pow(color, vec3(1.f / gamma));
  return color;
}

vec3 RomBinDaHouseToneMapping(vec3 color, float exposure) {
  color = exp( -1.f / ( 2.72f * color + 0.15f ) );
  color = pow(color, vec3(1.7f / gamma));
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
  color = pow(color, vec3(1.0f / gamma));
  return color;
}

vec3 jToneMapping(vec3 color, float exposure) {
  return 1.0 - exp(-exposure * color);
}

vec3 HDR(vec3 color, float exposure) {
  //return exponentialToneMapping(color, exposure);  
  //return linearToneMapping(color, exposure);
  //return simpleReinhardToneMapping(color, exposure);
  //return lumaBasedReinhardToneMapping(color, exposure);
  //return whitePreservingLumaBasedReinhardToneMapping(color, exposure);
  //return RomBinDaHouseToneMapping(color, exposure);		
  //return filmicToneMapping(color, exposure);
  //return Uncharted2ToneMapping(color, exposure);
  return jToneMapping(color, exposure); 
}
