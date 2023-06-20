/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#version __CONTEXT__

#include "floatoperations.glsl"

in vec2 ge_brightness;
in vec4 ge_gPosition;
in float ge_starDistFromSun;
in float ge_cameraDistFromSun;
in float ge_observedDist;
in float ge_otherData;
in float gs_screenSpaceDepth;
in float ge_cameraDistance;

layout (location = 0) out vec4 outColor;

uniform sampler1D colorTexture;
uniform float luminosityMultiplier;
uniform int renderOption;
uniform float viewScaling;
uniform bool staticLuminosity;

//thesis 2023
uniform vec2 colorRange;


const float ONE_PARSEC = 3.08567758e16; // 1 Parsec
const float LUM_LOWER_CAP = 0.01;


vec3 color2rgb(float color) {
  //Normalize st coordinate between min and max range to [0,1]
  color = clamp(color, colorRange.x, colorRange.y);
  float st = (color - colorRange.x) / (colorRange.y - colorRange.x);
  return texture(colorTexture, st).rgb;
}


void main() {
  // Assume all stars has equal luminosity as the Sun when no magnitude is loaded.
  float luminosity = 0.05;
  vec3 color = vec3(luminosity);
  float ratioMultiplier = 1.0;

  // color = color2rgb(ge_brightness.y); 
  if(ge_otherData >colorRange.y || ge_otherData < colorRange.x) {
    discard;
  }
  color = color2rgb(ge_otherData);
  ratioMultiplier = 0.01;

  // Absolute magnitude is brightness a star would have at 10 pc away.
  float absoluteMagnitude = ge_brightness.x;

  // From formula: MagSun - MagStar = 2.5*log(LumStar / LumSun), it gives that:
  // LumStar = 10^(1.89 - 0.4*Magstar) , if LumSun = 1 and MagSun = 4.72
  luminosity = pow(10.0, 1.89 - 0.4 * absoluteMagnitude);

  // If luminosity is really really small then set it to a static low number.
  if (luminosity < LUM_LOWER_CAP) {
    luminosity = LUM_LOWER_CAP;
  }
  
  // Luminosity decrease by {squared} distance [measured in Pc].
  float observedDistance = ge_observedDist / ONE_PARSEC;
  luminosity /= pow(observedDistance, 2.0);

  // float foo = clamp(ge_cameraDistance, 1.0, 1.0);
  // if(ge_cameraDistance < 0.1) {
  //   color = vec3(1.0, 0,0);
  //   outColor = vec4(color, 1.0f);
  //   return;
  // }
  // luminosity /= ge_cameraDistance;

  // Multiply our color with the luminosity as well as a user-controlled property.
  color *= luminosity * pow(ge_cameraDistance, 3.0);

  // Decrease contributing brightness for stars in central cluster.
  if (ge_cameraDistFromSun > ge_starDistFromSun) {
    float ratio = ge_starDistFromSun / ge_cameraDistFromSun;
    // color *= ratio * ratioMultiplier;
  }

  if(staticLuminosity) {
    float lumi = 0.0001 * pow(luminosityMultiplier, 1.5);
    color = color2rgb(ge_otherData);
    color *= lumi;
  }
  
  // if(gl_FragCoord.x < 850)
  //   color = vec3(1.0, 0,0);
  // float maxVal = max(max(color.r, color.g), color.b);
  // if (maxVal > 1.0) {
  //   color /= maxVal;
  // }
  // color.r = clamp(color.r, 0.0, 1.0);
  // color.g = clamp(color.g, 0.0, 1.0);
  // color.b = clamp(color.b, 0.0, 1.0);
  // color = vec3(gl_FragCoord.z);
  // color = vec3(ge_cameraDistance, 0.f, 0.f);

  gl_FragDepth = gs_screenSpaceDepth;

  outColor = vec4(color, 1.0f);
}
