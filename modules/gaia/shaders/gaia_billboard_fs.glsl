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

#version __CONTEXT__

#include "floatoperations.glsl"

in Data {
  vec4 gPosition;
  vec2 brightness;
  vec2 texCoord;
  float starDistFromSun;
  float cameraDistFromSun;
  float observedDist;
} in_data;

layout (location = 0) out vec4 out_color;

uniform sampler2D psfTexture;
uniform sampler1D colorTexture;
uniform float luminosityMultiplier;
uniform float sharpness;
uniform int renderOption;

// Keep in sync with gaiaoptions.h:RenderOption enum
const int RenderOptionStatic = 0;
const int RenderOptionColor = 1;
const int RenderOptionMotion = 2;
const float Parsec = 3.08567758e16; // 1 Parsec
const float LumLowerCap = 0.01;


vec3 color2rgb(float color) {
  // BV is [-0.4, 2.0]
  float st = (color + 0.4) / (2.0 + 0.4);

  // Bp-Rp[-2.0, 6.5], Bp-G[-2.1, 5.0], G-Rp[-1.0, 3.0]
  //float st = (color + 1.0) / (5.0 + 1.0);
  return texture(colorTexture, st).rgb;
}


void main() {
  // Assume all stars has equal luminosity as the Sun when no magnitude is loaded.
  float luminosity = 0.05;
  vec3 color = vec3(luminosity);
  float ratioMultiplier = 0.05;

  vec4 textureColor = texture(psfTexture, in_data.texCoord);
  textureColor.a = pow(textureColor.a, sharpness);
  if (textureColor.a < 0.001) {
    discard;
  }

  // Calculate the color and luminosity if we have the magnitude and B-V color.
  if (renderOption != RenderOptionStatic) {
    color = color2rgb(in_data.brightness.y);
    ratioMultiplier = 0.5;

    // Absolute magnitude is brightness a star would have at 10 pc away.
    float absoluteMagnitude = in_data.brightness.x;

    // From formula: MagSun - MagStar = 2.5*log(LumStar / LumSun), it gives that:
    // LumStar = 10^(1.89 - 0.4*Magstar) , if LumSun = 1 and MagSun = 4.72
    luminosity = pow(10.0, 1.89 - 0.4 * absoluteMagnitude);

    // If luminosity is really really small then set it to a static low number.
    if (luminosity < LumLowerCap) {
      luminosity = LumLowerCap;
    }
  }

  // Luminosity decrease by {squared} distance [measured in Pc].
  float observedDistance = in_data.observedDist / Parsec;
  luminosity /= pow(observedDistance, 2.0);

  // Multiply our color with the luminosity as well as a user-controlled property.
  color *= luminosity * pow(luminosityMultiplier, 3.0);

  // Decrease contributing brightness for stars in central cluster.
  if (in_data.cameraDistFromSun > in_data.starDistFromSun) {
    float ratio = in_datastarDistFromSun / in_data.cameraDistFromSun;
    //color *= ratio * ratioMultiplier;
  }

  // Use truncating tonemapping here so we don't overexposure individual stars.
  //color = 1.0 - 1.0 * exp(-5.0 * color.rgb);
  float maxVal = max(max(color.r, color.g), color.b);
  if (maxVal > 1.0) {
    color /= maxVal;
  }

  out_color = vec4(color, textureColor.a);
}
