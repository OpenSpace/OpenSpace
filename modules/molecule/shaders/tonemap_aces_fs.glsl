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

uniform sampler2D u_texture;
uniform float u_exposure = 1.0;
uniform float u_gamma = 2.2;

out vec4 out_frag;

// Sources:
// https://knarkowicz.wordpress.com/2016/01/06/aces-filmic-tone-mapping-curve/
// https://github.com/TheRealMJP/BakingLab/blob/master/BakingLab/ACES.hlsl

// sRGB => XYZ => D65_2_D60 => AP1 => RRT_SAT
mat3 ACESInputMat = mat3(
  vec3(0.59719, 0.35458, 0.04823),
  vec3(0.07600, 0.90834, 0.01566),
  vec3(0.02840, 0.13383, 0.83777)
);

// ODT_SAT => XYZ => D60_2_D65 => sRGB
mat3 ACESOutputMat = mat3(
  vec3(1.60475, -0.53108, -0.07367),
  vec3(-0.10208, 1.10813, -0.00605),
  vec3(-0.00327, -0.07276, 1.07602)
);

vec3 RRTAndODTFit(vec3 v) {
  vec3 a = v * (v + 0.0245786f) - 0.000090537f;
  vec3 b = v * (0.983729f * v + 0.4329510f) + 0.238081f;
  return a / b;
}

vec3 ACESFitted(vec3 color) {
  color = color * ACESInputMat;
  color = RRTAndODTFit(color);
  color = color * ACESOutputMat;
  return clamp(color, 0.0, 1.0);
}

vec3 ACES(in vec3 c) {
  float a = 2.51f;
  float b = 0.03f;
  float y = 2.43f;
  float d = 0.59f;
  float e = 0.14f;
  return clamp((c * (a * c + b)) / (c * (y * c + d) + e), 0.0, 1.0);
}

void main() {
  vec4 in_frag = texelFetch(u_texture, ivec2(gl_FragCoord.xy), 0);
  const float exposure_bias = 0.5;
  vec3 color = in_frag.rgb * exposure_bias * u_exposure;
  color = ACESFitted(color);
  color = pow(color, vec3(1.0 / vec3(u_gamma)));
  out_frag = vec4(color, in_frag.a);
}
