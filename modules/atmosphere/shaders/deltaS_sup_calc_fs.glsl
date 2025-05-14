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

#version __CONTEXT__

#include "atmosphere_common.glsl"

out vec4 renderTarget;

uniform int SAMPLES_R;
uniform int SAMPLES_MU;
uniform int SAMPLES_MU_S;
uniform int SAMPLES_NU;
uniform int layer;
uniform sampler3D deltaSTexture;


void main() {
  vec2 p = gl_FragCoord.xy - vec2(0.5);

  float nu = -1.0 + floor(p.x / float(SAMPLES_MU_S)) / (float(SAMPLES_NU) - 1.0) * 2.0;
  vec3 uvw =
    vec3(gl_FragCoord.xy, float(layer) + 0.5) /
    vec3(ivec3(SAMPLES_MU_S * SAMPLES_NU, SAMPLES_MU, SAMPLES_R));

  // See Bruneton and Neyret paper, "Angular Precision" paragraph to understanding why we
  // are dividing the S[L*] by the Rayleigh phase function.
  renderTarget = vec4(texture(deltaSTexture, uvw).rgb / rayleighPhaseFunction(nu), 0.0);
 }
