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
 
#version __CONTEXT__

#include "atmosphere_common.glsl"

out vec4 renderTarget1;

uniform int layer;

uniform sampler3D deltaSRTexture;
uniform sampler3D deltaSMTexture;

void main(void) {
  // First we convert the window's fragment coordinate to
  // texel coordinates
  vec3 rst = vec3(gl_FragCoord.xy, float(layer) + 0.5f) /
    vec3(ivec3(SAMPLES_MU_S * SAMPLES_NU, SAMPLES_MU, SAMPLES_R));
  
  vec4 rayleighInscattering0 = texture(deltaSRTexture, rst);
  vec4 mieInscattering0      = texture(deltaSMTexture, rst);
  
  // We are using only the red component of the Mie scattering
  // See the Precomputed Atmosphere Scattering paper for details about
  // the angular precision. 
  renderTarget1 = vec4(rayleighInscattering0.rgb, mieInscattering0.r); 
}
