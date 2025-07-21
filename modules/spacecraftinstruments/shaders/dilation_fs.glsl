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

in vec2 vs_uv;
out vec4 color;

uniform sampler2D tex;
uniform sampler2D stencil;

// We conside the 8-neighborhood of a texel, so going a stepsize of '1' in both directions
vec2 offsets[8] = vec2[](
  vec2(-1.0, -1.0),
  vec2(-1.0,  0.0),
  vec2(-1.0,  1.0),
  vec2( 0.0, -1.0),
  vec2( 0.0,  1.0),
  vec2( 1.0, -1.0),
  vec2( 1.0,  0.0),
  vec2( 1.0,  1.0)
);


// Collect the contributing colors from the neighboring texels and return the averaged
// value of all texels that passed the masking test based on 'stencil'
vec3 gatherNeighboringColors() {
  vec2 texSize = textureSize(tex, 0);

  // The total number of contributing texels
  int nContributions = 0;

  // The summed color of all contributing texels
  vec3 totalColor = vec3(0.0);

  for (int i = 0; i < 8; i++) {
    // gl_FragCoord is in pixel coordinates; the ProjectionComponent sets the viewport
    // such that pixels=texels, so we can use gl_FragCoord as an integer texel coordinate
    // First offsetting them, then dividing by the texture size to get to [0,1]
    vec2 samplePosition = (gl_FragCoord.xy + offsets[i]) / texSize;

    // The stencelling determines the areas that we have to enlarge, such that we do not
    // enlarge a previously enlarged area
    if (texture(stencil, samplePosition).r != 0.0) {
      totalColor += texture(tex, samplePosition).rgb;
      nContributions++;
    }
  }

  // GLSL normally doesn't have a problem taking vec3(0.0)/0.0 but we don't want to tempt
  // the compiler gods
  if (nContributions > 0) {
    return totalColor / nContributions;
  }
  else {
    return vec3(0.0);
  }
}


void main() {
  if (texture(stencil, vs_uv).r == 0.0) {
    // This means that the current fragment/texel we are looking at has not been projected
    // on and we only want to do the dilation into these texels
    color = vec4(gatherNeighboringColors(), 1.0);
  }
  else {
    // We are in a region where an image has been projected, so we can reuse the already
    // sampled version
    color = texture(tex, vs_uv);
  }
}
