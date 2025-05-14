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

#include "fragment.glsl"

in float vs_depth;
flat in vec3 vs_normal;
in vec4 vs_positionViewSpace;

uniform vec3 color;
uniform float opacity;

uniform float ambientIntensity = 0.2;
uniform float diffuseIntensity = 0.8;
uniform bool performShading = true;

uniform uint nLightSources;
uniform vec3 lightDirectionsViewSpace[8];
uniform float lightIntensities[8];

const vec3 LightColor = vec3(1.0);

Fragment getFragment() {
  Fragment frag;

  if (opacity == 0.0) {
    discard;
  }
  frag.color = vec4(color, opacity);

  // Simple diffuse phong shading based on light sources
  if (performShading && nLightSources > 0) {
    // @TODO: Fix faulty triangle normals. This should not have to be inverted
    vec3 n = -normalize(vs_normal);

    // Ambient color
    vec3 shadedColor = ambientIntensity  * color;

    for (int i = 0; i < nLightSources; i++) {
      vec3 l = lightDirectionsViewSpace[i];

      // Diffuse
      vec3 diffuseColor = diffuseIntensity * max(dot(n,l), 0.0) * color;

      // Light contribution
      shadedColor += lightIntensities[i] * (LightColor * diffuseColor);
    }
    frag.color.xyz = shadedColor;
  }

  frag.depth = vs_depth;
  frag.gPosition = vs_positionViewSpace;
  frag.gNormal = vec4(0.0, 0.0, 0.0, 1.0);
  return frag;
}
