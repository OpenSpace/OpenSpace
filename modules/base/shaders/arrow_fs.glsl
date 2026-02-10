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

#include "fragment.glsl"

in Data {
  in vec4 positionViewSpace;
  in vec3 normal;
  in float depth;
} in_data;

uniform vec3 color;
uniform float opacity;
uniform float ambientIntensity = 0.2;
uniform float diffuseIntensity = 1.0;
uniform float specularIntensity = 1.0;
uniform bool performShading = true;

const vec3 LightColor = vec3(1.0);


Fragment getFragment() {
  Fragment frag;

  frag.color = vec4(color, opacity);

  // Simple phong shading (same color for diffuse and ambient. White specular)
  if (performShading) {
    const vec3 LightDirectionViewSpace = vec3(0.0, 0.0, 1.0);
    const float LightIntensity = 1.0;
    const float SpecularPower = 100.0;

    // Ambient color
    vec3 c = ambientIntensity * color;

    // Diffuse
    vec3 n = normalize(in_data.normal);
    vec3 l = LightDirectionViewSpace;
    c += diffuseIntensity * max(dot(n, l), 0.0) * color;

    // Specular
    vec3 viewDir = normalize(in_data.positionViewSpace.xyz);
    vec3 refDir = reflect(l, n);
    c += specularIntensity * pow(max(dot(viewDir, refDir), 0.0), SpecularPower) * color;

    // Light contribution (one light soruce)
    frag.color.rgb = c * LightIntensity * LightColor;
  }

  frag.depth = in_data.depth;
  frag.gPosition = in_data.positionViewSpace;
  frag.gNormal = vec4(0.0, 0.0, 0.0, 1.0);
  return frag;
}
