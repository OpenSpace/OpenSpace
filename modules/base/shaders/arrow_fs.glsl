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
in vec3 vs_normal;
in vec4 vs_positionViewSpace;

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
    const vec3 lightDirectionViewSpace = vec3(0.0, 0.0, 1.0);
    const float lightIntensity = 1.0;
    const float specularPower = 100.0;

    // Ambient color
    vec3 shadedColor = ambientIntensity * color;

    // Diffuse
    vec3 n = normalize(vs_normal);
    vec3 l = lightDirectionViewSpace;
    shadedColor += diffuseIntensity * max(dot(n,l), 0.0) * color;

    // Specular
    vec3 viewDir = normalize(vs_positionViewSpace.xyz);
    vec3 reflectDir = reflect(l, n);
    shadedColor +=
        specularIntensity * pow(max(dot(viewDir,reflectDir), 0), specularPower) * color;

    // Light contribution (one light soruce)
    shadedColor *= lightIntensity * LightColor;

    frag.color.xyz = shadedColor;
  }

  frag.depth = vs_depth;
  frag.gPosition = vs_positionViewSpace;
  frag.gNormal = vec4(0.0, 0.0, 0.0, 1.0);
  return frag;
}
