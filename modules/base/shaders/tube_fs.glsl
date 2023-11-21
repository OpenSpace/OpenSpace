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

#include "fragment.glsl"

in float vs_depth;
in vec3 vs_normal;
in vec4 vs_positionViewSpace;

uniform vec3 color;
uniform float opacity;

uniform bool performShading = true;
uniform float ambientIntensity = 0.2;
uniform float diffuseIntensity = 1.0;
uniform float specularIntensity = 1.0;

uniform int nLightSources;
uniform vec3 lightDirectionsViewSpace[8];
uniform float lightIntensities[8];

// Could be seperated into ambinet, diffuse and specular and passed in as uniforms
const vec3 LightColor = vec3(1.0);
const float SpecularPower = 100.0;

Fragment getFragment() {
  Fragment frag;

  if (opacity == 0.0) {
    discard;
  }

  frag.color = vec4(color, opacity);

  // Ambient light
  vec3 totalLightColor = ambientIntensity * LightColor * color;

  vec3 viewDirection = normalize(vs_positionViewSpace.xyz);

  for (int i = 0; i < nLightSources; ++i) {
    // Diffuse light
    vec3 lightDirection = lightDirectionsViewSpace[i];
    float diffuseFactor =  max(dot(vs_normal, lightDirection), 0.0);
    vec3 diffuseColor = diffuseIntensity * LightColor * diffuseFactor * color;

    // Specular light
    vec3 reflectDirection = reflect(lightDirection, vs_normal);
    float specularFactor =
      pow(max(dot(viewDirection, reflectDirection), 0.0), SpecularPower);
    vec3 specularColor = specularIntensity * LightColor * specularFactor;

    totalLightColor += lightIntensities[i] * (diffuseColor + specularColor);
  }
  frag.color.rgb = totalLightColor;

  frag.depth = vs_depth;
  frag.gPosition = vs_positionViewSpace;
  frag.gNormal = vec4(vs_normal, 1.0);
  return frag;
}
