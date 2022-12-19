/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

in vec2 vs_st;
in vec3 vs_normalViewSpace;
in vec4 vs_positionCameraSpace;
in float vs_screenSpaceDepth;
in mat3 TBN;

uniform float ambientIntensity = 0.2;
uniform float diffuseIntensity = 1.0;
uniform float specularIntensity = 1.0;

uniform bool performShading = true;
uniform bool use_forced_color = false;
uniform bool has_texture_diffuse;
uniform bool has_texture_normal;
uniform bool has_texture_specular;
uniform bool has_color_specular;

uniform bool opacityBlending = false;

uniform sampler2D texture_diffuse;
uniform sampler2D texture_normal;
uniform sampler2D texture_specular;

uniform vec3 color_diffuse;
uniform vec3 color_specular;

uniform int nLightSources;
uniform vec3 lightDirectionsViewSpace[8];
uniform float lightIntensities[8];

uniform float opacity = 1.0;

Fragment getFragment() {
  Fragment frag;

  if (has_texture_normal) {
    vec3 normalAlbedo = texture(texture_normal, vs_st).rgb;
    normalAlbedo = normalize(normalAlbedo * 2.0 - 1.0);
    frag.color.rgb = normalize(TBN * normalAlbedo);
  }
  else {
    frag.color.rgb = normalize(vs_normalViewSpace);
  }
  frag.color.a = 1.0;
  frag.gPosition = vs_positionCameraSpace;
  frag.gNormal = vec4(vs_normalViewSpace, 0.0);
  frag.disableLDR2HDR = true;
  return frag;
}
