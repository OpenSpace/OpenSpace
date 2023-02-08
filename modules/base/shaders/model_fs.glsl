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

in vec2 vs_st;
in vec2 viewportPixelCoord;
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
uniform sampler2D texture_diffuse;
uniform sampler2D texture_normal;
uniform sampler2D texture_specular;
uniform vec4 color_diffuse;
uniform vec4 color_specular;
uniform int nLightSources;
uniform vec3 lightDirectionsViewSpace[8];
uniform float lightIntensities[8];

Fragment getFragment() {
  Fragment frag;
  frag.depth = vs_screenSpaceDepth;
  frag.gPosition = vs_positionCameraSpace;
  frag.gNormal = vec4(vs_normalViewSpace, 0.0);
  frag.disableLDR2HDR = true;

  // Render invisible mesh with flashy procedural material
  if (use_forced_color) {
    vec3 adjustedPos = floor(vs_positionCameraSpace.xyz * 3.0);
    float chessboard  = adjustedPos.x + adjustedPos.y + adjustedPos.z;
    chessboard = fract(chessboard * 0.5);
    chessboard *= 2;

    // Pink and complementary green in a chessboard pattern
    frag.color.rgb = mix(vec3(1.0, 0.0, 0.8), vec3(0.0, 1.0, 0.2), chessboard);
    frag.color.a = 1.0;
    return frag;
  }

  vec4 diffuseAlbedo;
  if (has_texture_diffuse) {
    diffuseAlbedo = texture(texture_diffuse, vs_st);
  }
  else {
    diffuseAlbedo = color_diffuse;
  }

  if (performShading) {
    vec3 specularAlbedo;
    if (has_texture_specular) {
      specularAlbedo = texture(texture_specular, vs_st).rgb;
    }
    else {
      if (has_color_specular) {
        specularAlbedo = color_specular.rgb ;
      }
      else {
        specularAlbedo = vec3(1.0);
      }
    }

    // Some of these values could be passed in as uniforms
    const vec3 lightColorAmbient = vec3(1.0);
    const vec3 lightColor = vec3(1.0);

    vec3 n;
    if (has_texture_normal) {
      vec3 normalAlbedo = texture(texture_normal, vs_st).rgb;
      normalAlbedo = normalize(normalAlbedo * 2.0 - 1.0);
      n = normalize(TBN * normalAlbedo);
    }
    else {
      n = normalize(vs_normalViewSpace);
    }

    vec3 c = normalize(vs_positionCameraSpace.xyz);

    vec3 color = ambientIntensity * lightColorAmbient * diffuseAlbedo.rgb;

    for (int i = 0; i < nLightSources; ++i) {
      vec3 l = lightDirectionsViewSpace[i];
      vec3 r = reflect(l, n);

      float diffuseCosineFactor = dot(n,l);
      float specularCosineFactor = dot(c,r);
      const float specularPower = 100.0;

      vec3 diffuseColor =
        diffuseIntensity * lightColor * diffuseAlbedo.rgb * max(diffuseCosineFactor, 0);

      vec3 specularColor =
        specularIntensity * lightColor * specularAlbedo *
          pow(max(specularCosineFactor, 0), specularPower);

      color += lightIntensities[i] * (diffuseColor + specularColor);
    }
    frag.color.rgb = color;
  }
  else {
    frag.color.rgb = diffuseAlbedo.rgb;
  }

  frag.color.a = diffuseAlbedo.a;
  return frag;
}
