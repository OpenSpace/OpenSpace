/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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
in mat3 vs_TBN;
in vec3 vs_color;

uniform float ambientIntensity = 0.2;
uniform float diffuseIntensity = 1.0;
uniform float specularIntensity = 1.0;
uniform bool performShading = true;

uniform bool use_forced_color = false;
uniform bool use_vertex_colors = false;
uniform bool has_texture_diffuse;
uniform bool has_texture_normal;
uniform bool has_texture_specular;
uniform bool has_color_specular;

uniform sampler2D texture_diffuse;
uniform sampler2D texture_normal;
uniform sampler2D texture_specular;

uniform vec4 color_diffuse;
uniform vec4 color_specular;
uniform float opacity = 1.0;

uniform int nLightSources;
uniform vec3 lightDirectionsViewSpace[8];
uniform float lightIntensities[8];

uniform bool performManualDepthTest = false;
uniform sampler2D gBufferDepthTexture;

uniform vec2 resolution;

Fragment getFragment() {
  Fragment frag;
  frag.depth = vs_screenSpaceDepth;
  frag.gPosition = vs_positionCameraSpace;
  frag.gNormal = vec4(vs_normalViewSpace, 0.0);
  frag.disableLDR2HDR = true;
  frag.color.a = opacity;

  if (performManualDepthTest) {
    // gl_FragCoord.x goes from 0 to resolution.x and gl_FragCoord.y goes from 0 to
    // resolution.y, need to normalize it
    vec2 texCoord = gl_FragCoord.xy;
    texCoord.x = texCoord.x / resolution.x;
    texCoord.y = texCoord.y / resolution.y;

    // Manual depth test
    float gBufferDepth = denormalizeFloat(texture(gBufferDepthTexture, texCoord).x);
    if (vs_screenSpaceDepth > gBufferDepth) {
      frag.color = vec4(0.0);
      frag.depth = gBufferDepth;
      return frag;
    }
  }

  // Render invisible mesh with flashy procedural material
  if (use_forced_color) {
    vec3 adjustedPos = floor(vs_positionCameraSpace.xyz / 500.0);
    float chessboard  = adjustedPos.x + adjustedPos.y + adjustedPos.z;
    chessboard = fract(chessboard * 0.5);
    chessboard *= 2;

    // Pink and complementary green in a chessboard pattern
    frag.color.rgb = mix(vec3(1.0, 0.0, 0.8), vec3(0.0, 1.0, 0.2), chessboard);
    return frag;
  }

  // Base color
  vec4 diffuseAlbedo = vec4(0.0);
  if (has_texture_diffuse) {
    diffuseAlbedo = texture(texture_diffuse, vs_st);
  }
  else {
    diffuseAlbedo = color_diffuse;
  }

  // Multiply with vertex color if specified
  if (use_vertex_colors) {
    diffuseAlbedo.rgb *= vs_color;

    // Make sure to not go beyond color range
    diffuseAlbedo.r = clamp(diffuseAlbedo.r, 0.0, 1.0);
    diffuseAlbedo.g = clamp(diffuseAlbedo.g, 0.0, 1.0);
    diffuseAlbedo.b = clamp(diffuseAlbedo.b, 0.0, 1.0);
  }

  if (performShading) {
    // Specular color
    vec3 specularAlbedo;
    if (has_texture_specular) {
      specularAlbedo = texture(texture_specular, vs_st).rgb;
    }
    else {
      if (has_color_specular) {
        specularAlbedo = color_specular.rgb;
      }
      else {
        specularAlbedo = diffuseAlbedo.rgb;
      }
    }

    // Bump mapping
    vec3 normal;
    if (has_texture_normal) {
      vec3 normalAlbedo = texture(texture_normal, vs_st).rgb;
      normalAlbedo = normalize(normalAlbedo * 2.0 - 1.0);
      normal = normalize(vs_TBN * normalAlbedo);
    }
    else {
      normal = normalize(vs_normalViewSpace);
    }
    frag.gNormal = vec4(normal, 0.0);

    // Could be seperated into ambinet, diffuse and specular and passed in as uniforms
    const vec3 lightColor = vec3(1.0);
    const float specularPower = 100.0;

    // Ambient light
    vec3 totalLightColor = ambientIntensity * lightColor * diffuseAlbedo.rgb;

    vec3 viewDirection = normalize(vs_positionCameraSpace.xyz);

    for (int i = 0; i < nLightSources; i++) {
      // Diffuse light
      vec3 lightDirection = lightDirectionsViewSpace[i];
      float diffuseFactor =  max(dot(normal, lightDirection), 0.0);
      vec3 diffuseColor =
        diffuseIntensity * lightColor * diffuseFactor * diffuseAlbedo.rgb;

      // Specular light
      vec3 reflectDirection = reflect(lightDirection, normal);
      float specularFactor =
        pow(max(dot(viewDirection, reflectDirection), 0.0), specularPower);
      vec3 specularColor =
        specularIntensity * lightColor * specularFactor * specularAlbedo;

      totalLightColor += lightIntensities[i] * (diffuseColor + specularColor);
    }
    frag.color.rgb = totalLightColor;
  }
  else {
    frag.color.rgb = diffuseAlbedo.rgb;
  }

  frag.color.a = diffuseAlbedo.a * opacity;
  return frag;
}
