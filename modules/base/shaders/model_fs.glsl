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
  mat3 tbn;
  vec4 positionCameraSpace;
  vec3 normalViewSpace;
  vec3 color;
  vec2 st;
  float screenSpaceDepth;
} in_data;

uniform float ambientIntensity = 0.2;
uniform float diffuseIntensity = 1.0;
uniform float specularIntensity = 1.0;
uniform float specularPower = 100.0;
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

// See renderableglobe and renderer_fs.glsl
uniform bool has_shadow_depth_map;
uniform sampler2D shadow_depth_map;
in vec4 lightspace_position;

uniform bool has_override_color;
uniform vec4 override_color;


Fragment getFragment() {
  Fragment frag;
  frag.depth = in_data.screenSpaceDepth;
  frag.gPosition = in_data.positionCameraSpace;
  frag.gNormal = vec4(in_data.normalViewSpace, 0.0);
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
    if (in_data.screenSpaceDepth > gBufferDepth) {
      frag.color = vec4(0.0);
      frag.depth = gBufferDepth;
      return frag;
    }
  }

  // Render invisible mesh with flashy procedural material
  if (use_forced_color) {
    vec3 adjustedPos = floor(in_data.positionCameraSpace.xyz / 500.0);
    float chessboard = adjustedPos.x + adjustedPos.y + adjustedPos.z;
    chessboard = fract(chessboard * 0.5);
    chessboard *= 2.0;

    // Pink and complementary green in a chessboard pattern
    frag.color.rgb = mix(vec3(1.0, 0.0, 0.8), vec3(0.0, 1.0, 0.2), chessboard);
    return frag;
  }

  // Base color
  vec4 diffuseAlbedo = vec4(0.0);
  if (has_texture_diffuse) {
    diffuseAlbedo = texture(texture_diffuse, in_data.st);
  }
  else {
    diffuseAlbedo = color_diffuse;
  }

  // Multiply with vertex color if specified
  if (use_vertex_colors) {
    diffuseAlbedo.rgb *= in_data.color;

    // Make sure to not go beyond color range
    diffuseAlbedo.r = clamp(diffuseAlbedo.r, 0.0, 1.0);
    diffuseAlbedo.g = clamp(diffuseAlbedo.g, 0.0, 1.0);
    diffuseAlbedo.b = clamp(diffuseAlbedo.b, 0.0, 1.0);
  }

  if (performShading) {
    // Specular color
    vec3 specularAlbedo;
    if (has_texture_specular) {
      specularAlbedo = texture(texture_specular, in_data.st).rgb;
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
      vec3 normalAlbedo = texture(texture_normal, in_data.st).rgb;
      normalAlbedo = normalize(normalAlbedo * 2.0 - 1.0);
      normal = normalize(in_data.tbn * normalAlbedo);
    }
    else {
      normal = normalize(in_data.normalViewSpace);
    }
    frag.gNormal = vec4(normal, 0.0);

    // Could be seperated into ambinet, diffuse and specular and passed in as uniforms
    const vec3 lightColor = vec3(1.0);

    // Ambient light
    vec3 totalLightColor = ambientIntensity * lightColor * diffuseAlbedo.rgb;

    vec3 viewDirection = normalize(in_data.positionCameraSpace.xyz);
    vec3 totalSpecularColor = vec3(0.0);

    for (int i = 0; i < nLightSources; i++) {
      // Diffuse light
      vec3 lightDirection = lightDirectionsViewSpace[i];
      float diffuseFactor = max(dot(normal, lightDirection), 0.0);
      vec3 diffuseColor =
        diffuseIntensity * lightColor * diffuseFactor * diffuseAlbedo.rgb;

      // Specular light
      vec3 reflectDirection = reflect(lightDirection, normal);
      float specularFactor =
        pow(max(dot(viewDirection, reflectDirection), 0.0), specularPower);
      vec3 specularColor =
        specularIntensity * lightColor * specularFactor * specularAlbedo;

      totalLightColor += lightIntensities[i] * diffuseColor;
      totalSpecularColor += lightIntensities[i] * specularColor;
    }
    frag.color.rgb = totalLightColor + totalSpecularColor;

    if (has_shadow_depth_map) {
      const float Bias = 0.0005;
      vec3 coords = 0.5 + 0.5 * lightspace_position.xyz / lightspace_position.w;

      // Any fragment that is behind the stored depth is in shadow, multisampling for
      // smoother shadows
      const int ShadowFilterSize = 3;
      int accum = 0;
      vec2 stepSize = 1.0 / textureSize(shadow_depth_map, 0);
      for (int x = -ShadowFilterSize; x <= ShadowFilterSize; x++) {
        for (int y = -ShadowFilterSize; y <= ShadowFilterSize; y++) {
          float depth = texture(
            shadow_depth_map,
            coords.xy + vec2(x * stepSize.x, y * stepSize.y)
          ).r;
          if (coords.z < 1.0 && depth > coords.z - Bias) {
            accum++;
          }
        }
      }

      // Scale the accumulated shadow factor to [0, 1]
      const float Norm = pow(2.0 * ShadowFilterSize + 1, 2.0);
      float shadowFactor = float(accum) / Norm;
      // Apply shadow to diffuse lighting (with ambient contribution)
      vec3 ambientLightColor = ambientIntensity * lightColor * diffuseAlbedo.rgb;
      totalLightColor *= ambientLightColor + (1.0 - ambientLightColor) * shadowFactor;
      // Apply shadow to specular lighting (more aggressive - specular highlights should
      // be sharply attenuated in shadows)
      totalSpecularColor *= shadowFactor;

      frag.color.rgb = totalLightColor + totalSpecularColor;
    }
  }
  else {
    frag.color.rgb = diffuseAlbedo.rgb;
  }

  frag.color.a = diffuseAlbedo.a * opacity;

  if (has_override_color) {
    frag.color = override_color;
  }

  return frag;
}
