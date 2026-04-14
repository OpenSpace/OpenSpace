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

#version __CONTEXT__

const float SpecularExponent = 100.0;
const vec3 EnvironmentalRadiance = vec3(5.0);

in Data {
  vec2 texCoords;
} in_data;

out vec4 out_color;

uniform sampler2D texDepth;
uniform sampler2D texColor;
uniform sampler2D texNormal;
uniform mat4 invProjMat;
uniform vec3 lightDir;
uniform vec3 lightCol;


// @TODO (2026-02-11, abock): Use linear depth instead and use uniform vec4 for unpacking
// to view coords.

vec4 depthToViewCoord(vec2 texCoord, float depth) {
  vec4 clipCoord = vec4(vec3(texCoord, depth) * 2.0 - 1.0, 1.0);
  vec4 viewCoord = invProjMat * clipCoord;
  return viewCoord / viewCoord.w;
}

float fresnel(float HdotV) {
  const float N1 = 1.0;
  const float N2 = 1.5;
  const float R0 = pow((N1 - N2) / (N1 + N2), 2.0);
  return R0 + (1.0 - R0) * pow(1.0 - HdotV, 5.0);
}

// https://aras-p.info/texts/CompactNormalStorage.html
vec3 decodeNormal(vec2 enc) {
  vec2 fenc = enc * 4.0 - 2.0;
  float f = dot(fenc, fenc);
  float g = sqrt(1.0 - f / 4.0);
  return vec3(fenc * g, 1 - f / 2.0);
}

vec3 lambert(vec3 radiance) {
  const float OneOverPi = 1.0 / 3.1415926535;
  return radiance * OneOverPi;
}

vec3 shade(vec3 color, vec3 V, vec3 N) {
  float NdotL = clamp(dot(N, lightDir), 0.0, 1.0);
  vec3 diffuse = color.rgb * lambert(EnvironmentalRadiance + NdotL * lightCol);

  vec3 H = normalize(lightDir + V);
  float HdotV = clamp(dot(H, V), 0.0, 1.0);
  float fr = fresnel(HdotV);
  float NdotH = clamp(dot(N, H), 0.0, 1.0);
  vec3 specular = fr * (EnvironmentalRadiance + lightCol) * pow(NdotH, SpecularExponent);

  return diffuse + specular;
}

void main() {
  float depth = texelFetch(texDepth, ivec2(gl_FragCoord.xy), 0).x;
  if (depth == 1.0) {
    out_color = vec4(0.0);
    return;
  }
  vec4 c = texelFetch(texColor, ivec2(gl_FragCoord.xy), 0);
  vec3 normal = decodeNormal(texelFetch(texNormal, ivec2(gl_FragCoord.xy), 0).xy);
  vec4 viewCoord = depthToViewCoord(in_data.texCoords, depth);

  vec3 N = normal;
  vec3 V = -normalize(viewCoord.xyz);
  vec3 result = shade(c.rgb, V, N);

  out_color = vec4(result, c.a);
}
