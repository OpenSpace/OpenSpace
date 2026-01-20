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

in vec2 tc;
out vec4 fragColor;

uniform sampler2D u_texture_depth;
uniform sampler2D u_texture_color;
uniform sampler2D u_texture_normal;

uniform mat4 u_inv_proj_mat;
uniform vec3 u_light_dir;
uniform vec3 u_light_col;
uniform float u_time;


// TODO: Use linear depth instead and use uniform vec4 for unpacking to view coords.

vec4 depthToViewCoord(vec2 texCoord, float depth) {
  vec4 clipCoord = vec4(vec3(texCoord, depth) * 2.0 - 1.0, 1.0);
  vec4 viewCoord = u_inv_proj_mat * clipCoord;
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
  float NdotL = clamp(dot(N, u_light_dir), 0.0, 1.0);
  vec3 diffuse = color.rgb * lambert(EnvironmentalRadiance + NdotL * u_light_col);

  vec3 H = normalize(u_light_dir + V);
  float HdotV = clamp(dot(H, V), 0.0, 1.0);
  float fr = fresnel(HdotV);
  float NdotH = clamp(dot(N, H), 0.0, 1.0);
  vec3 specular =
    fr * (EnvironmentalRadiance + u_light_col) * pow(NdotH, SpecularExponent);

  return diffuse + specular;
}

void main() {
  float depth = texelFetch(u_texture_depth, ivec2(gl_FragCoord.xy), 0).x;
  if (depth == 1.0) {
    fragColor = vec4(0.0);
    return;
  }
  vec4 c = texelFetch(u_texture_color, ivec2(gl_FragCoord.xy), 0);
  vec3 normal = decodeNormal(texelFetch(u_texture_normal, ivec2(gl_FragCoord.xy), 0).xy);
  vec4 viewCoord = depthToViewCoord(tc, depth);

  vec3 N = normal;
  vec3 V = -normalize(viewCoord.xyz);
  vec3 result = shade(c.rgb, V, N);

  fragColor = vec4(result, c.a);
}
