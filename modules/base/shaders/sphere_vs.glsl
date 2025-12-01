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

#version __CONTEXT__

layout(location = 0) in vec4 in_position;
layout(location = 1) in vec2 in_textureCoords;

out vec2 vs_textureCoords;
out vec4 vs_position;
out vec3 vs_normal;
out float vs_screenSpaceDepth;

uniform mat4 modelViewProjection;
uniform mat4 modelViewTransform;
uniform mat3 modelViewRotation;

const int Equirectangular = 0;
const int AngularFisheye = 1;
uniform int textureProjection;

const float M_PI = 3.14159265358979323846;

// Remap equirectangular texture coordinates into angular fisheye
vec2 equiToAngularFisheye(vec2 textureCoords) {
  vec2 pos2 = textureCoords * 2.0 - 1.0; // Map [0,1] tex coords to [-1,1]

  // 2D equi to 3D vector
  float lat = pos2.y * 0.5 * M_PI;
  float lon = pos2.x * M_PI;

  // Map to 3D position, with Z being the north pole
  vec3 pos3 = vec3(
    cos(lat) * cos(lon),
    sin(lat),
    cos(lat) * sin(lon)
  );

  float coverAngle = M_PI; // 180 degrees

  // 3D vector to normalized 2D fisheye [-1,1]
  float r = 2.0 / coverAngle * atan(sqrt(dot(pos3.xz, pos3.xz)), pos3.y);
  float theta = atan(pos3.z, pos3.x);
  vec2 fisheye2D = vec2(r * cos(theta), r * sin(theta));

  if (r > 1.0) {
    return vec2(-1.0, -1.0); // Indicate invalid coordinates
  }

  // Remap to [0,1]
  return 0.5 * fisheye2D + 0.5;
}

void main() {
  if (textureProjection == AngularFisheye) {
    vs_textureCoords = equiToAngularFisheye(in_textureCoords);
  }
  else { // Equirectangular
    vs_textureCoords = in_textureCoords;
  }

  vs_normal = modelViewRotation * normalize(in_position.xyz);
  vec4 position = modelViewProjection * vec4(in_position.xyz, 1.0);
  vs_position = modelViewTransform * vec4(in_position.xyz, 1.0);

  // Set z to 0 to disable near/far-plane clipping
  gl_Position = vec4(position.xy, 0.0, position.w);

  vs_screenSpaceDepth = position.w;
}
