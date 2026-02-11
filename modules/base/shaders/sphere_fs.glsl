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
  vec4 position;
  vec2 textureCoords;
  vec3 normal;
  float screenSpaceDepth;
} in_data;

uniform sampler2D colorTexture;

uniform bool usingTransferFunction = false;
uniform sampler1D transferFunction;
uniform vec2 dataMinMaxValues;

uniform float opacity;
uniform bool mirrorTexture;

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
  vec3 pos3 = vec3(cos(lat) * cos(lon), sin(lat), cos(lat) * sin(lon));

  // 3D vector to normalized 2D fisheye [-1,1]
  float r = 2.0 / M_PI * atan(sqrt(dot(pos3.xz, pos3.xz)), pos3.y);
  if (r > 1.0) {
    // Invalid coordinates (outside fisheye frame)
     discard;
  }

  float theta = atan(pos3.z, pos3.x);
  vec2 fisheye2D = vec2(r * cos(theta), r * sin(theta));

  // Remap to [0,1]
  return 0.5 * fisheye2D + 0.5;
}

Fragment getFragment() {
  // Equirectangular
  vec2 texCoord = in_data.textureCoords;

  if (textureProjection == AngularFisheye) {
    texCoord = equiToAngularFisheye(in_data.textureCoords);
  }

  Fragment frag;
  if (mirrorTexture) {
    texCoord.x = 1.0 - texCoord.x;
  }
  if (usingTransferFunction) {
    vec4 dataValue = texture(colorTexture, texCoord);
    float minVal = dataMinMaxValues.x;
    float maxVal = dataMinMaxValues.y;
    // dataValue and minVal comes from the same texture so dataValue cannot be < minVal
    float lookUpVal = (dataValue.x - minVal) / (maxVal - minVal);
    frag.color = vec4(texture(transferFunction, lookUpVal).rgb, 1.0);
  }
  else {
    frag.color = texture(colorTexture, texCoord);
  }
  frag.color.a *= opacity;
  frag.depth = in_data.screenSpaceDepth;
  frag.gPosition = in_data.position;
  frag.gNormal = vec4(in_data.normal, 1.0);
  return frag;
}
