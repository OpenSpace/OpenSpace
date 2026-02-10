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
  vec3 position;
  flat vec3 velocity;
  vec2 texCoords;
  flat float bv;
  flat float speed;
  flat float otherData;
  flat float screenSpaceDepth;
} in_data;

// layout(bindless_sampler) uniform sampler1D colorTexture;
uniform sampler1D colorTexture;
// layout(bindless_sampler) uniform sampler2D glareTexture;
uniform sampler2D glareTexture;
uniform float opacity;
uniform vec3 fixedColor;
uniform int colorOption;
// layout(bindless_sampler) uniform sampler1D otherDataTexture;
uniform sampler1D otherDataTexture;
uniform vec2 otherDataRange;
uniform bool filterOutOfRange;

uniform float glareMultiplier;
uniform float glareGamma;
uniform float glareScale;

uniform bool hasCore;
// layout(bindless_sampler) uniform sampler2D coreTexture;
uniform sampler2D coreTexture;
uniform float coreMultiplier;
uniform float coreGamma;
uniform float coreScale;

// keep in sync with renderablestars.h:ColorOption enum
const int ColorOptionColor = 0;
const int ColorOptionVelocity = 1;
const int ColorOptionSpeed = 2;
const int ColorOptionOtherData = 3;
const int ColorOptionFixedColor = 4;


vec4 bv2rgb(float bv) {
  // BV is [-0.4,2.0]
  float t = (bv + 0.4) / (2.0 + 0.4);
  t = clamp(t, 0.0, 1.0);
  return texture(colorTexture, t);
}

bool isOtherDataValueInRange() {
  float t =
    (in_data.otherData - otherDataRange.x) / (otherDataRange.y - otherDataRange.x);
  return t >= 0.0 && t <= 1.0;
}

vec4 otherDataValue() {
  float t =
    (in_data.otherData - otherDataRange.x) / (otherDataRange.y - otherDataRange.x);
  t = clamp(t, 0.0, 1.0);
  return texture(otherDataTexture, t);
}


Fragment getFragment() {
  vec4 color = vec4(0.0);

  switch (colorOption) {
    case ColorOptionColor:
      color = bv2rgb(in_data.bv);
      break;
    case ColorOptionVelocity:
      color = vec4(abs(in_data.velocity), 0.5);
      break;
    case ColorOptionSpeed:
      // @TODO Include a transfer function here ---abock
      color = vec4(vec3(in_data.speed), 0.5);
      break;
    case ColorOptionOtherData:
      if (filterOutOfRange && !isOtherDataValueInRange()) {
        discard;
      }
      else {
        color = otherDataValue();
      }
      break;
    case ColorOptionFixedColor:
      color = vec4(fixedColor, 1.0);
      break;
  }

  vec2 shiftedCoords = (in_data.texCoords - 0.5) * 2;

  vec2 scaledCoordsGlare = shiftedCoords / glareScale;
  vec2 unshiftedCoordsGlare = (scaledCoordsGlare + 1.0) / 2.0;
  float glareValue = texture(glareTexture, unshiftedCoordsGlare).a;
  float alpha = pow(glareValue, glareGamma) * glareMultiplier;
  if (hasCore) {
    vec2 scaledCoordsCore = shiftedCoords / coreScale;
    vec2 unshiftedCoordsCore = (scaledCoordsCore + 1.0) / 2.0;
    float coreValue = texture(coreTexture, unshiftedCoordsCore).a;
    float core = pow(coreValue, coreGamma) * coreMultiplier;
    alpha += core;
  }

  vec4 fullColor = vec4(color.rgb, alpha * opacity);

  if (fullColor.a < 0.001) {
    discard;
  }

  Fragment frag;
  frag.color = fullColor;
  frag.depth = in_data.screenSpaceDepth;
  frag.gPosition = vec4(in_data.position, 1.0);
  frag.gNormal = vec4(0.0, 0.0, 0.0, 1.0);
  frag.disableLDR2HDR = true;

  return frag;
}
