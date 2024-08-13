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

#version __CONTEXT__

#include "PowerScaling/powerScaling_vs.hglsl"

in vec3 in_position0;
in vec3 in_position1;

// Only used if spline interpolation is desired
in vec3 in_position_before;
in vec3 in_position_after;

in float in_colorParameter0;
in float in_colorParameter1;
in float in_scalingParameter0;
in float in_scalingParameter1;

in float in_textureLayer;

in vec4 in_orientation0; // quaternion
in vec4 in_orientation1; // quaternion

uniform bool useSpline;
uniform float interpolationValue;

flat out float textureLayer;
flat out float colorParameter;
flat out float scalingParameter;

flat out vec4 orientation; // quaternion

float interpolateDataValue(float v0, float v1, float t) {
  const float Epsilon = 1E-7;
  const float NaN = log(-1.0); // undefined
  // To make sure we render values at knots with neighboring missing values,
  // check 0 and 1 expicitly
  if (abs(t) < Epsilon) {
      return v0;
  }
  if (abs(1.0 - t) < Epsilon) {
      return v1;
  }
  bool isMissing = isnan(v0) || isnan(v1);
  return isMissing ? NaN : mix(v0, v1, t);
}

vec3 interpolateCatmullRom(float t, vec3 p0, vec3 p1, vec3 p2, vec3 p3) {
    float t2 = t * t;
    float t3 = t2 * t;
    return 0.5 * (
        2.0 * p1 +
        t * (p2 - p0) +
        t2 * (2.0 * p0 - 5.0 * p1 + 4.0 * p2 - p3) +
        t3 * (3.0 * p1 - p0  - 3.0 * p2 + p3)
    );
}

// Quaternion math from: https://gist.github.com/mattatz/40a91588d5fb38240403f198a938a593
vec4 quaternionSlerp(vec4 a, vec4 b, float t) {
    // if either input is zero, return the other.
    if (length(a) == 0.0) {
        if (length(b) == 0.0) {
            return vec4(0.0, 0.0, 0.0, 1.0);
        }
        return b;
    }
    else if (length(b) == 0.0) {
        return a;
    }

    float cosHalfAngle = a.w * b.w + dot(a.xyz, b.xyz);

    if (cosHalfAngle >= 1.0 || cosHalfAngle <= -1.0) {
        return a;
    }
    else if (cosHalfAngle < 0.0) {
        b.xyz = -b.xyz;
        b.w = -b.w;
        cosHalfAngle = -cosHalfAngle;
    }

    float blendA;
    float blendB;
    if (cosHalfAngle < 0.99) {
        // Do proper slerp for big angles
        float halfAngle = acos(cosHalfAngle);
        float sinHalfAngle = sin(halfAngle);
        float oneOverSinHalfAngle = 1.0 / sinHalfAngle;
        blendA = sin(halfAngle * (1.0 - t)) * oneOverSinHalfAngle;
        blendB = sin(halfAngle * t) * oneOverSinHalfAngle;
    }
    else {
        // Do lerp if angle is really small
        blendA = 1.0 - t;
        blendB = t;
    }

    vec4 result = vec4(blendA * a.xyz + blendB * b.xyz, blendA * a.w + blendB * b.w);
    if (length(result) > 0.0) {
        return normalize(result);
    }
    return vec4(0.0, 0.0, 0.0, 1.0);
}

void main() {
  float t = interpolationValue;

  colorParameter = interpolateDataValue(in_colorParameter0, in_colorParameter1, t);
  scalingParameter = interpolateDataValue(in_scalingParameter0, in_scalingParameter1, t);

  vec3 position = mix(in_position0, in_position1, t);
  if (useSpline) {
    position = interpolateCatmullRom(
      t,
      in_position_before,
      in_position0,
      in_position1,
      in_position_after
    );
  }

  orientation = quaternionSlerp(in_orientation0, in_orientation1, t);

  textureLayer = in_textureLayer;

  gl_Position = vec4(position, 1.0);
}
