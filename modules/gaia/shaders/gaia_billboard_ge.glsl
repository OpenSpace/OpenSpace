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

#include "floatoperations.glsl"

layout(points) in;
in Data {
  vec4 gPosition;
  vec2 brightness;
  float starDistFromSun;
  float cameraDistFromSun;
} in_data[];

layout(triangle_strip, max_vertices = 4) out;
out Data {
  vec4 gPosition;
  vec2 brightness;
  vec2 texCoord;
  float starDistFromSun;
  float cameraDistFromSun;
  float observedDist;
} out_data;

uniform dmat4 view;
uniform dmat4 projection;
uniform dvec3 cameraPos;
uniform dvec3 cameraLookUp;
uniform float viewScaling;
uniform float cutOffThreshold;
uniform float closeUpBoostDist;
uniform float billboardSize;
uniform int renderOption;
uniform float magnitudeBoost;

// Keep in sync with gaiaoptions.h:RenderOption enum
const int RenderOptionStatic = 0;
const int RenderOptionColor = 1;
const int RenderOptionMotion = 2;
const float Eps = 1e-5;

const vec2 Corners[4] = vec2[4](
  vec2(0.0, 1.0),
  vec2(0.0, 0.0),
  vec2(1.0, 1.0),
  vec2(1.0, 0.0)
);


void main() {
  out_data.brightness = in_data[0].brightness;
  out_data.starDistFromSun = in_data[0].starDistFromSun;
  out_data.cameraDistFromSun = in_data[0].cameraDistFromSun;

  vec4 viewPosition = vec4(view * in_data[0].gPosition);

  // Make closer stars look a bit bigger.
  out_data.observedDist = safeLength(viewPosition / viewScaling);
  float closeUpBoost = closeUpBoostDist / out_data.observedDist;
  float initStarSize = billboardSize;

  // Use magnitude for size boost as well.
  if (renderOption != RenderOptionStatic) {
    // DR1 magnitudes are [4, 20], but could be [-15, 20] according to this chart:
    // https://qph.fs.quoracdn.net/main-qimg-317a18e3b228efc7d7f67a1632a55961
    // Negative magnitude => Giants
    // Big positive magnitude => Dwarfs
    float absoluteMagnitude = in_data[0].brightness.x;
    float normalizedMagnitude = (absoluteMagnitude - 20.0) / -1.0; // (-15 - 20);

    // TODO: A linear scale is prabably not the best!
    initStarSize += normalizedMagnitude * (magnitudeBoost / 50.0);
  }

  vec4 position = gl_in[0].gl_Position;
  vec2 starSize = vec2(initStarSize + closeUpBoost) * position.w / 1000.0;

  float distThreshold = cutOffThreshold - log(out_data.observedDist) / log(4.0);

  // Discard geometry if star has no position (but wasn't a nullArray).
  // Or if observed distance is above threshold set by cutOffThreshold.
  // By discarding in gs instead of fs we save computations for when nothing is visible.
  if (length(position) < Eps || distThreshold <= 0) {
    return;
  }

  vec4 centerWorldPos = in_data[0].gPosition;

  dvec3 cameraNormal = normalize(cameraPos - dvec3(centerWorldPos.xyz));
  dvec3 newRight = normalize(cross(cameraLookUp, cameraNormal));
  dvec3 newUp = cross(cameraNormal, newRight);
  vec4 wCameraRight = vec4(newRight, 0.0);
  vec4 wCameraUp = vec4(newUp, 0.0);

  float multiplier = float(length(cameraPos));
  starSize *= float(multiplier / 10.0);

  for (int i = 0; i < 4; i++) {
    // Always turn the billboard towards the camera (needed for warped screen).
    vec4 cornerPoint = centerWorldPos + wCameraRight * starSize.x * (Corners[i].x - 0.5) +
      wCameraUp * starSize.y * (Corners[i].y - 0.5);
    gl_Position = vec4(projection * view * cornerPoint);
    gl_Position.z = 0.0;
    out_data.texCoord = Corners[i];
    out_data.gPosition = viewPosition;

    EmitVertex();
  }

  EndPrimitive();
}
