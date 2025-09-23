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

#include "floatoperations.glsl"

in vec3 in_position;
in vec2 in_brightness;
in vec3 in_velocity;

out vec2 vs_brightness;
out vec4 vs_gPosition;
out float vs_starDistFromSun;
out float vs_cameraDistFromSun;

uniform dmat4 model;
uniform dmat4 view;
uniform dmat4 projection;
uniform float time;
uniform int renderOption;
uniform vec2 posXThreshold;
uniform vec2 posYThreshold;
uniform vec2 posZThreshold;
uniform vec2 gMagThreshold;
uniform vec2 bpRpThreshold;
uniform vec2 distThreshold;

// Keep in sync with gaiaoptions.h:RenderOption enum
const int RENDEROPTION_STATIC = 0;
const int RENDEROPTION_COLOR = 1;
const int RENDEROPTION_MOTION = 2;
const float EPS = 1e-5;
const float Parsec = 3.0856776e16;


void main() {
  vs_brightness = in_brightness;

  // Check if we should filter this star by position. Thres depending on original values.
  if ((abs(posXThreshold.x) > EPS && in_position.x < posXThreshold.x) ||
      (abs(posXThreshold.y) > EPS && in_position.x > posXThreshold.y) ||
      (abs(posYThreshold.x) > EPS && in_position.y < posYThreshold.x) ||
      (abs(posYThreshold.y) > EPS && in_position.y > posYThreshold.y) ||
      (abs(posZThreshold.x) > EPS && in_position.z < posZThreshold.x) ||
      (abs(posZThreshold.y) > EPS && in_position.z > posZThreshold.y) ||
      (abs(distThreshold.x - distThreshold.y) < EPS
      && abs(length(in_position) - distThreshold.y) < EPS) ||
      (renderOption != RENDEROPTION_STATIC && (
      (abs(gMagThreshold.x - gMagThreshold.y) < EPS && abs(gMagThreshold.x - in_brightness.x) < EPS) ||
      (abs(gMagThreshold.x - 20.0) > EPS && in_brightness.x < gMagThreshold.x) ||
      (abs(gMagThreshold.y - 20.0) > EPS && in_brightness.x > gMagThreshold.y) ||
      (abs(bpRpThreshold.x - bpRpThreshold.y) < EPS && abs(bpRpThreshold.x - in_brightness.y) < EPS) ||
      (abs(bpRpThreshold.x) > EPS && in_brightness.y < bpRpThreshold.x) ||
      (abs(bpRpThreshold.y) > EPS && in_brightness.y > bpRpThreshold.y))))
  {
    // Discard star in geometry shader.
    vs_gPosition = vec4(0.0);
    gl_Position = vec4(0.0);
    return;
  }

  // Convert kiloParsec to meter.
  vec4 objectPosition = vec4(in_position * 1000 * Parsec, 1.0);

  // Add velocity if we've read any.
  if (renderOption == RENDEROPTION_MOTION) {
    // Velocity is already in [m/s].
    objectPosition.xyz += time * in_velocity;
  }

  // Thres moving stars by their new position.
  float distPosition = length(objectPosition.xyz / (1000.0 * Parsec));
  if ((abs(distThreshold.x - distThreshold.y) > EPS &&
      ((abs(distThreshold.x) > EPS && distPosition< distThreshold.x) ||
      (abs(distThreshold.y) > EPS && distPosition > distThreshold.y))))
  {
    // Discard star in geometry shader.
    vs_gPosition = vec4(0.0);
    gl_Position = vec4(0.0);
    return;
  }

  // Apply camera transforms.
  dvec4 viewPosition = view * model * objectPosition;
  vec4 sunPosition = vec4(view * model * vec4(0.0, 0.0, 0.0, 1.0));

  vs_starDistFromSun = safeLength(objectPosition);
  vs_cameraDistFromSun = safeLength(sunPosition);

  // Remove stars without position, happens when VBO chunk is stuffed with zeros.
  // Has to be done in Geometry shader because Vertices cannot be discarded here.
  if (length(in_position) > EPS) {
    vs_gPosition = vec4(model * objectPosition);
    gl_Position = vec4(projection * viewPosition);
  }
  else {
    vs_gPosition = vec4(0.0);
    gl_Position = vec4(0.0);
  }
}
