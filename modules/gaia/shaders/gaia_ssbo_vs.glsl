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

layout (std430) buffer ssbo_idx_data {
  int starsPerChunk[];
};

layout (std430) buffer ssbo_comb_data {
  float allData[];
};

out Data {
  vec4 gPosition;
  vec2 brightness;
  float starDistFromSun;
  float cameraDistFromSun;
} out_data;

uniform dmat4 model;
uniform dmat4 view;
uniform dmat4 projection;
uniform float time;
uniform int renderOption;
uniform int maxStarsPerNode;
uniform int valuesPerStar;
uniform int nChunksToRender;
uniform vec2 posXThreshold;
uniform vec2 posYThreshold;
uniform vec2 posZThreshold;
uniform vec2 gMagThreshold;
uniform vec2 bpRpThreshold;
uniform vec2 distThreshold;

// Keep in sync with gaiaoptions.h:RenderOption enum
const int RenderOptionStatic = 0;
const int RenderOptionColor = 1;
const int RenderOptionMotion = 2;
const float Eps = 1e-5;
const float Parsec = 3.0856776e16;


// Use binary search to find the chunk containing our star ID.
int findChunkId(int left, int right, int id) {
  while (left <= right) {
    int middle = (left + right) / 2;
    int firstStarInChunk = starsPerChunk[middle];
    if (left == right || (firstStarInChunk <= id && id < starsPerChunk[middle+1])) {
      return middle;
    }
    else if (id < firstStarInChunk) {
      // Go smaller
      right = middle - 1;
    }
    else {
      // Go bigger
      left = middle + 1;
    }
  }
  return -1;
}


void main() {
  // Fetch our data.
  int chunkId = findChunkId(0, nChunksToRender - 1, gl_VertexID);
  // Fail safe - this should never happen!
  if (chunkId == -1) {
    out_data.gPosition = vec4(0.0);
    gl_Position = vec4(0.0);
    return;
  }
  int placeInChunk = gl_VertexID - starsPerChunk[chunkId];
  int firstStarInChunk = valuesPerStar * maxStarsPerNode * chunkId;
  int nStarsInChunk = starsPerChunk[chunkId + 1] - starsPerChunk[chunkId];
  // Remove possible duplicates
  if (nStarsInChunk <= 0) {
    out_data.gPosition = vec4(0.0);
    gl_Position = vec4(0.0);
    return;
  }

  int startOfPos = firstStarInChunk + placeInChunk * 3;
  vec3 in_position = vec3(
    allData[startOfPos],
    allData[startOfPos + 1],
    allData[startOfPos + 2]
  );
  vec2 in_brightness = vec2(0.0);
  vec3 in_velocity = vec3(0.0);

  // Check if we should filter this star by position
  if ((abs(posXThreshold.x) > Eps && in_position.x < posXThreshold.x) ||
      (abs(posXThreshold.y) > Eps && in_position.x > posXThreshold.y) ||
      (abs(posYThreshold.x) > Eps && in_position.y < posYThreshold.x) ||
      (abs(posYThreshold.y) > Eps && in_position.y > posYThreshold.y) ||
      (abs(posZThreshold.x) > Eps && in_position.z < posZThreshold.x) ||
      (abs(posZThreshold.y) > Eps && in_position.z > posZThreshold.y) ||
      (abs(distThreshold.x - distThreshold.y) < Eps &&
      abs(length(in_position) - distThreshold.y) < Eps))
  {
    // Discard star in geometry shader
    out_data.gPosition = vec4(0.0);
    gl_Position = vec4(0.0);
    return;
  }


  if (renderOption != RenderOptionStatic) {
    int startOfCol = firstStarInChunk + nStarsInChunk * 3 + placeInChunk * 2;
    in_brightness = vec2(allData[startOfCol], allData[startOfCol + 1]);

    // Check if we should filter this star by magnitude or color
    if ((abs(gMagThreshold.x - gMagThreshold.y) < Eps &&
         abs(gMagThreshold.x - in_brightness.x) < Eps) ||
        (abs(gMagThreshold.x - 20.0) > Eps && in_brightness.x < gMagThreshold.x) ||
        (abs(gMagThreshold.y - 20.0) > Eps && in_brightness.x > gMagThreshold.y) ||
        (abs(bpRpThreshold.x - bpRpThreshold.y) < Eps &&
         abs(bpRpThreshold.x - in_brightness.y) < Eps) ||
        (abs(bpRpThreshold.x) > Eps && in_brightness.y < bpRpThreshold.x) ||
        (abs(bpRpThreshold.y) > Eps && in_brightness.y > bpRpThreshold.y))
    {
      // Discard star in geometry shader
      out_data.gPosition = vec4(0.0);
      gl_Position = vec4(0.0);
      return;
    }

    if (renderOption == RenderOptionMotion) {
      int startOfVel = firstStarInChunk + nStarsInChunk * 5 + placeInChunk * 3;
      in_velocity = vec3(
        allData[startOfVel],
        allData[startOfVel + 1],
        allData[startOfVel + 2]
      );
    }
  }
  out_data.brightness = in_brightness;

  // Convert kiloParsec to meter
  vec4 objectPosition = vec4(in_position * 1000.0 * Parsec, 1.0);

  // Add velocity [m/s] if we've read any
  objectPosition.xyz += time * in_velocity;

  // Thres moving stars by their new position
  float distPosition = length(objectPosition.xyz / (1000.0 * Parsec));
  if ((abs(distThreshold.x - distThreshold.y) > Eps &&
      ((abs(distThreshold.x) > Eps && distPosition < distThreshold.x) ||
      (abs(distThreshold.y) > Eps && distPosition > distThreshold.y))))
  {
    // Discard star in geometry shader
    out_data.gPosition = vec4(0.0);
    gl_Position = vec4(0.0);
    return;
  }

  // Apply camera transforms
  dvec4 viewPosition = view * model * objectPosition;
  vec4 sunPosition = vec4(view * model * dvec4(0.0, 0.0, 0.0, 1.0));

  out_data.starDistFromSun = safeLength(objectPosition);
  out_data.cameraDistFromSun = safeLength(sunPosition);

  // Remove stars without position, happens when VBO chunk is stuffed with zeros.
  // Has to be done in Geometry shader because Vertices cannot be discarded here.
  if (length(in_position) > Eps) {
    out_data.gPosition = vec4(model * objectPosition);
    gl_Position = vec4(projection * viewPosition);
  }
  else {
    out_data.gPosition = vec4(0.0);
    gl_Position = vec4(0.0);
  }
}
