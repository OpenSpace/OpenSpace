/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

// Vertex attributes
layout(location = 0) in vec4 in_position;
layout(location = 1) in vec2 in_st;
layout(location = 2) in vec3 in_normal;

uniform vec3 camerasCenters[20];
uniform vec3 camerasAxes[20];
uniform vec3 camerasHorizontals[20];
uniform vec3 camerasVectors[20];

uniform vec4 camerasColoredCenters[20];
uniform vec4 camerasColoredAxes[20];
uniform vec4 camerasColoredHorizontals[20];
uniform vec4 camerasColoredVectors[20];

uniform bool useMastCamColor = false;

// Uniforms
uniform mat4 modelViewTransform;
uniform mat4 projectionTransform;

uniform int size;
uniform int colorSize;

uniform float _magnification;

// Outputs
out vec3 vs_normalViewSpace;
out vec4 vs_positionScreenSpace;
out vec4 vs_positionCameraSpace;

out vec2 vs_stDone[22];
flat out int textureIndex;
out vec2 vs_st_color[7];
flat out int coloredTextureIndexOut;

#include "PowerScaling/powerScaling_vs.hglsl"

void main () {
  int colsize = 1024;
  vec2 textureSize = vec2(colsize,colsize);
  int nrOfIterations = size;
  vec2 center = vec2(0.5,0.5);
  float maxDist = 100;
  int bestIndex = -1;

  for (int i = 0; i < nrOfIterations; i++) {
    vec4 pointToCamera = in_position - vec4(camerasCenters[i], 1);

    double floor = dot(pointToCamera, vec4(camerasAxes[i], 0));

    if (floor < 0) {
      // This means that the point is behind the camera/imageplane
      continue;
    }

    double xRoof = dot(pointToCamera, vec4(camerasHorizontals[i], 1));

    double yRoof = dot(pointToCamera, vec4(camerasVectors[i], 1));

    double x = xRoof / floor;
    double y = yRoof / floor;

    vec2 tempUV = vec2((1.0 / textureSize.x) * x, 1.0 - (1.0 / textureSize.y) * y);

    vec2 tempp = tempUV - center;
    float dist = length(tempp);
    if (dist < maxDist) {
      maxDist = dist;
      bestIndex = i;
    }
    vs_stDone[i] = tempUV;
  }

  textureIndex = bestIndex;

  //////////////////////////////////////COLORED
  int coloredTextureIndex = -1;

  if (useMastCamColor) {
    maxDist = 1000;
    int colsize2 = 1200;
    int linesize2 = 1344;
    vec2 textureSize2 = vec2(linesize2,colsize2);

    int nrOfColorIterations = colorSize;
    for (int i = 0; i < nrOfColorIterations; ++i) {
      vec4 pointToCameraColored = in_position - camerasColoredCenters[i];

      double floor2 = dot(pointToCameraColored, camerasColoredAxes[i]);

      if (floor2 < 0) {
        continue;
      }

      double xRoof2 = dot(pointToCameraColored, camerasColoredHorizontals[i]);


      double yRoof2 = dot(pointToCameraColored, camerasColoredVectors[i]);

      double x2 = xRoof2 / floor2;
      double y2 = yRoof2 / floor2;

      vec2 tempUV2 = vec2((1.0 / textureSize2.x) * x2, 1.0 - (1.0 / textureSize2.y) * y2);

      vec2 tempp = tempUV2 - center;
      float dist = length(tempp);
      if (dist < maxDist) {
        maxDist = dist;
        coloredTextureIndex = i;
      }
      vs_st_color[i] = tempUV2;
    }
  }

  coloredTextureIndexOut = coloredTextureIndex;
  /////////////////////////////////////////////

  vec4 position = in_position;
  position.z = position.z - 0.1;
  position.xyz *= pow(10, _magnification);
  vs_positionCameraSpace = modelViewTransform * position;
  vec4 positionClipSpace = projectionTransform * vs_positionCameraSpace;

  vs_positionScreenSpace = z_normalization(positionClipSpace);
  gl_Position = vs_positionScreenSpace;

  // The normal transform should be the transposed inverse of the model transform?
  vs_normalViewSpace = normalize(mat3(modelViewTransform) * in_normal);
}
