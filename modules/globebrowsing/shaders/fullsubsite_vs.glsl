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

// Uniforms
uniform mat4 modelViewTransform;
uniform mat4 projectionTransform;

//uniform vec4 cameraCenter;
//uniform vec4 cameraAxis;
//uniform vec4 cameraHorizontal;
//uniform vec4 cameraVector;

//uniform vec4 cameraCenter2;
//uniform vec4 cameraAxis2;
//uniform vec4 cameraHorizontal2;
//uniform vec4 cameraVector2;

//uniform vec4 cameraCenter3;
//uniform vec4 cameraAxis3;
//uniform vec4 cameraHorizontal3;
//uniform vec4 cameraVector3;

uniform int size;


uniform vec3 cameraDirectionWorldSpace;

uniform float _magnification;

uniform bool useUVCoord;

// Outputs
out vec3 vs_normalViewSpace;
out vec4 vs_positionScreenSpace;
out vec4 vs_positionCameraSpace;
out vec4 vs_color;

out vec2 vs_stDone[20];
out int vs_size;

#include "PowerScaling/powerScaling_vs.hglsl"

void main () {
  int colsize = 1024;
  vec2 textureSize = vec2(colsize,colsize);
  int nrOfIterations = size;

  for (int i = 0; i < nrOfIterations; i++) {
    vec4 pointToCamera = in_position - vec4(camerasCenters[i], 1);

    double floor = dot(pointToCamera, vec4(camerasAxes[i], 1));

    double xRoof = dot(pointToCamera, vec4(camerasHorizontals[i], 1));

    double yRoof = dot(pointToCamera, vec4(camerasVectors[i], 1));

    double x = xRoof / floor;
    double y = yRoof / floor;

    vs_stDone[i] = vec2((1.0 / textureSize.x) * x, 1.0 - (1.0 / textureSize.y) * y);
  }

  vs_size = size;

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
