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

#include "powerscaling/powerscalingmath.glsl"

layout(location = 0) in vec3 in_position; // in meters
layout(location = 1) in float in_fluxValue; // the extra value used to color lines
layout(location = 2) in float in_rValue; // the extra value used to mask parts of lines

out Data {
  vec4 color;
  vec2 vs_st;
  float vs_depth;
  float vs_closeToEarth;
} out_data;

// General Uniforms that's always needed
uniform mat4 modelViewProjection;

// Uniforms needed to color by quantity
uniform int colorMode;
uniform sampler1D colorTable;
uniform vec2 colorTableRange;
uniform vec2 domainLimZ;

// Fluxnodes specific uniforms
uniform float nodeSize;
uniform vec4 streamColor;
uniform float thresholdFlux;
uniform float filterLower;
uniform float filterUpper;
uniform int scalingMode;
uniform int nodeSkipMethod;
uniform int nodeSkip;
uniform int nodeSkipDefault;
uniform int nodeSkipEarth;
uniform float nodeSkipFluxThreshold;
uniform float nodeSkipRadiusThreshold;
uniform float fluxColorAlpha;
uniform vec3 earthPos;
uniform float distanceThreshold;
uniform float proximityNodesSize;
uniform double time;

// Specific uniforms for cameraperspective
uniform float maxNodeDistanceSize;

uniform vec3 cameraPos;
//uniform vec2 screenSize;
uniform bool usingCameraPerspective;
uniform float perspectiveDistanceFactor;

uniform vec2 minMaxNodeSize;
uniform bool usingPulse;

// These should correspond to the enum 'ColorMode'
const int ColorModeColorByFluxValue = 0;
const int ColorModeUniformColor = 1;

const int NodeSkipUniform = 0;
const int NodeSkipFlux = 1;
const int NodeSkipRadius = 2;

const int ScalingModeFlux = 0;
const int ScalingModeRFlux = 1;
const int ScalingModeR2Flux = 2;
const int ScalingModeLog10RFlux = 3;
const int ScalingModeLnRFlux = 4;

const float AUtoMeter = 149597871000.0;


vec4 transferFunctionColor(sampler1D inColorTable) {
  // Remap the color scalar to a [0,1] range
  float scaleValue = 0.0;
  if (scalingMode == ScalingModeFlux) {
    scaleValue = in_fluxValue;
  }
  else if (scalingMode == ScalingModeRFlux) {
    scaleValue = in_rValue * in_fluxValue;
  }
  else if (scalingMode == ScalingModeLog10RFlux) {
    // conversion from logbase e to log10 since glsl does not support log10.
    float logToTen = log(in_rValue) / log(10.0);
    scaleValue = logToTen * in_fluxValue;
  }
  else if (scalingMode == ScalingModeLnRFlux) {
    scaleValue = log(in_rValue) * in_fluxValue;
  }
  else if (scalingMode == ScalingModeR2Flux) {
    scaleValue = in_rValue * in_rValue * in_fluxValue;
  }
  float val = (scaleValue - colorTableRange.x) / (colorTableRange.y - colorTableRange.x);
  return texture(inColorTable, val);
}

bool checkIfSkipVertex() {
  int nodeIndex = gl_VertexID;

  if (nodeSkipMethod == NodeSkipUniform) {
    if (mod(nodeIndex, nodeSkip) == 0) {
      return true;
    }
  }
  else if (nodeSkipMethod == NodeSkipFlux) {
    if (in_fluxValue > nodeSkipFluxThreshold && mod(nodeIndex, nodeSkip) == 0) {
      return true;
    }
    if (in_fluxValue < nodeSkipFluxThreshold && mod(nodeIndex, nodeSkipDefault) == 0) {
      return true;
    }
  }
  else if (nodeSkipMethod == NodeSkipRadius) {
    if (in_rValue < nodeSkipRadiusThreshold && mod(nodeIndex, nodeSkip) == 0) {
      return true;
    }
    if (in_rValue > nodeSkipRadiusThreshold && mod(nodeIndex, nodeSkipDefault) == 0) {
      return true;
    }
  }
  return false;
}

void setEarthProximitySettings() {
  float distancevec = distance(earthPos, in_position);
  out_data.closeToEarth = 0.0;

  if (distancevec < AUtoMeter * distanceThreshold) {
    if (usingPulse) {
      const int Speed = 2;
      int modulusResult = int(Speed * time) % 2;
      if (in_fluxValue > thresholdFlux) {
        out_data.color.a = modulusResult == 1  ?  0.01  :  1.0;
      }
      else {
        out_data.color.a = 0.0;
      }
    }
    out_data.closeToEarth = 1.0;
    if (mod(gl_VertexID, nodeSkipEarth) == 0) {
      gl_PointSize = proximityNodesSize;
    }
    else {
      out_data.color = vec4(0.0);
    }
  }
}


void main() {
  // Default gl_PointSize if it is not set anywhere else.
  gl_PointSize = 2;

  // Checking if we should render the vertex dependent on the vertexindex,
  // by using modulus.
  if ((checkIfSkipVertex() ||
      distance(earthPos, in_position) < (distanceThreshold * AUtoMeter)) &&
      filterLower < in_rValue / AUtoMeter && filterUpper > in_rValue / AUtoMeter &&
      in_position.z > (domainLimZ.x * AUtoMeter) &&
      in_position.z < (domainLimZ.y * AUtoMeter))
  {
    out_data.color = transferFunctionColor(colorTable);
    if (colorMode == ColorModeColorByFluxValue) {
      if (in_fluxValue > thresholdFlux) {
        out_data.color = transferFunctionColor(colorTable);
        out_data.color.a = fluxColorAlpha;
        gl_PointSize = nodeSize;
      }
      else {
        out_data.color.a = 0.0;
      }
    }
    else if (colorMode == ColorModeUniformColor) {
      out_data.color = streamColor;
    }
    setEarthProximitySettings();
  }
  else {
    out_data.color = vec4(0.0);
  }

  if (usingCameraPerspective) {
    float distanceVec = distance(cameraPos, in_position);
    float maxDistance = 100000000000.0 * perspectiveDistanceFactor;

    if (distanceVec < maxDistance) {
      float distScale = 1.0 - smoothstep(0.0, maxDistance, distanceVec);
      float factorS = pow(distScale, 9.0) * 500.0;
      if (distance(earthPos, in_position) < (distanceThreshold * AUtoMeter)) {
        gl_PointSize = proximityNodesSize;
      }
      else {
        gl_PointSize = factorS * maxNodeDistanceSize;
      }
    }

    if (gl_PointSize > minMaxNodeSize.y) {
      gl_PointSize = minMaxNodeSize.y;
    }

    if (gl_PointSize < minMaxNodeSize.x) {
      gl_PointSize = minMaxNodeSize.x;
    }
  }

  vec4 positionClipSpace = modelViewProjection * vec4(in_position, 1.0);
  gl_Position = vec4(positionClipSpace.xy, 0.0, positionClipSpace.w);
  out_data.depth = gl_Position.w;
}
