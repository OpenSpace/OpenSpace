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

#include "PowerScaling/powerScalingMath.hglsl"

layout(location = 0) in vec3 in_position; // in meters
layout(location = 1) in float fluxValue; // the extra value used to color lines
layout(location = 2) in float rValue; // the extra value used to mask out parts of lines

out vec4    vs_color;
out float   vs_depth;
out vec2    vs_st;
out float   vs_closeToEarth;

// These should correspond to the enum 'ColorMode'
const int colorByFluxValue = 0;
const int uniformColor = 1;

const int uniformskip = 0;
const int fluxSkip = 1;
const int radiusSkip = 2;

const int fluxMode = 0;
const int RFlux = 1;
const int R2Flux = 2;
const int log10RFlux = 3;
const int lnRFlux = 4;

const float AUtoMeter = 149597871000.0;

// General Uniforms that's always needed
uniform mat4      modelViewProjection;

// Uniforms needed to color by quantity
uniform int       colorMode;
uniform sampler1D colorTable;
uniform vec2      colorTableRange;
uniform vec2      domainLimZ;

// Fluxnodes specific uniforms
uniform float   nodeSize;
uniform vec4    streamColor;
uniform float   thresholdFlux;
uniform float   filterLower;
uniform float   filterUpper;
uniform int     scalingMode;
uniform int     nodeSkipMethod;
uniform int     nodeSkip;
uniform int     nodeSkipDefault;
uniform int     nodeSkipEarth;
uniform float   nodeSkipFluxThreshold;
uniform float   nodeSkipRadiusThreshold;
uniform float   fluxColorAlpha;
uniform vec3    earthPos;
uniform float   distanceThreshold;
uniform float   proximityNodesSize;
uniform double  time;

// Speicific uniforms for cameraperspective
uniform float maxNodeDistanceSize;

uniform vec3    cameraPos;
//uniform vec2 screenSize;
uniform bool    usingCameraPerspective;
uniform float   perspectiveDistanceFactor;

uniform vec2    minMaxNodeSize;
uniform bool    usingPulse;


vec4 getTransferFunctionColor(sampler1D inColorTable) {
  // Remap the color scalar to a [0,1] range
  float scaleValue = 0.0;
  if (scalingMode == fluxMode) {
    scaleValue = fluxValue;
  }
  else if (scalingMode == RFlux) {
    scaleValue = rValue * fluxValue;
  }
  else if (scalingMode == log10RFlux) {
    // conversion from logbase e to log10 since glsl does not support log10.
    float logtoTen = log(rValue) / log(10.0);
    scaleValue = logtoTen * fluxValue;
  }
  else if (scalingMode == lnRFlux) {
    scaleValue = log(rValue) * fluxValue;
  }
  else if (scalingMode == R2Flux) {
    scaleValue = rValue * rValue * fluxValue;
  }
  float lookUpVal =
    (scaleValue - colorTableRange.x) / (colorTableRange.y - colorTableRange.x);

  return texture(inColorTable, lookUpVal);
}

bool checkIfSkipVertex() {
  int nodeIndex = gl_VertexID;

  if (nodeSkipMethod == uniformskip) {
    if (mod(nodeIndex, nodeSkip) == 0) {
      return true;
    }
  }
  else if (nodeSkipMethod == fluxSkip) {
    if (fluxValue > nodeSkipFluxThreshold && mod(nodeIndex, nodeSkip) == 0) {
      return true;
    }
    if (fluxValue < nodeSkipFluxThreshold && mod(nodeIndex, nodeSkipDefault) == 0) {
      return true;
    }
  }
  else if (nodeSkipMethod == radiusSkip) {
    if (rValue < nodeSkipRadiusThreshold && mod(nodeIndex, nodeSkip) == 0) {
      return true;
    }
    if (rValue > nodeSkipRadiusThreshold && mod(nodeIndex, nodeSkipDefault) == 0) {
      return true;
    }
  }
  return false;
}

void setEarthProximitySettings() {
  float distancevec = distance(earthPos, in_position.xyz);
  vs_closeToEarth = 0.0;

  if (distancevec < AUtoMeter * distanceThreshold) {
    if (usingPulse) {
      int speed = 2;
      int modulusResult = int(speed * time) % 2;
      if (fluxValue > thresholdFlux) {
        if (modulusResult == 1) {
          vs_color.a = 0.01;
        }
        else {
          vs_color.a = 1.0;
        }
      }
      else {
        vs_color.a = 0.0;
      }
    }
    vs_closeToEarth = 1.0;
    if (mod(gl_VertexID, nodeSkipEarth) == 0) {
      gl_PointSize = proximityNodesSize;
    }
    else {
      vs_color = vec4(0.0);
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
      filterLower < rValue / AUtoMeter &&
      filterUpper > rValue / AUtoMeter &&
      in_position.z > (domainLimZ.x * AUtoMeter) &&
      in_position.z < (domainLimZ.y * AUtoMeter))
  {
    vs_color =  getTransferFunctionColor(colorTable);
    if (colorMode == colorByFluxValue) {
      if (fluxValue > thresholdFlux) {
        vs_color =  getTransferFunctionColor(colorTable);
        vs_color.a = fluxColorAlpha;
        gl_PointSize = nodeSize;
      }
      else {
        vs_color.a = 0.0;
      }
    }
    else if (colorMode == uniformColor) {
      vs_color = streamColor;
    }
    setEarthProximitySettings();
  }
  else {
    vs_color = vec4(0.0);
  }

  if (usingCameraPerspective) {
    float rtemp = min(rValue, 1.0);

    float maxDistance = 100000000000.0 * perspectiveDistanceFactor;
    float distanceVec = distance(cameraPos, in_position.xyz);

    if (distanceVec < maxDistance) {
      float distScale = 1.0 - smoothstep(0.0, maxDistance, distanceVec);
      float factorS = pow(distScale, 9.0) * 500.0;
      if (distance(earthPos, in_position.xyz) < (distanceThreshold * AUtoMeter)) {
        gl_PointSize =  proximityNodesSize;
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

  vec4 position_in_meters = vec4(in_position, 1.0);
  vec4 positionClipSpace = modelViewProjection * position_in_meters;

  gl_Position = vec4(positionClipSpace.xy, 0.0, positionClipSpace.w);
  vs_depth = gl_Position.w;
}
