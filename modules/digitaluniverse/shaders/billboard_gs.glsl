/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

layout(points) in;
flat in vec4 colorMap[];
flat in float dvarScaling[];

layout(triangle_strip, max_vertices = 4) out;
flat out vec4 gs_colorMap;
out vec2 texCoord;
flat out float vs_screenSpaceDepth;
out float ta;

// General settings
uniform float scaleFactor;
uniform int renderOption;
uniform mat4 cameraViewProjectionMatrix;
uniform dmat4 modelMatrix;
uniform bool enabledRectSizeControl;
uniform bool hasDvarScaling;

// RenderOption: CameraViewDirection
uniform vec3 up;
uniform vec3 right;

// RenderOption: CameraPositionNormal
uniform dvec3 cameraPosition;
uniform vec3 cameraLookUp;
uniform float correctionSizeFactor;
uniform float correctionSizeEndDistance;

// Pixel size control: true
uniform vec2 screenSize;
uniform float maxBillboardSize;
uniform float minBillboardSize;

const double PARSEC = 0.308567756e17LF;

const vec2 corners[4] = vec2[4](
  vec2(0.0, 0.0),
  vec2(1.0, 0.0), 
  vec2(1.0, 1.0),
  vec2(0.0, 1.0)
);

const int RenderOptionCameraViewDirection = 0;
const int RenderOptionCameraPositionNormal = 1;


void main() {
  ta = 1.0;
  vec4 pos = gl_in[0].gl_Position;
  gs_colorMap = colorMap[0];
  
  double unit = PARSEC;

  // Must be the same as the enum in RenderableBillboardsCloud.h
  if (pos.w == 1.0) { unit = 1E3; }
  else if (pos.w == 2.0) { unit = PARSEC; }
  else if (pos.w == 3.0) { unit = 1E3 * PARSEC; }
  else if (pos.w == 4.0) { unit = 1E6 * PARSEC; }
  else if (pos.w == 5.0) { unit = 1E9 * PARSEC; }
  else if (pos.w == 6.0) {
    // Conversion factor from Parsecs to GigalightYears
    unit = 306391534.73091 * PARSEC;
  }

  dvec4 dpos = modelMatrix * dvec4(dvec3(pos.xyz) * unit, 1.0);

  float scaleMultiply = exp(scaleFactor * 0.10);
  if (hasDvarScaling) {
    scaleMultiply *= dvarScaling[0];
  }
  
  vec3 scaledRight = vec3(0.0);
  vec3 scaledUp = vec3(0.0);
  
  if (renderOption == RenderOptionCameraViewDirection) {
    scaledRight = scaleMultiply * right * 0.5;
    scaledUp = scaleMultiply * up * 0.5;
  }
  else if (renderOption == RenderOptionCameraPositionNormal) {
    vec3 normal = vec3(normalize(cameraPosition - dpos.xyz));
    vec3 newRight = normalize(cross(cameraLookUp, normal));
    vec3 newUp = cross(normal, newRight);

    if (!enabledRectSizeControl) {
      double distCamera = length(cameraPosition - dpos.xyz);
      float expVar = float(-distCamera) / pow(10.0, correctionSizeEndDistance);
      float factorVar = pow(10.0, correctionSizeFactor);
      scaleMultiply *= 1.0 / (1.0 + factorVar * exp(expVar));
    }

    scaledRight = scaleMultiply * newRight * 0.5;
    scaledUp = scaleMultiply * newUp * 0.5;
  }
  
  if (enabledRectSizeControl) {
    vec4 initialPosition = z_normalization(cameraViewProjectionMatrix * 
      vec4(vec3(dpos.xyz) - scaledRight - scaledUp, dpos.w));
    
    vs_screenSpaceDepth = initialPosition.w;
    
    vec4 crossCorner = z_normalization(cameraViewProjectionMatrix * 
      vec4(vec3(dpos.xyz) + scaledUp + scaledRight, dpos.w));
    
    // Testing size for rectangular viewport:
    vec2 halfViewSize = screenSize * 0.5;
    vec2 topRight = crossCorner.xy / crossCorner.w;
    vec2 bottomLeft = initialPosition.xy / initialPosition.w;
    
    // width and height
    vec2 sizes = abs(halfViewSize * (topRight - bottomLeft));
    
    if (enabledRectSizeControl && (length(sizes) > maxBillboardSize)) {
      float correctionScale = maxBillboardSize / length(sizes);
      
      scaledRight *= correctionScale;
      scaledUp *= correctionScale;
    }
    else {
      // linear alpha decay
      if (sizes.x < 2.0 * minBillboardSize) {
        float maxVar = 2.0 * minBillboardSize;
        float minVar = minBillboardSize;
        float var = sizes.y + sizes.x;
        ta = (var - minVar) / (maxVar - minVar);
        if (ta == 0.0) {
          return;
        }
      }
    }
  }

  // Saving one matrix multiplication:
  vec4 dposClip = cameraViewProjectionMatrix * vec4(dpos);
  vec4 scaledRightClip = cameraViewProjectionMatrix * vec4(scaledRight, 0.0);
  vec4 scaledUpClip = cameraViewProjectionMatrix * vec4(scaledUp, 0.0);

  vec4 initialPosition = z_normalization(dposClip - scaledRightClip - scaledUpClip);
  vs_screenSpaceDepth = initialPosition.w;
  vec4 secondPosition = z_normalization(dposClip + scaledRightClip - scaledUpClip);
  vec4 crossCorner = z_normalization(dposClip + scaledUpClip + scaledRightClip);
  vec4 thirdPosition = z_normalization(dposClip + scaledUpClip - scaledRightClip);

  // Build primitive
  texCoord = corners[0];
  gl_Position = initialPosition;
  EmitVertex();
  
  texCoord = corners[1];
  gl_Position = secondPosition;
  EmitVertex();

  texCoord = corners[3];
  gl_Position = thirdPosition;
  EmitVertex();
      
  texCoord = corners[2];
  gl_Position = crossCorner;
  EmitVertex();
  
  EndPrimitive();
}
