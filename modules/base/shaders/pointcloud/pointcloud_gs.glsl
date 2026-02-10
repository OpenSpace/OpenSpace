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

layout(points) in;
in Data {
  flat vec4 orientation; // quaternion
  flat float textureLayer;
  flat float colorParameter;
  flat float scalingParameter;
} in_data[];

layout(triangle_strip, max_vertices = 4) out;
out Data {
  flat vec4 positionViewSpace;
  vec2 texCoord;
  flat float colorParameter;
  flat float screenSpaceDepth;
  flat int layer;
} out_data;

// General settings
uniform float scaleExponent;
uniform float scaleFactor;
uniform int renderOption;
uniform dmat4 cameraViewMatrix;
uniform dmat4 projectionMatrix;
uniform dmat4 modelMatrix;
uniform bool enableMaxSizeControl;
uniform bool hasDvarScaling;
uniform float dvarScaleFactor;
uniform bool useOrientationData;

// RenderOption: CameraViewDirection
uniform vec3 up;
uniform vec3 right;

// RenderOption: CameraPositionNormal
uniform dvec3 cameraPosition;
uniform vec3 cameraLookUp;

// Max size control: true
// The max size is an angle, in degrees, for the diameter
uniform float maxAngularSize;

uniform vec2 aspectRatioScale;

const vec2 corners[4] = vec2[4](
  vec2(0.0, 0.0),
  vec2(1.0, 0.0),
  vec2(1.0, 1.0),
  vec2(0.0, 1.0)
);

const int RenderOptionCameraViewDirection = 0;
const int RenderOptionCameraPositionNormal = 1;
const int RenderOptionFixedRotation = 2;


// Quaternion math code from:
// https://gist.github.com/mattatz/40a91588d5fb38240403f198a938a593
vec4 quatMult(vec4 q1, vec4 q2) {
  return vec4(
    q2.xyz * q1.w + q1.xyz * q2.w + cross(q1.xyz, q2.xyz),
    q1.w * q2.w - dot(q1.xyz, q2.xyz)
  );
}

// Vector rotation with a quaternion
// http://mathworld.wolfram.com/Quaternion.html
vec3 rotate_vector(vec3 v, vec4 q) {
  vec4 q_conjugate = q * vec4(-1.0, -1.0, -1.0, 1.0);
  return quatMult(q, quatMult(vec4(v, 0.0), q_conjugate)).xyz;
}


void main() {
  vec4 pos = gl_in[0].gl_Position;
  out_data.layer = int(in_data[0].textureLayer);
  out_data.colorParameter = in_data[0].colorParameter;

  dvec4 dpos = modelMatrix * dvec4(dvec3(pos.xyz), 1.0);

  float scaleMultiply = pow(10.0, scaleExponent);
  if (hasDvarScaling) {
    scaleMultiply *= in_data[0].scalingParameter * dvarScaleFactor;
  }

  vec3 scaledRight = vec3(1.0, 0.0, 0.0);
  vec3 scaledUp = vec3(0.0, 1.0, 0.0);

  if (renderOption == RenderOptionCameraViewDirection) {
    scaledRight = right;
    scaledUp = up;
  }
  else if (renderOption == RenderOptionCameraPositionNormal) {
    vec3 normal = vec3(normalize(cameraPosition - dpos.xyz));
    vec3 newRight = normalize(cross(cameraLookUp, normal));
    vec3 newUp = cross(normal, newRight);

    scaledRight = newRight;
    scaledUp = newUp;
  }
  else if (renderOption == RenderOptionFixedRotation) {
    if (useOrientationData) {
      vec4 quat = in_data[0].orientation;
      scaledRight = normalize(rotate_vector(scaledRight, quat));
      scaledUp = normalize(rotate_vector(scaledUp, quat));
    }
    // Else use default
  }

  scaledRight *= scaleMultiply * 0.5;
  scaledUp *= scaleMultiply * 0.5;

  if (enableMaxSizeControl) {
    // Limit the max size of the points, as the angle in "FOV" that the point is allowed
    // to take up. Note that the max size is for the diameter, and we need the radius
    float desiredAngleRadians = radians(maxAngularSize * 0.5);

    double distanceToCamera = length(dpos.xyz - cameraPosition);
    double pointSize = length(dvec3(scaledRight));
    // @TODO (2023-01-05, emmbr) Consider if this atan computation can be optimized using
    // approximation
    float angle = atan(float(pointSize / distanceToCamera));

    if ((angle > desiredAngleRadians) && (distanceToCamera > 0.0)) {
      float correctionScaleFactor =
        float(distanceToCamera) * tan(desiredAngleRadians) / float(pointSize);
      scaledRight *= correctionScaleFactor;
      scaledUp *= correctionScaleFactor;
    }
  }

  dmat4 cameraViewProjectionMatrix = projectionMatrix * cameraViewMatrix;

  vec4 dposClip = vec4(cameraViewProjectionMatrix * dpos);
  vec4 scaledRightClip = scaleFactor * aspectRatioScale.x *
    vec4(cameraViewProjectionMatrix * dvec4(scaledRight, 0.0));
  vec4 scaledUpClip = scaleFactor * aspectRatioScale.y *
    vec4(cameraViewProjectionMatrix * dvec4(scaledUp, 0.0));

  vec4 dposViewSpace= vec4(cameraViewMatrix * dpos);
  out_data.positionViewSpace = dposViewSpace;

  vec4 initialPosition = z_normalization(dposClip - scaledRightClip - scaledUpClip);
  out_data.screenSpaceDepth = initialPosition.w;
  vec4 secondPosition = z_normalization(dposClip + scaledRightClip - scaledUpClip);
  vec4 crossCorner = z_normalization(dposClip + scaledUpClip + scaledRightClip);
  vec4 thirdPosition = z_normalization(dposClip + scaledUpClip - scaledRightClip);

  // Build primitive
  out_data.texCoord = corners[0];
  gl_Position = initialPosition;
  EmitVertex();

  out_data.texCoord = corners[1];
  gl_Position = secondPosition;
  EmitVertex();

  out_data.texCoord = corners[3];
  gl_Position = thirdPosition;
  EmitVertex();

  out_data.texCoord = corners[2];
  gl_Position = crossCorner;
  EmitVertex();

  EndPrimitive();
}
