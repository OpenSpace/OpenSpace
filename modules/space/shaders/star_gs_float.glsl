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

// ---------------------------------------------------------------------------
// Single-precision float emulation path for Star Geometry Shader.
// Selected when NO_GPU_DOUBLE is set in the environment.
//
// Key numerical strategies:
// 1. ModelView transform is pre-composed on the CPU in double precision and split into
//    (modelViewMatrix_hi, modelViewMatrix_lo) with exact error representation.
// 2. Star positions are transformed from model space to camera view space with 48-bit
//    Dekker/Veltkamp precision via hardware FMA. This cancels the astronomical coordinate
//    offset on the CPU, eliminating catastrophic cancellation and floating-point jitter.
// 3. In view space, camera eye is at (0,0,0) and camera up is (0,1,0), allowing
//    billboard construction and distance calculation to execute purely in fast,
//    native single-precision float ALUs without any fp64 software emulation penalty.
// ---------------------------------------------------------------------------

#version __CONTEXT__

#include "powerscaling/powerscalingmath.glsl"
#include "df64.glsl"

layout(points) in;
in Data {
  vec3 bvLumAbsMag;
  vec3 velocity;
  float speed;
  float otherData;
} in_data[];

layout(triangle_strip, max_vertices = 4) out;
out Data {
  flat float bv;
  flat vec3 velocity;
  flat float speed;
  flat float otherData;
  flat float screenSpaceDepth;
  vec3 position;
  vec2 texCoords;
} out_data;

uniform float magnitudeExponent;
uniform int sizeComposition;
uniform float lumCent;
uniform float radiusCent;

// Precomputed ModelView matrix split into high and low parts (48-bit precision)
uniform mat4 modelViewMatrix_hi;
uniform mat4 modelViewMatrix_lo;

// Standard projection matrix
uniform mat4 projectionMatrix;

// Camera view scaling factor (Camera::_scaling in combinedViewMatrix)
uniform float viewScaling;

const float Parsec = 3.08567756e16;

// FRAGILE
// All of these values have to be synchronized with the values in the optionproperty
const int SizeCompositionOptionLumSizeDistanceModulus = 0;
const int SizeCompositionOptionAppBrightness = 1;
const int SizeCompositionOptionLumSize = 2;
const int SizeCompositionOptionLumSizeAbsMagnitude = 3;
const int SizeCompositionOptionLumSizeAppMagnitude = 4;

const float SunTemperature = 5800.0;
const float SunAbsMagnitude = 4.83;
const float SunRadius = 6.957E8; // meters


float bvToKelvin(float bv) {
  float tmp = 0.92 * bv;
  return 4600.0 * (1.0 / (tmp + 1.7) + 1.0 / (tmp + 0.62));
}

float scaleForApparentBrightness(float distanceToStar, float luminance) {
  // Working like Partiview: (10^(29 + magnitudeExponent) * luminance * 10) / distanceToStar
  // Rewrite using log10 to prevent 32-bit float exponent overflow when magnitudeExponent >= 9.0
  float logDist = log(max(distanceToStar, 1.0)) / log(10.0);
  float exponent = 30.0 + magnitudeExponent - logDist;
  return pow(10.0, exponent) * luminance;
}

float scaleForLuminositySize(float bv, float luminance, float absMagnitude) {
  float adjustedLum = luminance + 5e9;
  float L_over_Lsun = pow(2.51, SunAbsMagnitude - absMagnitude);
  float temperature = bvToKelvin(bv);
  float relativeTemperature = SunTemperature / temperature;
  float rStar = SunRadius * pow(relativeTemperature, 2.0) * sqrt(L_over_Lsun);
  return (lumCent * adjustedLum + (radiusCent * rStar)) * pow(10.0, magnitudeExponent);
}

float scaleForAbsoluteMagnitude(float absMagnitude) {
  return (-absMagnitude + 35.0) * pow(10.0, magnitudeExponent + 8.5);
}

float scaleForApparentMagnitude(float distanceToStar, float absMag) {
  float distanceToStarInParsecs = distanceToStar / Parsec;
  float appMag = absMag + 5.0 * (log(distanceToStarInParsecs / 10.0) / log(2.0));
  return (-appMag + 50.0) * pow(10.0, magnitudeExponent + 7.5);
}

float scaleForDistanceModulus(float absMag) {
  return exp((-30.623 - absMag) * 0.462) * pow(10.0, magnitudeExponent + 12.5) * 2000.0;
}


void main() {
  vec3 pos = gl_in[0].gl_Position.xyz;
  out_data.position = pos;

  // Transform star position from model space (parsecs) to camera view space (meters)
  // using 48-bit Dekker/Veltkamp precision. ModelViewMatrix pre-cancels the huge
  // camera position on the CPU in 64-bit precision, completely eliminating catastrophic
  // cancellation and floating-point jitter.
  df64_vec4 p_view_df = df64_mat4_mul_vec4(
    modelViewMatrix_hi,
    modelViewMatrix_lo,
    vec4(pos, 1.0)
  );
  vec3 p_view = p_view_df.hi.xyz + p_view_df.lo.xyz;

  out_data.bv = in_data[0].bvLumAbsMag.x;
  out_data.velocity = in_data[0].velocity;
  out_data.speed = in_data[0].speed;
  out_data.otherData = in_data[0].otherData;

  // In OpenSpace, Camera::combinedViewMatrix() scales coordinates by viewScaling (Camera::_scaling).
  // Thus, length(p_view) is distance_in_meters * viewScaling.
  // Dividing by viewScaling restores the true physical distance in meters.
  float distanceToStar = length(p_view) / max(viewScaling, 1e-30);
  float scaleMultiply = 1.0;

  if (sizeComposition == SizeCompositionOptionLumSizeDistanceModulus) {
    float absMagnitude = in_data[0].bvLumAbsMag.z;
    scaleMultiply = scaleForDistanceModulus(absMagnitude);
  }
  else if (sizeComposition == SizeCompositionOptionAppBrightness) {
    float luminance = in_data[0].bvLumAbsMag.y;
    scaleMultiply = scaleForApparentBrightness(distanceToStar, luminance);
  }
  else if (sizeComposition == SizeCompositionOptionLumSize) {
    float bv = in_data[0].bvLumAbsMag.x;
    float luminance = in_data[0].bvLumAbsMag.y;
    float absMagnitude = in_data[0].bvLumAbsMag.z;
    scaleMultiply = scaleForLuminositySize(bv, luminance, absMagnitude);
  }
  else if (sizeComposition == SizeCompositionOptionLumSizeAbsMagnitude) {
    float absMagnitude = in_data[0].bvLumAbsMag.z;
    scaleMultiply = scaleForAbsoluteMagnitude(absMagnitude);
  }
  else if (sizeComposition == SizeCompositionOptionLumSizeAppMagnitude) {
    float absMagnitude = in_data[0].bvLumAbsMag.z;
    scaleMultiply = scaleForApparentMagnitude(distanceToStar, absMagnitude);
  }

  // Billboard construction in view space:
  // Camera eye is at (0, 0, 0) and camera up is (0, 1, 0) in view space.
  vec3 normal = -normalize(p_view);
  vec3 right = cross(vec3(0.0, 1.0, 0.0), normal);
  float lenRight = length(right);
  vec3 newRight = lenRight > 1e-4 ? right / lenRight : vec3(1.0, 0.0, 0.0);
  vec3 newUp = cross(normal, newRight);
  // In view space, coordinates are scaled by viewScaling.
  // Multiply scaleMultiply (which is in physical meters) by viewScaling so that
  // the billboard displacement matches the coordinate scale of p_view.
  // When projected to NDC, dividing by w (which is also proportional to viewScaling)
  // cancels viewScaling completely, preserving exact screen-space size regardless of zoom!
  float scaleView = scaleMultiply * viewScaling;
  vec3 scaledRight = scaleView * newRight;
  vec3 scaledUp    = scaleView * newUp;

  // Project 4 billboard corners to clip space.
  // Using linear distributivity P * (p_view + delta) = P * p_view + P * delta
  // and normalizing by w in NDC space preserves full 24-bit floating point mantissa
  // across all astronomical distances (10^16 - 10^20 meters) without catastrophic
  // cancellation or precision truncation against the huge p_view coordinates.
  vec4 centerClip = projectionMatrix * vec4(p_view, 1.0);
  float w = centerClip.w;

  // If star center is behind camera plane, cull early
  if (w <= 0.0) {
    return;
  }

  vec4 offsetRightClip = projectionMatrix * vec4(scaledRight, 0.0);
  vec4 offsetUpClip    = projectionMatrix * vec4(scaledUp, 0.0);

  vec2 centerNdc      = centerClip.xy / w;
  vec2 offsetRightNdc = offsetRightClip.xy / w;
  vec2 offsetUpNdc    = offsetUpClip.xy / w;

  vec4 lowerLeft  = vec4(centerNdc - offsetRightNdc - offsetUpNdc, 0.0, 1.0);
  vec4 upperRight = vec4(centerNdc + offsetRightNdc + offsetUpNdc, 0.0, 1.0);
  vec4 lowerRight = vec4(centerNdc + offsetRightNdc - offsetUpNdc, 0.0, 1.0);
  vec4 upperLeft  = vec4(centerNdc - offsetRightNdc + offsetUpNdc, 0.0, 1.0);

  out_data.screenSpaceDepth = w;

  // Build primitive
  gl_Position = lowerLeft;
  out_data.texCoords = vec2(0.0, 0.0);
  EmitVertex();

  gl_Position = lowerRight;
  out_data.texCoords = vec2(1.0, 0.0);
  EmitVertex();

  gl_Position = upperLeft;
  out_data.texCoords = vec2(0.0, 1.0);
  EmitVertex();

  gl_Position = upperRight;
  out_data.texCoords = vec2(1.0, 1.0);
  EmitVertex();

  EndPrimitive();
}
