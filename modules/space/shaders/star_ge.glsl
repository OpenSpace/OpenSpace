/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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
in vec3 vs_bvLumAbsMag[];
in vec3 vs_velocity[];
in float vs_speed[];

layout(triangle_strip, max_vertices = 4) out;
out vec3 vs_position;
out vec2 texCoords;
flat out float ge_bv;
flat out vec3 ge_velocity;
flat out float ge_speed;
flat out float gs_screenSpaceDepth;

uniform float magnitudeExponent;
uniform dvec3 eyePosition;
uniform dvec3 cameraUp;
uniform int sizeComposition;
uniform float lumCent;
uniform float radiusCent;
uniform dmat4 cameraViewProjectionMatrix;
uniform dmat4 modelMatrix;

const double PARSEC = 3.08567756E16;

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
  return 4600 * (1.0 / (tmp + 1.7) + 1.0 / (tmp + 0.62));
}

double scaleForApparentBrightness(dvec3 dpos, float luminance) {
  // Working like Partiview
  double pSize = pow(10, 29.0 + magnitudeExponent);
  float luminosity = luminance * 10.0;
  double distanceToStar = length(dpos - eyePosition);
  return (pSize * luminosity) / distanceToStar;
}

double scaleForLuminositySize(float bv, float luminance, float absMagnitude) {
  double adjustedLuminance = luminance + 5E9;
  float L_over_Lsun = pow(2.51, SunAbsMagnitude - absMagnitude);
  float temperature = bvToKelvin(bv);
  float relativeTemperature = SunTemperature / temperature;
  double starRadius = SunRadius * pow(relativeTemperature, 2.0) * sqrt(L_over_Lsun);
  return (lumCent * adjustedLuminance + (radiusCent * starRadius)) * pow(10.0, magnitudeExponent);
}

double scaleForAbsoluteMagnitude(float absMagnitude) {
  return (-absMagnitude + 35) * pow(10.0, magnitudeExponent + 8.5);
}

double scaleForApparentMagnitude(dvec3 dpos, float absMag) {
  double distanceToStarInMeters = length(dpos - eyePosition);
  double distanceToCenterInMeters = length(eyePosition);
  float distanceToStarInParsecs = float(distanceToStarInMeters/PARSEC);
  float appMag = absMag + 5.0 * (log(distanceToStarInParsecs/10.0)/log(2.0));
  return (-appMag + 50.0) * pow(10.0, magnitudeExponent + 7.5);
}

double scaleForDistanceModulus(float absMag) {
  return exp((-30.623 - absMag) * 0.462) * pow(10.0, magnitudeExponent + 12.5) * 2000;
}


void main() {
  vec3 pos = gl_in[0].gl_Position.xyz;
  vs_position = pos; // in object space
  dvec4 dpos  = modelMatrix * dvec4(pos, 1.0);

  ge_bv = vs_bvLumAbsMag[0].x;
  ge_velocity = vs_velocity[0];
  ge_speed = vs_speed[0];

  double scaleMultiply = 1.0;

  if (sizeComposition == SizeCompositionOptionLumSizeDistanceModulus) {
    float absMagnitude = vs_bvLumAbsMag[0].z;

    scaleMultiply = scaleForDistanceModulus(absMagnitude);
  }
  else if (sizeComposition == SizeCompositionOptionAppBrightness) {
    float luminance = vs_bvLumAbsMag[0].y;

    scaleMultiply = scaleForApparentBrightness(dpos.xyz, luminance);
  }
  else if (sizeComposition == SizeCompositionOptionLumSize) {
    float bv = vs_bvLumAbsMag[0].x;
    float luminance = vs_bvLumAbsMag[0].y;
    float absMagnitude = vs_bvLumAbsMag[0].z;

    scaleMultiply = scaleForLuminositySize(bv, luminance, absMagnitude);
  }
  else if (sizeComposition == SizeCompositionOptionLumSizeAbsMagnitude) {
    float absMagnitude = vs_bvLumAbsMag[0].z;

    scaleMultiply = scaleForAbsoluteMagnitude(absMagnitude);
  }
  else if (sizeComposition == SizeCompositionOptionLumSizeAppMagnitude) {
    float absMagnitude = vs_bvLumAbsMag[0].z;

    scaleMultiply = scaleForApparentMagnitude(dpos.xyz, absMagnitude);
  }

  dvec3 normal = eyePosition - dpos.xyz;
  dvec3 newRight = normalize(cross(cameraUp, normal));
  dvec3 newUp = normalize(cross(normal, newRight));
  dvec3 scaledRight = scaleMultiply * newRight;
  dvec3 scaledUp = scaleMultiply * newUp;

  vec4 lowerLeft = z_normalization(
    vec4(cameraViewProjectionMatrix * dvec4(dpos.xyz - scaledRight - scaledUp, dpos.w))
  );

  vec4 upperRight = z_normalization(
    vec4(cameraViewProjectionMatrix * dvec4(dpos.xyz + scaledUp + scaledRight, dpos.w))
  );

  vec4 lowerRight = z_normalization(
    vec4(cameraViewProjectionMatrix * dvec4(dpos.xyz + scaledRight - scaledUp, dpos.w))
  );

  vec4 upperLeft = z_normalization(
    vec4(cameraViewProjectionMatrix * dvec4(dpos.xyz + scaledUp - scaledRight, dpos.w))
  );

  gs_screenSpaceDepth = lowerLeft.w;

  // Build primitive
  gl_Position = lowerLeft;
  texCoords = vec2(0.0, 0.0);
  EmitVertex();

  gl_Position = lowerRight;
  texCoords = vec2(1.0, 0.0);
  EmitVertex();

  gl_Position = upperLeft;
  texCoords = vec2(0.0, 1.0);
  EmitVertex();

  gl_Position = upperRight;
  texCoords = vec2(1.0, 1.0);
  EmitVertex();

  EndPrimitive();
}
