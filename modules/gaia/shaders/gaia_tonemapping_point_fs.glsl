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

#include "fragment.glsl"

in Data {
  vec2 texCoords;
} in_data;

uniform sampler2D renderedTexture;
uniform mat4 projection;
uniform vec2 screenSize;
uniform int filterSize;
uniform float sigma;
uniform float pixelWeightThreshold;

const float M_PI = 3.141592653589793238462;
const float DefaultDepth = 3.08567758e19; // 1000 Pc


Fragment getFragment() {
  // Use frustum params to be able to compensate for a skewed frustum (in a dome)
  dmat4 proj = dmat4(projection);
  float near = float(proj[3][2] / (proj[2][2] - 1.0));
  float left = float(near * (proj[2][0] - 1.0) / proj[0][0]);
  float right = float(near * (proj[2][0] + 1.0) / proj[0][0]);
  float top = float(near * (proj[2][1] + 1.0) / proj[1][1]);
  float bottom = float(near * (proj[2][1] - 1.0) / proj[1][1]);

  float xFactor = float(proj[0][0]);
  float yFactor = float(proj[1][1]);

  float planeAspect = yFactor / xFactor; // Equals: (right - left) / (top - bottom)
  float screenAspect = screenSize.x / screenSize.y;
  float fullAspect = planeAspect / screenAspect;

  // Find screenPos in skewed frustum. uv is [0, 1]
  vec2 screenPos =
    in_data.texCoords * vec2(right - left, top - bottom) + vec2(left, bottom);

  // Find our elliptic scale factors by trigonometric approximation
  float beta = atan(length(screenPos) / near);
  vec2 sigmaScaleFactor = vec2(1.0 / cos(beta), 1.0 / pow(cos(beta), 2.0));

  float defaultScreen = 1200.0;
  float scaling = screenSize.y / defaultScreen * yFactor;

  // Scale filter size depending on screen pos
  // The screen position can be negative, for which `pow` is undefined
  vec2 screenPosNear = screenPos / near;
  vec2 filterScaleFactor = vec2(
    screenPosNear.x * screenPosNear.x * fullAspect,
    screenPosNear.y * screenPosNear.y
  );

  // Use to ignore scaling.
  //filterScaleFactor = vec2(0.0);
  //scaling = 1.0;
  //sigmaScaleFactor = vec2(1.0);

  // Use the following to find the origo in a skewed frustum.
  //Fragment origoFrag;
  //vec2 screenOrigo = vec2(-left, -bottom) / vec2(right - left, top - bottom);
  //if (abs(screenOrigo.x - uv.x) > 0.0005 && abs(screenOrigo.y - uv.y) > 0.0005) {
  //    origoFrag.color = vec4(0.0);
  //}
  //else {
  //    origoFrag.color = vec4(1.0);
  //}
  //return origoFrag;

  // Uncomment to compare to original filterSize (assumes origo in center of screen).
  //screenPos = (uv - 0.5) * 2.0; // [-1, 1]
  //filterScaleFactor = vec2(
  //    pow(screenPos.x, 2.0),
  //    pow(screenPos.y, 2.0)
  //);

  // Make use of the following flag this to toggle between circular and elliptic
  // distribution.
  bool useCircleDist = false;

  // Apply scaling on bloom filter
  vec2 newFilterSize = vec2(filterSize) * (1.0 + length(filterScaleFactor)) * scaling;

  // Calculate params for a rotated Elliptic Gaussian distribution
  float a = 0.0;
  float b = 0.0;
  float c = 0.0;
  if (!useCircleDist) {
    // `atan` is undefined if both arguments are 0
    float alpha = screenPos == vec2(0.0) ? 0.0 : atan(screenPos.y, screenPos.x);
    // Apply scaling on sigma.
    float sigmaMajor = sigma * sigmaScaleFactor.y * scaling;
    float sigmaMinor = sigma * sigmaScaleFactor.x * scaling;

    // The sine and cosine can be negative, for which `pow` is undefined
    float cosAlpha2 = cos(alpha) * cos(alpha);
    float sinAlpha2 = sin(alpha) * sin(alpha);
    float sigmaMajor2 = sigmaMajor * sigmaMajor;
    float sigmaMinor2 = sigmaMinor * sigmaMinor;

    a = cosAlpha2 / (2.0 * sigmaMajor2) + sinAlpha2 / (2.0 * sigmaMinor2);
    b = sin(2.0 * alpha) / (4.0 * sigmaMajor2) - sin(2.0 * alpha) / (4.0 * sigmaMinor2);
    c = sinAlpha2 / (2.0 * sigmaMajor2) + cosAlpha2 / (2.0 * sigmaMinor2);
  }

  // Get a [newFilterSize x newFilterSize] filter around our pixel. UV is [0, 1]
  vec3 intensity = vec3(0.0);
  vec2 pixelSize = 1.0 / screenSize;
  ivec2 halfFilterSize = ivec2((newFilterSize - 1.0) / 2.0);
  for (int y = -halfFilterSize.y; y <= halfFilterSize.y; y++) {
    for (int x = -halfFilterSize.x; x <= halfFilterSize.x; x++) {
      vec2 sPoint = in_data.texCoords + (pixelSize * ivec2(x, y));

      // Calculate the contribution of this pixel (elliptic gaussian distribution)
      float xAspect = float(x) * fullAspect;
      float pixelWeight = exp(
        -(a * xAspect * xAspect + 2.0 * b * xAspect * float(y) + c * float(y * y))
      );

      // Only sample inside FBO texture and if the pixel will contribute to final color
      if (all(greaterThan(sPoint, vec2(0.0))) && all(lessThan(sPoint, vec2(1.0))) &&
          pixelWeight > pixelWeightThreshold)
      {
        vec4 sIntensity = texture(renderedTexture, sPoint);

        // Use normal distribution function for halo/bloom effect
        if (useCircleDist) {
          vec2 circlePos = vec2(x, y) / (1.0 + length(filterScaleFactor));
          float circleDist2 = dot(circlePos, circlePos);
          intensity += sIntensity.rgb * (1.0 / (sigma * sqrt(2.0 * M_PI))) *
            exp(-(circleDist2 / (2.0 * sigma * sigma))) / filterSize;
        }
        else {
          // Divide contribution by area of ellipse
          intensity += sIntensity.rgb * pixelWeight * fullAspect;
        }
      }
    }
  }

  // Tonemap intensity to color
  intensity = pow(intensity, vec3(0.8));

  if (length(intensity) < 0.01) {
    discard;
  }

  Fragment frag;
  frag.color = vec4(intensity, 1.0);
  frag.depth = DefaultDepth;
  frag.gNormal = vec4(0.0, 0.0, 0.0, 1.0);
  frag.blend = BlendModeNormal;

  return frag;
}
