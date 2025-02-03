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

#include "fragment.glsl"

in vec2 uv;

uniform sampler2D renderedTexture;
uniform dmat4 projection;
uniform vec2 screenSize;
uniform int filterSize;
uniform float sigma;
uniform float pixelWeightThreshold;

const float M_PI = 3.141592653589793238462;
const float DEFAULT_DEPTH = 3.08567758e19; // 1000 Pc


Fragment getFragment() {
  vec4 color = vec4(0.0);

  // GL_POINTS

  // Use frustum params to be able to compensate for a skewed frustum (in a dome).
  float near = float(projection[3][2] / (projection[2][2] - 1.0));
  float left = float(near * (projection[2][0] - 1.0) / projection[0][0]);
  float right = float(near * (projection[2][0] + 1.0) / projection[0][0]);
  float top = float(near * (projection[2][1] + 1.0) / projection[1][1]);
  float bottom = float(near * (projection[2][1] - 1.0) / projection[1][1]);

  float xFactor = float(projection[0][0]);
  float yFactor = float(projection[1][1]);

  float planeAspect = yFactor / xFactor; // Equals: (right - left) / (top - bottom)
  float screenAspect = screenSize.x / screenSize.y;
  float fullAspect = planeAspect / screenAspect;

  // Find screenPos in skewed frustum. uv is [0, 1]
  vec2 screenPos = uv * vec2(right - left, top - bottom) + vec2(left, bottom);

  // Find our elliptic scale factors by trigonometric approximation.
  float beta = atan(length(screenPos) / near);
  vec2 sigmaScaleFactor = vec2(1.0 / cos(beta), 1.0 / pow(cos(beta), 2.0));

  float defaultScreen = 1200.0;
  float scaling = screenSize.y / defaultScreen * yFactor;

  // Scale filter size depending on screen pos.
  vec2 filterScaleFactor = vec2(
    pow(screenPos.x / near, 2.0) * fullAspect,
    pow(screenPos.y / near, 2.0)
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

  // Make use of the following flag this to toggle betweeen circular and elliptic distribution.
  bool useCircleDist = false;

  // Apply scaling on bloom filter.
  vec2 newFilterSize = vec2(filterSize) * (1.0 + length(filterScaleFactor)) * scaling;

  // Calculate params for a rotated Elliptic Gaussian distribution.
  float sigmaMajor;
  float sigmaMinor;
  float a;
  float b;
  float c;
  if (!useCircleDist) {
    float alpha = atan(screenPos.y, screenPos.x);
    // Apply scaling on sigma.
    sigmaMajor = sigma * sigmaScaleFactor.y * scaling;
    sigmaMinor = sigma * sigmaScaleFactor.x * scaling;

    a = pow(cos(alpha), 2.0) / (2 * pow(sigmaMajor, 2.0)) +
        pow(sin(alpha), 2.0) / (2 * pow(sigmaMinor, 2.0)) ;
    b = sin(2 * alpha) / (4 * pow(sigmaMajor, 2.0)) -
        sin(2 * alpha) / (4 * pow(sigmaMinor, 2.0)) ;
    c = pow(sin(alpha), 2.0) / (2 * pow(sigmaMajor, 2.0)) +
        pow(cos(alpha), 2.0) / (2 * pow(sigmaMinor, 2.0)) ;
  }

  // Get a [newFilterSize x newFilterSize] filter around our pixel. UV is [0, 1]
  vec3 intensity = vec3(0.0);
  vec2 pixelSize = 1.0 / screenSize;
  ivec2 halfFilterSize = ivec2((newFilterSize - 1.0) / 2.0);
  for (int y = -halfFilterSize.y; y <= halfFilterSize.y; y += 1) {
    for (int x = -halfFilterSize.x; x <= halfFilterSize.x; x += 1) {
      vec2 sPoint = uv + (pixelSize * ivec2(x, y));

      // Calculate the contribution of this pixel (elliptic gaussian distribution).
      float pixelWeight = exp(-(
          a * pow(x * fullAspect, 2.0) + 2 * b * x * y * fullAspect + c * pow(y, 2.0)
      ));

      // Only sample inside FBO texture and if the pixel will contribute to final color.
      if (all(greaterThan(sPoint, vec2(0.0))) && all(lessThan(sPoint, vec2(1.0))) &&
          pixelWeight > pixelWeightThreshold)
      {
        vec4 sIntensity = texture(renderedTexture, sPoint);

        // Use normal distribution function for halo/bloom effect.
        if (useCircleDist) {
          float circleDist = sqrt(pow(x / (1 + length(filterScaleFactor)), 2.0) +
            pow(y / (1 + length(filterScaleFactor)), 2.0));
          intensity += sIntensity.rgb * (1.0 / (sigma * sqrt(2.0 * M_PI))) *
            exp(-(pow(circleDist, 2.0) / (2.0 * pow(sigma, 2.0)))) / filterSize;
        }
        else {
          // Divide contribution by area of ellipse.
          intensity += sIntensity.rgb * pixelWeight * fullAspect;
        }
      }
    }
  }
  // Tonemap intensity to color!
  //intensity = 1.0 - 1.0 * exp(-25.0 * intensity);
  intensity = pow(intensity, vec3(0.8));

  if (length(intensity) < 0.01) {
    discard;
  }

  color = vec4(intensity, 1.0);

  // Use the following to check for any intensity at all.
  //color = (length(intensity.rgb) > 0.001) ? vec4(1.0) : vec4(0.0);

  Fragment frag;
  frag.color = color;
  // Place stars at back to begin with.
  frag.depth = DEFAULT_DEPTH;
  frag.gNormal = vec4(0.0, 0.0, 0.0, 1.0);
  frag.blend = BLEND_MODE_NORMAL;

  return frag;
}
