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

in vec2 vs_st;
in float vs_depth;

uniform sampler1D discTexture;
uniform vec2 offset; // relative to semi major axis
uniform float opacity;
uniform float eccentricity;
uniform float semiMajorAxis;
uniform vec3 multiplyColor = vec3(1.0);

const float Epsilon = 0.0000001;


// Compute semi minor axis from major axis, a, and eccentricity, e
float semiMinorAxis(float a, float e) {
  return a * sqrt(1.0 - e * e);
}

// If returned value <= 1, the point is insdie or on the ellipse specified by the input:
// a and b are the semi-major and semi-minor axes, respectively.
// cx is the displacement of the center of the ellipse along the x-axis (for an orbit,
// the y-displacement is always zero)
float ellipseTest(vec2 point, float a, float b, float cx) {
  float x = point.x;
  float y = point.y;
  return (pow(x - cx, 2.0) / (a*a)) + ((y*y) / (b*b));
}


Fragment getFragment() {
  // Moving the origin to the center
  vec2 st = (vs_st - vec2(0.5)) * 2.0;

  float offsetLower = offset.x * semiMajorAxis;
  float offsetUpper = offset.y * semiMajorAxis;

  float AUpper = semiMajorAxis + offsetUpper;
  float BUpper = semiMinorAxis(AUpper, eccentricity);
  float CUpper = sqrt(AUpper*AUpper - BUpper*BUpper);
  float outerApoapsisDistance = AUpper * (1.0 + eccentricity);

  float ALower = semiMajorAxis - offsetLower;
  float BLower = semiMinorAxis(ALower, eccentricity);
  float CLower = sqrt(ALower*ALower - BLower*BLower);
  float innerApoapsisDistance = ALower * (1.0 + eccentricity);

  // Normalize based on outer apoapsis distance (size of plane)
  float AU_n = AUpper / outerApoapsisDistance;
  float BU_n = BUpper / outerApoapsisDistance;
  float CU_n = CUpper / outerApoapsisDistance;
  float AL_n = ALower / outerApoapsisDistance;
  float BL_n = BLower / outerApoapsisDistance;
  float CL_n = CLower / outerApoapsisDistance;

  if (eccentricity <= Epsilon) {
    CU_n = 0.0;
    CL_n = 0.0;
  }

  float outer = ellipseTest(st, AU_n, BU_n, -CU_n);
  float inner = ellipseTest(st, AL_n, BL_n, -CL_n);
  if (outer > 1.0 || inner < 1.0) {
    // Point is outside outer ellipse or inside inner ellipse
    discard;
  }

  // Remapping the texture coordinates
  vec2 dir = normalize(st);

  // Find outer ellipse: where along the direction does the equation == 1?
  float denominator = pow(BU_n * dir.x, 2.0) + pow(AU_n * dir.y, 2.0);
  float first = -(BU_n * BU_n * dir.x * CU_n) / denominator;
  float second = pow((BU_n * BU_n * dir.x * CU_n)  /  denominator, 2.0);
  float third = (pow(BU_n * CU_n, 2.0) - pow(AU_n * BU_n, 2.0)) / denominator;

  float scale = first + sqrt(second - third);

  vec2 outerPoint = dir * scale;
  vec2 innerPoint = outerPoint * (innerApoapsisDistance / outerApoapsisDistance);

  float discWidth = distance(outerPoint, innerPoint);
  float distanceFromOuterEdge = distance(outerPoint, st);
  float relativeDistance = distanceFromOuterEdge / discWidth;

  // Compute texture coordinate based on the distance to outer edge
  float textureCoord = 0.0;

  // The midpoint (textureCoord = 0.5) depends on the ratio between the offsets
  // (Note that the texture goes from outer to inner edge of disc)
  float midPoint = offsetUpper / (offsetUpper + offsetLower);
  if (relativeDistance > midPoint) {
    textureCoord = 0.5 + 0.5 * (relativeDistance - midPoint) / (1.0 - midPoint);
  }
  else {
    textureCoord = 0.5 * (relativeDistance / midPoint);
  }

  vec4 diffuse = texture(discTexture, textureCoord);
  diffuse.a *= opacity;
  diffuse.rgb *= multiplyColor;

  Fragment frag;
  frag.color = diffuse;
  frag.depth = vs_depth;
  return frag;
}
