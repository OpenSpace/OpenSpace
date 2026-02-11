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
  float depth;
} in_data;

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
  return (pow(x - cx, 2.0) / (a * a)) + ((y * y) / (b * b));
}


Fragment getFragment() {
  // Moving the origin to the center
  vec2 st = (in_data.texCoords - vec2(0.5)) * 2.0;

  float offsetLower = offset.x * semiMajorAxis;
  float offsetUpper = offset.y * semiMajorAxis;

  float aUpper = semiMajorAxis + offsetUpper;
  float bUpper = semiMinorAxis(aUpper, eccentricity);
  float cUpper = sqrt(aUpper * aUpper - bUpper * bUpper);
  float outerApoapsisDistance = aUpper * (1.0 + eccentricity);

  float aLower = semiMajorAxis - offsetLower;
  float bLower = semiMinorAxis(aLower, eccentricity);
  float cLower = sqrt(aLower * aLower - bLower * bLower);
  float innerApoapsisDistance = aLower * (1.0 + eccentricity);

  // Normalize based on outer apoapsis distance (size of plane)
  float normAU = aUpper / outerApoapsisDistance;
  float normBU = bUpper / outerApoapsisDistance;
  float normCU = eccentricity <= Epsilon  ?  cUpper / outerApoapsisDistance  :  0.0;
  float normAL = aLower / outerApoapsisDistance;
  float normBL = bLower / outerApoapsisDistance;
  float normCL = eccentricity <= Epsilon  ?  cLower / outerApoapsisDistance  :  0.0;

  float outer = ellipseTest(st, normAU, normBU, -normCU);
  float inner = ellipseTest(st, normAL, normBL, -normCL);
  if (outer > 1.0 || inner < 1.0) {
    // Point is outside outer ellipse or inside inner ellipse
    discard;
  }

  // Remapping the texture coordinates
  vec2 dir = normalize(st);

  // Find outer ellipse: where along the direction does the equation == 1?
  float denominator = pow(normBU * dir.x, 2.0) + pow(normAU * dir.y, 2.0);
  float first = -(normBU * normBU * dir.x * normCU) / denominator;
  float second = pow((normBU * normBU * dir.x * normCU)  /  denominator, 2.0);
  float third = (pow(normBU * normCU, 2.0) - pow(normAU * normBU, 2.0)) / denominator;

  float scale = first + sqrt(second - third);

  vec2 outerPoint = dir * scale;
  vec2 innerPoint = outerPoint * (innerApoapsisDistance / outerApoapsisDistance);

  float discWidth = distance(outerPoint, innerPoint);
  float distanceFromOuterEdge = distance(outerPoint, st);
  float relativeDistance = distanceFromOuterEdge / discWidth;

  // Compute texture coordinate based on the distance to outer edge

  // The midpoint (textureCoord = 0.5) depends on the ratio between the offsets
  // (Note that the texture goes from outer to inner edge of disc)
  float midPoint = offsetUpper / (offsetUpper + offsetLower);
  float textureCoord = relativeDistance > midPoint ?
    0.5 + 0.5 * (relativeDistance - midPoint) / (1.0 - midPoint) :
    0.5 * (relativeDistance / midPoint);

  Fragment frag;
  frag.color = texture(discTexture, textureCoord) * vec4(multiplyColor, opacity);
  frag.depth = in_data.depth;
  return frag;
}
