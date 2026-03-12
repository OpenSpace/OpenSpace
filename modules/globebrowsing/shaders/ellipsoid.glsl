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

bool rayIntersectsEllipsoid(vec3 rayOrigin, vec3 rayDir, vec3 ellipsoidCenter,
                            vec3 ellipsoidRadii)
{
  // Translate ray to ellipsoid's local coordinate system
  vec3 oc = rayOrigin - ellipsoidCenter;

  // Normalize by ellipsoid radii to convert to unit sphere problem
  vec3 ocNorm = oc / ellipsoidRadii;
  vec3 dirNorm = rayDir / ellipsoidRadii;

  // Quadratic equation coefficients: A*t^2 + B*t + C = 0
  float a = dot(dirNorm, dirNorm);
  float b = dot(ocNorm, dirNorm); // Note: factor of 2 moved to discriminant calc
  float c = dot(ocNorm, ocNorm) - 1.0;

  // Calculate discriminant (optimized: b^2 - ac since we factored out the 2)
  float discriminant = b * b - a * c;

  // Early exit if no intersection
  if (discriminant < 0.0) {
    return false;
  }

  // Check if at least one intersection is in front of ray origin
  // For quadratic A*t^2 + 2*B*t + C = 0, if we want to check if any t >= 0:
  // If C <= 0, ray origin is inside ellipsoid, so definitely intersects
  if (c <= 0.0) {
    return true;
  }

  // If both intersections exist and C > 0, check if the smaller root t1 >= 0
  // t1 = (-b - sqrt(discriminant)) / a
  // Since we need t1 >= 0: -b - sqrt(discriminant) >= 0
  // This means: -b >= sqrt(discriminant), so b <= -sqrt(discriminant)
  // Since sqrt(discriminant) >= 0, this means b <= 0
  return b <= 0.0;
}
