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
#include "floatoperations.glsl"

in vec4 vs_position;
in vec3 vs_color;
in float vs_screenSpaceDepth;
in float vs_starBrightness;

uniform float opacityCoefficient;


Fragment getFragment() {
  Fragment frag;

  float multipliedOpacityCoefficient = opacityCoefficient * opacityCoefficient;
  vec3 extinction = exp(vec3(0.6, 0.2, 0.3) - vs_color);

  // We use the star brightness as the alpha value here to dim the stars as the camera
  // moves further away.  Otherwise they would occlude the main milkway image even though
  // they themselves nolonger have any color contribution left
  vec4 fullColor = vec4(
    vs_color * extinction * vs_starBrightness * multipliedOpacityCoefficient,
    vs_starBrightness
  );
  frag.color = fullColor;

  frag.depth = vs_screenSpaceDepth;
  frag.gPosition = vs_position;
  frag.gNormal = vec4(0.0, 0.0, 0.0, 1.0);

  return frag;
}
