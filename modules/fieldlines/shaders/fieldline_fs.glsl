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
#include "PowerScaling/powerScaling_fs.hglsl"

in vec4 gs_color;
in vec4 gs_position;
in vec3 gs_normal;

uniform bool classification;
uniform vec4 fieldLineColor;


Fragment getFragment() {
  float alpha = 1 - length(gs_normal) * length(gs_normal);

  Fragment frag;
  if (classification) {
    frag.color = vec4(gs_color.rgb * alpha, 1.0);
  }
  else {
    frag.color = vec4(fieldLineColor.rgb * fieldLineColor.a * alpha, 1.0);
  }

  frag.depth = pscDepth(gs_position);

  // G-Buffer
  frag.gPosition  = vec4(0.0);//vs_gPosition;
  // There is no normal here
  // TODO: Add the correct normal if necessary (JCC)
  frag.gNormal = vec4(0.0, 0.0, -1.0, 1.0);

  return frag;
}
