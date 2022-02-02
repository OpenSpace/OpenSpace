/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

//in float ge_vertexID;
in vec4 ge_position;
in vec4 ge_interpColor;
noperspective in vec2 ge_mathLine;

uniform float lineWidth;

Fragment getFragment() {
  Fragment frag;

  frag.gPosition = ge_position;
  frag.depth = ge_position.w;
  frag.color = ge_interpColor;
  frag.gNormal = vec4(0.0, 0.0, -1.0, 1.0);
  //frag.blend = BLEND_MODE_ADDITIVE;

  double distanceCenter = length(ge_mathLine - vec2(gl_FragCoord.xy));
  double dLineWidth = double(lineWidth);
  float blendFactor = 20;
   
  if (distanceCenter > dLineWidth) {
    frag.color.a = 0.0;
  }
  else {
    frag.color.a *= pow(float((dLineWidth - distanceCenter) / dLineWidth), blendFactor);
  }

  return frag;
}
