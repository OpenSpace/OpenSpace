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

#version __CONTEXT__

in vec3 in_position;
in vec3 in_bvLumAbsMag;
in vec3 in_velocity;
in float in_speed;

out vec3 vs_bvLumAbsMag;
out vec3 vs_velocity;
out float vs_speed;

uniform int useProperMotion;
uniform float diffTime;

void main() {
  vs_bvLumAbsMag = in_bvLumAbsMag;
  vs_velocity = in_velocity;
  vs_speed = in_speed;

  if (useProperMotion == 1) {
    // 1000 to get from km/s to m/s
    dvec3 offsetInParsec = dvec3(in_velocity) * double(diffTime) * 1000.0;
    vec3 pos = in_position + vec3(offsetInParsec);
    gl_Position = vec4(pos, 1.0);
  }
  else {
    gl_Position = vec4(in_position, 1.0);
  }

}
