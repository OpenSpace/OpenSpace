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

#include "PowerScaling/powerScaling_fs.hglsl"
#include "fragment.glsl"

in vec2 out_position;

uniform float opacity;
uniform float thickness;
uniform vec3 color;


Fragment getFragment() {
  // calculate normal from texture coordinates
  vec3 n;
  n.xy = gl_PointCoord.st * vec2(2.0, -2.0) + vec2(-1.0, 1.0);
  float mag = dot(n.xy, n.xy);
  if (mag > 1.0) {
    // kill pixels outside circle
    discard;
  }
  n.z = sqrt(1.0 - mag);
  
  // calculate lighting
  vec3 light_dir = vec3(0.0, 0.0, 1.0);
  float diffuse = max(0.0, dot(light_dir, n));
  float alpha = min(pow(sqrt(mag), thickness), opacity);

  Fragment frag;
  frag.color = vec4(color * diffuse, alpha);
  frag.depth = 1.0;
  return frag;
}
