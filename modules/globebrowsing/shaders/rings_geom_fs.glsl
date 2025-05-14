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

#include "PowerScaling/powerScaling_fs.hglsl"
#include "fragment.glsl"

in vec2 vs_st;
in float vs_screenSpaceDepth;

uniform sampler1D ringTexture;
uniform vec2 textureOffset;


Fragment getFragment() {
  // Moving the origin to the center
  vec2 st = (vs_st - vec2(0.5)) * 2.0;

  // The length of the texture coordinates vector is our distance from the center
  float radius = length(st);

  // We only want to consider ring-like objects so we need to discard everything else
  if (radius > 1.0) {
    discard;
  }

  // Remapping the texture coordinates
  // Radius \in [0,1],  texCoord \in [textureOffset.x, textureOffset.y]
  // textureOffset.x -> 0
  // textureOffset.y -> 1
  float texCoord = (radius - textureOffset.x) / (textureOffset.y - textureOffset.x);
  if (texCoord < 0.0 || texCoord > 1.0) {
    discard;
  }

  float diffuse = length(texture(ringTexture, texCoord).rgb);

  // The normal for the one plane depends on whether we are dealing
  // with a front facing or back facing fragment
  //vec3 normal;
  // The plane is oriented on the xz plane
  // WARNING: This might not be the case for Uranus
  // if (gl_FrontFacing) {
  //     normal = vec3(-1.0, 0.0, 0.0);
  // }
  // else {
  //     normal = vec3(1.0, 0.0, 0.0);
  // }

  Fragment frag;
  frag.color = vec4(vec3(vs_screenSpaceDepth), 1.0);
  frag.depth = (diffuse < 0.5) ? 1E30 : vs_screenSpaceDepth;

  return frag;
}
