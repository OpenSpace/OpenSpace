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
in vec4 vs_position;
//in vec4 vs_gPosition;
//in vec3 vs_gNormal;

uniform sampler1D texture1;
uniform vec2 textureOffset;
uniform float colorFilterValue;
uniform bool hasSunPosition;
uniform vec3 sunPosition;
uniform float _nightFactor;


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

  vec4 diffuse = texture(texture1, texCoord);
  // divided by 3 as length of vec3(1.0, 1.0, 1.0) will return 3 and we want
  // to normalize the alpha value to [0,1]
  float colorValue = length(diffuse.rgb) / 3.0;
  if (colorValue < colorFilterValue) {
    diffuse.a = colorValue * colorFilterValue;
    if (diffuse.a < 0.65) {
      discard;
    }
  }

  // The normal for the one plane depends on whether we are dealing
  // with a front facing or back facing fragment
  vec3 normal;
  // The plane is oriented on the xz plane
  // WARNING: This might not be the case for Uranus
  if (gl_FrontFacing) {
    normal = vec3(-1.0, 0.0, 0.0);
  }
  else {
    normal = vec3(1.0, 0.0, 0.0);
  }

  // Reduce the color of the fragment by the user factor
  // if we are facing away from the Sun
  if (dot(sunPosition, normal) < 0) {
    diffuse.xyz *= _nightFactor;
  }

  Fragment frag;
  frag.color = diffuse;
  frag.depth = vs_position.w;

  //frag.gPosition  = vs_gPosition;
  //frag.gNormal    = vs_gNormal;

  return frag;
}
