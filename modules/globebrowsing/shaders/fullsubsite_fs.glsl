/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

// Uniforms
uniform float transparency;
uniform float alpha;
uniform bool performShading = true;
uniform bool useMastCamColor = false;

uniform vec3 directionToSunViewSpace;

uniform sampler2DArray roverTerrainTextures;

// Input from the vertex shader
in vec2 vs_st;
in vec2 vs_st2;
in vec2 vs_st3;
in vec3 vs_normalViewSpace;
in vec4 vs_positionCameraSpace;
in vec4 vs_positionScreenSpace;
in vec4 vs_color;
flat in int textureIndex;

in vec2 vs_stDone[20];


#include "PowerScaling/powerScaling_fs.hglsl"
#include "fragment.glsl"

Fragment getFragment() {
  int temporary2 = textureIndex;

  vec4 prevTexture3 = texture(roverTerrainTextures, vec3(vs_stDone[textureIndex], temporary2));
  prevTexture3.a = alpha;

  Fragment frag2;
  frag2.color = prevTexture3;
  frag2.depth = vs_positionScreenSpace.w;

  return frag2;
}
