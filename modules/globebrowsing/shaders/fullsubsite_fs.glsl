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
uniform float fading;
uniform bool performShading = true;
uniform bool useMastCamColor = false;

uniform vec3 directionToSunViewSpace;

uniform sampler2D texture1;
uniform sampler2D texture2;
uniform sampler2D texture3;

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
in vec2 textureIndexDone[20];
flat in int vs_size;

#include "PowerScaling/powerScaling_fs.hglsl"
#include "fragment.glsl"

Fragment getFragment() {

  float alpha = 1;
  vec4 diffuseAl;
  vec4 prevTexture = vec4(1,0,0,1);
  int size = vs_size;
  int counter = 0;

  /*for (int i = 0; i < vs_size; ++i) {
    int temporary = int(textureIndexDone[i].x);
    vec4 textureSample = texture(roverTerrainTextures, vec3(vs_stDone[i], temporary));
      diffuseAl = mix(prevTexture, textureSample, alpha);
      prevTexture = diffuseAl;
  }*/

  vec4 textureSample = texture(roverTerrainTextures, vec3(vs_st, textureIndex));
  prevTexture = vec4(textureSample.rgb, 1.0);


  Fragment frag2;
  frag2.color = prevTexture;
  frag2.depth = vs_positionScreenSpace.w;

  return frag2;
}
