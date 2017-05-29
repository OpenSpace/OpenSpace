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

int colorMap[21][3] = {
    {204, 102, 0},
    {199, 102, 5},
    {194, 102, 10},
    {189, 102, 15},
    {184, 102, 20},
    {179, 102, 25},
    {173, 102, 31},
    {168, 102, 36},
    {163, 102, 41},
    {158, 102, 46},
    {153, 102, 51},
    {148, 102, 56},
    {143, 102, 61},
    {138, 102, 66},
    {133, 102, 71},
    {128, 102, 77},
    {122, 102, 82},
    {117, 102, 87},
    {112, 102, 92},
    {107, 102, 97},
    {102, 102, 102}
};

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
  /*for (int i = 0; i < size; i++) {
    //if (vs_stDone[i].s > 0 && vs_stDone[i].t > 0 && vs_stDone[i].s < 1 && vs_stDone[i].t < 1) {
      vec4 textureSample = texture(roverTerrainTextures, vec3(vs_stDone[i], i));

      if (counter == 0) {
        counter++;
        prevTexture = vec4(textureSample.rgb, 1.0);
      } else {
        diffuseAl = mix(prevTexture, textureSample, alpha);
        prevTexture = diffuseAl;
      }

    //}
  }*/

  /*for (int i = 0; i < vs_size; ++i) {
    vec4 textureSample = texture(roverTerrainTextures, vec3(vs_stDone[i], textureIndexDone[i].s));
    if (vs_size > 1 && i > 0) {
        diffuseAl = mix(prevTexture, textureSample, alpha);
        prevTexture = diffuseAl;
      } else {
        prevTexture = textureSample;
      }
  }*/

  vec4 textureSample = texture(roverTerrainTextures, vec3(vs_st, textureIndex));
  prevTexture = vec4(textureSample.rgb, 1.0);


  /*vec4 textureSample = texture(texture1, vs_stDone[0]);
  vec4 textureSample2 = texture(texture2, vs_stDone[1]);
  vec4 textureSample3 = texture(texture3, vs_stDone[2]);

  vec3 diffuseAl;
  vec3 diffuseAl2;
  vec3 diffuseAl3;
  bool tex1 = false;
  bool tex2 = false;
  bool tex3 = false;
  if (vs_stDone[0].s > 0 && vs_stDone[0].t > 0 && vs_stDone[0].s < 1 && vs_stDone[0].t < 1) {
      tex1 = true;
      diffuseAl = textureSample.rgb;
  }
  if (vs_stDone[1].s > 0 && vs_stDone[1].t > 0 && vs_stDone[1].s < 1 && vs_stDone[1].t < 1) {
      tex2 = true;
      diffuseAl2 = textureSample2.rgb;
  }
  if (vs_stDone[2].s > 0 && vs_stDone[2].t > 0 && vs_stDone[2].s < 1 && vs_stDone[2].t < 1) {
      tex3 = true;
      diffuseAl3 = textureSample3.rgb;
  }
  float alpha2 = 1;
  vec4 colorS = vec4(diffuseAl, alpha2);
  vec4 colorS2 = vec4(diffuseAl2, alpha2);
  vec4 colorS3 = vec4(diffuseAl3, alpha2);
  vec4 colorSS;
  vec4 colorSSS;

  if (tex1 && tex2) {
      colorSS = mix(colorS, colorS2, alpha2);
      colorSS += vec4(1,0,0,0.2);
  } else if (tex1) {
      colorSS = colorS;
  } else if (tex2) {
      colorSS = colorS2;
  } else {
      colorSS = vec4(1,0,0,0);
  }

  if (tex3) {
    colorSSS = mix(colorSS, colorS3, alpha2);
  } else {
    colorSSS = colorSS;
  }*/


  Fragment frag2;
  frag2.color = prevTexture;
  frag2.depth = vs_positionScreenSpace.w;

  return frag2;
}
