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

// uniform float time;
in vec2 vs_st;
in vec4 vs_positionScreenSpace;
in vec4 clipSpace;
in vec4 posModel;
in vec3 rawPos;

uniform mat4 trans;
uniform vec3 sunDir;
uniform sampler2D imageryTexture;
uniform sampler1D lut;
uniform vec3 planePosition;

// TODO(mnoven): Uniform
const float FULL_PLANE_SIZE = (1391600000 * 0.5) / 0.785;

#include "fragment.glsl"

Fragment getFragment() {
    vec4 outColor;

    vec3 unitSpacecraftDir = normalize(-sunDir);
    vec3 positionModelspace = normalize(posModel.xyz);
    float product = dot(unitSpacecraftDir, positionModelspace);

    if (planePosition.z > rawPos.z) {
        discard;
        //outColor = vec4(1.0, 0.0, 0.0, 0.0);
    } else {
        vec2 uv = rawPos.xy;
        uv /= (FULL_PLANE_SIZE * 2);
        uv += 0.5;
        float intensityOrg = float(texture(imageryTexture, uv).r);
        outColor =  texture(lut, intensityOrg);
    }

    Fragment frag;
    frag.color = outColor;
    frag.depth = vs_positionScreenSpace.w;

    return frag;
}
