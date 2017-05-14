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

in vec2 vs_st;
in vec4 vs_positionScreenSpace;
in vec4 clipSpace;
in vec3 vUv;

uniform dvec3 planePositionSpacecraft[2];
uniform sampler2D imageryTexture[2];
uniform sampler1D lut[2];
uniform sampler2D magnetogram;

// TODO(mnoven): Uniform
const float FULL_PLANE_SIZE = (1391600000 * 0.5) / 0.785; // / 0.61877
// TODO(mnoven): Metadata
const float magnetogramMin = -2265.132812;
const float magnetogramMax = 2417.483887;

#include "fragment.glsl"

Fragment getFragment() {
    vec4 outColor;
    float intensity = texture(magnetogram, vs_st).r;

    intensity = (intensity - magnetogramMin) / (magnetogramMax - magnetogramMin);
    outColor = vec4(intensity, intensity, intensity, 1.0);

    // if (planePositionSpacecraft[0].z > vUv.z) {
    //     //discard;
    //     outColor = vec4(intensity, intensity, intensity, 1.0);
    // } else {
    //     vec2 uv = vUv.xy;
    //     uv /= (FULL_PLANE_SIZE * 2);
    //     uv += 0.5;
    //     float intensityOrg = float(texture(imageryTexture[0], vec2(uv.x  /* +  ( (1024.0 - 1021.81 ) / 2048 ) */, uv.y /*  +    ((1024.0 - 926.171) / 2048) */)).r);
    //     outColor = texture(lut[0], intensityOrg);
    // }

    Fragment frag;
    frag.color = outColor;
    frag.depth = vs_positionScreenSpace.w;

    return frag;
}
