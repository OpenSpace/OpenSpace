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
const int MAX_SPACECRAFT_OBSERVATORY = 6;

in vec2 vs_st;
in vec4 vs_positionScreenSpace;
in vec4 clipSpace;
in vec3 vUv[MAX_SPACECRAFT_OBSERVATORY];

uniform dvec3 planePositionSpacecraft[MAX_SPACECRAFT_OBSERVATORY];
uniform sampler1D lut[MAX_SPACECRAFT_OBSERVATORY];
uniform sampler2D imageryTexture[MAX_SPACECRAFT_OBSERVATORY];
uniform sampler2D magnetogram;
uniform int numSpacecraftCameraPlanes;

// TODO(mnoven): Uniform
const float FULL_PLANE_SIZE = (1391600000 * 0.5) / 0.785; // / 0.61877
const float FULL_PLANE_SIZE_STEREO = (1391600000 * 0.5) / 0.61877;
// TODO(mnoven): Metadata
const float magnetogramMin = -2265.132812;
const float magnetogramMax = 2417.483887;

#include "fragment.glsl"

Fragment getFragment() {
    vec4 outColor;

    bool renderMagnetoGram = true;
    // //while (c < 2){
    // for (int i = 0; i < numSpacecraftCameraPlanes; ++i) {
    //     if (planePositionSpacecraft[i].z < vUv[i].z) {
    //         vec2 uv = vUv[i].xy;
    //         uv /= (FULL_PLANE_SIZE * 2);
    //         uv += 0.5;
    //         float intensityOrg = texture(imageryTexture[i], vec2(uv.x  /* +  ( (1024.0 - 1021.81 ) / 2048 ) */, uv.y /*  +    ((1024.0 - 926.171) / 2048) */)).r;
    //         outColor = texture(lut[i], intensityOrg);
    //         //outColor = vec4(intensityOrg,intensityOrg,intensityOrg, 1.0 );
    //         renderMagnetoGram = false;
    //     }
    // }

    if (planePositionSpacecraft[0].z < vUv[0].z && planePositionSpacecraft[1].z < vUv[1].z) {
        vec2 uv1 = vUv[0].xy;
        uv1 /= (FULL_PLANE_SIZE * 2);
        uv1 += 0.5;
        float intensityOrg1 = texture(imageryTexture[0], vec2(uv1.x, uv1.y)).r;
        vec4 outColor1 = texture(lut[0], intensityOrg1);

        vec2 uv2 = vUv[1].xy;
        uv2 /= (FULL_PLANE_SIZE_STEREO * 2);
        uv2 += 0.5;
        float intensityOrg2 = texture(imageryTexture[1], vec2(uv2.x  +  ( (1024.0 - 1021.81 ) / 2048 ), uv2.y +  ((1024.0 - 926.171) / 2048))).r;
        vec4 outColor2 = texture(lut[1], intensityOrg2);

        outColor = mix(outColor1, outColor2, 0.5);
    } else if (planePositionSpacecraft[0].z < vUv[0].z) {
        vec2 uv = vUv[0].xy;
        uv /= (FULL_PLANE_SIZE * 2);
        uv += 0.5;
        float intensityOrg = texture(imageryTexture[0], vec2(uv.x, uv.y)).r;
        outColor = texture(lut[0], intensityOrg);
        //outColor = vec4(intensityOrg,intensityOrg,intensityOrg, 1.0 );
    } else if (planePositionSpacecraft[1].z < vUv[1].z) {
        vec2 uv = vUv[1].xy;
        uv /= (FULL_PLANE_SIZE_STEREO * 2);
        uv += 0.5;
        float intensityOrg = texture(imageryTexture[1], vec2(uv.x  +  ( (1024.0 - 1021.81 ) / 2048 ), uv.y + ((1024.0 - 926.171) / 2048))).r;
        outColor = texture(lut[1], intensityOrg);
    } else {
        float intensity = texture(magnetogram, vs_st).r;
        intensity = (intensity - magnetogramMin) / (magnetogramMax - magnetogramMin);
        outColor = vec4(intensity, intensity, intensity, 1.0);
    }

    Fragment frag;
    frag.color = outColor;
    frag.depth = vs_positionScreenSpace.w;

    return frag;
}
