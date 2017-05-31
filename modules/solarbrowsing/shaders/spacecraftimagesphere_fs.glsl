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
uniform bool hasLut[MAX_SPACECRAFT_OBSERVATORY];

uniform float sharpenValue[MAX_SPACECRAFT_OBSERVATORY];
uniform float contrastValue[MAX_SPACECRAFT_OBSERVATORY];
uniform float opacityValue[MAX_SPACECRAFT_OBSERVATORY];
uniform float gammaValue[MAX_SPACECRAFT_OBSERVATORY];
uniform float imageSize[MAX_SPACECRAFT_OBSERVATORY];

uniform dvec2 magicPlaneOffset[MAX_SPACECRAFT_OBSERVATORY];
uniform float magicPlaneFactor[MAX_SPACECRAFT_OBSERVATORY];

//uniform float scale[MAX_SPACECRAFT_OBSERVATORY];
uniform vec2 centerPixel[MAX_SPACECRAFT_OBSERVATORY];

const float HALF_SUN_RADIUS = (1391600000 * 0.5);

// TODO(mnoven): Uniform
//const float FULL_PLANE_SIZE = (1391600000 * 0.5) / magicPlaneFactor[0];
//const float FULL_PLANE_SIZE_STEREO = (1391600000 * 0.5) / magicPlaneFactor[1];
// TODO(mnoven): Metadata
const float magnetogramMin = -2265.132812;
const float magnetogramMax = 2417.483887;

#include "fragment.glsl"

float contrast(float intensity, int i) {
    return min(clamp(0.5 + (intensity - 0.5) * (1 + contrastValue[i]/10.0), 0.0, 1.0), sqrt(intensity) + intensity);
}

Fragment getFragment() {
    vec4 outColor = vec4(0);
    bool renderMagnetoGram = true;

    for (int i = 0; i < numSpacecraftCameraPlanes; i++) {
        if (planePositionSpacecraft[i].z < vUv[i].z) {
            vec2 uv = vUv[i].xy;
            uv /= ( (HALF_SUN_RADIUS / magicPlaneFactor[i]) * 2);
            uv += 0.5;

            uv.x += ((centerPixel[i].x) / HALF_SUN_RADIUS) / 2.0;
            uv.y -= ((centerPixel[i].y) /  HALF_SUN_RADIUS) / 2.0;

            float intensityOrg = texture(imageryTexture[i], vec2(uv.x, 1.0 - uv.y)).r;
            intensityOrg = contrast(intensityOrg, i);

            vec4 res;
            if (hasLut[i]) {
                res = texture(lut[i], intensityOrg);
            } else {
                res = vec4(intensityOrg, intensityOrg, intensityOrg, 1.0);
            }

            res.r = pow(res.r, gammaValue[i]);
            res.g = pow(res.g, gammaValue[i]);
            res.b = pow(res.b, gammaValue[i]);

            if (outColor == vec4(0)) {
                outColor = res;
            } else {
                outColor = mix(outColor, res, 0.5);
            }
            renderMagnetoGram = false;
        }
    }

    if (renderMagnetoGram) {
        float intensity = texture(magnetogram, vs_st).r;
        intensity = (intensity - magnetogramMin) / (magnetogramMax - magnetogramMin);
        outColor = vec4(intensity, intensity, intensity, 1.0);
    }

    Fragment frag;
    frag.color = outColor;
    frag.depth = vs_positionScreenSpace.w;

    return frag;
}
