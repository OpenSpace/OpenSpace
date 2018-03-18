/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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
const int MAX_SPACECRAFT_OBSERVATORY = 7;

in vec4 vs_positionScreenSpace;
in vec4 clipSpace;
in vec3 vUv[MAX_SPACECRAFT_OBSERVATORY];

uniform int numSpacecraftCameraPlanes;
uniform dvec3 planePositionSpacecraft[MAX_SPACECRAFT_OBSERVATORY];
uniform sampler1D lut[MAX_SPACECRAFT_OBSERVATORY];
uniform sampler2D imageryTexture[MAX_SPACECRAFT_OBSERVATORY];
uniform bool hasLut[MAX_SPACECRAFT_OBSERVATORY];
uniform float contrastValue[MAX_SPACECRAFT_OBSERVATORY];
uniform float opacityValue[MAX_SPACECRAFT_OBSERVATORY];
uniform float gammaValue[MAX_SPACECRAFT_OBSERVATORY];
uniform float imageSize[MAX_SPACECRAFT_OBSERVATORY];
uniform bool isEnabled[MAX_SPACECRAFT_OBSERVATORY];
uniform bool isCoronaGraph[MAX_SPACECRAFT_OBSERVATORY];
uniform float scale[MAX_SPACECRAFT_OBSERVATORY];
uniform vec2 centerPixel[MAX_SPACECRAFT_OBSERVATORY];

const float SUN_RADIUS = 1391600000 * 0.5;

#include "fragment.glsl"

float contrast(float intensity, int i) {
    return min(clamp(0.5 + (intensity - 0.5) * (1 + contrastValue[i]/10.0), 0.0, 1.0), sqrt(intensity) + intensity);
}

Fragment getFragment() {
    vec4 outColor = vec4(0);
    bool renderSurface = true;

    for (int i = 0; i < numSpacecraftCameraPlanes; i++) {
        if (isCoronaGraph[i] || !isEnabled[i]) {
            continue;
        }

        if (planePositionSpacecraft[i].z < vUv[i].z) {
            vec3 uv = vUv[i].xyz;
            uv /= ( (SUN_RADIUS / scale[i]) * 2);
            uv += 0.5;

            uv.x += ((centerPixel[i].x) / SUN_RADIUS) / 2.0;
            uv.y -= ((centerPixel[i].y) /  SUN_RADIUS) / 2.0;

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

            // Not initialized
            if (outColor == vec4(0)) {
                float factor2 = smoothstep(0.5, uv.x, uv.z);
                outColor = mix(res, res, factor2);
            } else {
                // Blend between
                float factor = smoothstep(0.5, 1.0 - uv.x, uv.z);
                float factor2 = smoothstep(0.5, uv.x, uv.z);
                outColor = mix(outColor, res, factor + factor2);
            }
            renderSurface = false;
        }
    }

    if (renderSurface) {
        // Yellow-ish. Could discard to get the standard sun texture
        outColor = vec4(0.93, 0.96, 0.3, 1.0);
    }

    Fragment frag;
    frag.color = outColor;
    frag.depth = vs_positionScreenSpace.w;

    return frag;
}