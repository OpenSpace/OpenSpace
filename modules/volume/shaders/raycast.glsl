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

uniform float maxStepSize#{id} = 0.02;
uniform sampler3D volumeTexture_#{id};
uniform sampler1D transferFunction_#{id};
uniform int gridType_#{id} = 0;

uniform int nClips_#{id};
uniform vec3 clipNormals_#{id}[8];
uniform vec2 clipOffsets_#{id}[8];

uniform float opacity_#{id} = 10.0;


// Normalization factor x for radius r [0, 1].
// value *= 1/(r^x)
// only working for volumes given in spherical coordianates.
// Applied after any linear value remapping.
uniform float rNormalization_#{id} = 0.0;

uniform float rUpperBound_#{id} = 1.0;

void sample#{id}(vec3 samplePos, vec3 dir, inout vec3 accumulatedColor,
                 inout vec3 accumulatedAlpha, inout float stepSize)
{

    vec3 transformedPos = samplePos;
    if (gridType_#{id} == 1) {
        transformedPos = volume_cartesianToSpherical(samplePos);
        if (abs(transformedPos.r) > 1.0) {
           return;
        }
    }

    float clipAlpha = 1.0;
    vec3 centerToPos = transformedPos - vec3(0.5);

    for (int i = 0; i < nClips_#{id} && i < 8; i++) {
        vec3 clipNormal = clipNormals_#{id}[i];
        float clipBegin = clipOffsets_#{id}[i].x;
        float clipEnd = clipBegin + clipOffsets_#{id}[i].y;
        clipAlpha *= smoothstep(clipBegin, clipEnd, dot(centerToPos, clipNormal));
    }

    clipAlpha *= 1.0 - smoothstep(rUpperBound_#{id} - 0.01, rUpperBound_#{id} + 0.01, transformedPos.x);

    if (clipAlpha > 0) {
        float val = texture(volumeTexture_#{id}, transformedPos).r;

        if (rNormalization_#{id} > 0 && gridType_#{id} == 1) {
            val *= pow(transformedPos.x, rNormalization_#{id});
        }

        vec4 color = texture(transferFunction_#{id}, val);

        vec3 backColor = color.rgb;
        vec3 backAlpha = color.aaa;

        backColor *= stepSize*opacity_#{id} * clipAlpha;
        backAlpha *= stepSize*opacity_#{id} * clipAlpha;

        backColor = clamp(backColor, 0.0, 1.0);
        backAlpha = clamp(backAlpha, 0.0, 1.0);

        vec3 oneMinusFrontAlpha = vec3(1.0) - accumulatedAlpha;
        accumulatedColor += oneMinusFrontAlpha * backColor;
        accumulatedAlpha += oneMinusFrontAlpha * backAlpha;
    }

    stepSize = maxStepSize#{id};
}

float stepSize#{id}(vec3 samplePos, vec3 dir) {
    return maxStepSize#{id};
}
