/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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

#version __CONTEXT__

uniform sampler2D exitColorTexture;
uniform sampler2D exitDepthTexture;
uniform sampler2D mainDepthTexture;

uniform vec3 cameraPosInRaycaster;
uniform vec2 windowSize;

uniform int rayCastSteps;

#include "blending.glsl"
#include "rand.glsl"
#include "floatoperations.glsl"

#for id, helperPath in helperPaths
#include <#{helperPath}>
#endfor

#include <#{raycastPath}>

out vec4 finalColor;

#define ALPHA_LIMIT 0.99

#include <#{getEntryPath}>

void main() {
    vec2 texCoord = vec2(gl_FragCoord.xy / windowSize);

    // Boundary position in view space
    vec4 exitColorTexture = texture(exitColorTexture, texCoord);

    // If we don't have an exit, discard the ray
    if (exitColorTexture.a < 1.f || exitColorTexture.rgb == vec3(0.f)) {
        discard;
    }

    // Fetch exit point from texture
    vec3 exitPos = exitColorTexture.rgb;
    float exitDepth =  denormalizeFloat(texture(exitDepthTexture, texCoord).x);

    float jitterFactor = 0.5f + 0.5f * rand(gl_FragCoord.xy); // should be between 0.5 and 1.0

    vec3 entryPos;
    float entryDepth;
    getEntry(entryPos, entryDepth);
    // If we don't have an entry, discard the ray
    if (entryPos == vec3(0.f)) {
        discard;
    }

    vec3 position = entryPos;
    vec3 diff = exitPos - entryPos;

    vec3 direction = normalize(diff);
    float raycastDepth = length(diff);

    float geoDepth = denormalizeFloat((texture(mainDepthTexture, texCoord).x));
    float geoRatio = clamp((geoDepth - entryDepth) / (exitDepth - entryDepth), 0.f, 1.f);
    raycastDepth = geoRatio * raycastDepth;

    float currentDepth = 0.f;
    float nextStepSize = stepSize#{id}(position, direction);
    float currentStepSize;
    float previousJitterDistance = 0.f;

    int nSteps = 0;

    int sampleIndex = 0;
    float opacityDecay = 1.f;

    vec3 accumulatedColor = vec3(0.f);
    vec3 accumulatedAlpha = vec3(0.f);


    for (nSteps = 0; 
        (accumulatedAlpha.r < ALPHA_LIMIT || accumulatedAlpha.g < ALPHA_LIMIT || 
         accumulatedAlpha.b < ALPHA_LIMIT) && nSteps < rayCastSteps; 
         ++nSteps) 
    {
        if (nextStepSize < raycastDepth / 10000000000.f) {
            break;
        }

        currentStepSize = nextStepSize;
        currentDepth += currentStepSize;

        float jitteredStepSize = currentStepSize * jitterFactor;
        vec3 jitteredPosition = position + direction * jitteredStepSize;
        position += direction * currentStepSize;

        sample#{id}(jitteredPosition, direction, accumulatedColor, accumulatedAlpha, nextStepSize);
        float sampleDistance = jitteredStepSize + previousJitterDistance;

        previousJitterDistance = currentStepSize - jitteredStepSize;

        float maxStepSize = raycastDepth - currentDepth;
        nextStepSize = min(nextStepSize, maxStepSize);
    }

    finalColor = vec4(accumulatedColor, (accumulatedAlpha.r + accumulatedAlpha.g + accumulatedAlpha.b) / 3.f);

    finalColor.rgb /= finalColor.a ;

    gl_FragDepth = normalizeFloat(entryDepth);
}
