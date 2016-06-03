/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2016                                                                    *
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
uniform sampler2DMS mainDepthTexture;

#include "blending.glsl"
#include "rand.glsl"
#include "PowerScaling/powerScalingMath.hglsl"
#include <#{fragmentPath}>

#for id, helperPath in helperPaths
#include <#{helperPath}>
#endfor

#include <#{raycastPath}>

out vec4 finalColor;


#define ALPHA_LIMIT 0.99
#define RAYCAST_MAX_STEPS 1000
#define MAX_AA_SAMPLES 8

uniform int nAaSamples;


void main() {

    vec2 texCoord = vec2(gl_FragCoord.x / #{rendererData.windowWidth},
                         gl_FragCoord.y / #{rendererData.windowHeight});


    vec4 exitColorTexture = texture(exitColorTexture, texCoord);
    if (exitColorTexture.a < 1.0) {
        discard;
    }

    // fetch exit point from texture
    vec3 exitPos = exitColorTexture.rgb;
    float exitDepth =  denormalizeFloat(texture(exitDepthTexture, texCoord).x);



    float jitterFactor = 0.5 + 0.5 * rand(gl_FragCoord.xy); // should be between 0.5 and 1.0

    // fetch entry point from rendered fragment
    Fragment f = getFragment();
    vec3 entryPos = f.color.xyz;
    float entryDepth = f.depth;

    vec3 position = entryPos;
    vec3 diff = exitPos - entryPos;

    vec3 direction = normalize(diff);
    float raycastDepth = length(diff);

    float raycastDepths[MAX_AA_SAMPLES];

    int i, j;
    float tmp;

    for (i = 0; i < nAaSamples; i++) {
        float geoDepth = denormalizeFloat(texelFetch(mainDepthTexture, ivec2(gl_FragCoord), i).x);
        float geoRatio = clamp((geoDepth - entryDepth) / (exitDepth - entryDepth), 0.0, 1.0);
        raycastDepths[i] = geoRatio * raycastDepth;
    }

    for(i = 1; i < nAaSamples; ++i) {
        tmp = raycastDepths[i];
        for(j = i; j > 0 && tmp < raycastDepths[j - 1]; --j) {
            raycastDepths[j] = raycastDepths[j-1];
        }
        raycastDepths[j] = tmp;
    }


    float currentDepth = 0.0;
    // todo: shorten depth if geometry is intersecting!
    float nextStepSize = stepSize#{id}(position, direction);
    float currentStepSize;
    float previousJitterDistance = 0.0;

    int steps = 0;

    float aaOpacity = 1.0;
    int sampleIndex = 0;
    float opacityDecay = 1.0 / nAaSamples;

    vec3 accumulatedColor = vec3(0.0);
    vec3 accumulatedAlpha = vec3(0.0);
        
    
    for (steps = 0; (accumulatedAlpha.r < ALPHA_LIMIT || accumulatedAlpha.g < ALPHA_LIMIT || accumulatedAlpha.b < ALPHA_LIMIT) && steps < RAYCAST_MAX_STEPS; ++steps) {

        
        while (sampleIndex < nAaSamples && currentDepth + nextStepSize * jitterFactor > raycastDepths[sampleIndex]) {
            sampleIndex++;
            aaOpacity -= opacityDecay;
        }
        bool shortStepSize = nextStepSize < raycastDepth / 10000000000.0;

        if (sampleIndex >= nAaSamples || shortStepSize) {
            break;
        }

        currentStepSize = nextStepSize;
        currentDepth += currentStepSize;

        float jitteredStepSize = currentStepSize * jitterFactor;
        vec3 jitteredPosition = position + direction*jitteredStepSize;
        position += direction * currentStepSize;

        
        sample#{id}(jitteredPosition, direction, accumulatedColor, accumulatedAlpha, nextStepSize);

        float sampleDistance = aaOpacity * (jitteredStepSize + previousJitterDistance);

        //blendStep(finalColor, raycasterContribution, sampleDistance);
        //finalColor 

        previousJitterDistance = currentStepSize - jitteredStepSize;
        
        float maxStepSize = raycastDepths[nAaSamples - 1] - currentDepth;

        nextStepSize = min(nextStepSize, maxStepSize);

    }

    finalColor = vec4(accumulatedColor, (accumulatedAlpha.r + accumulatedAlpha.g + accumulatedAlpha.b) / 3);
    
    finalColor.rgb /= finalColor.a;
    gl_FragDepth = normalizeFloat(entryDepth);
}
