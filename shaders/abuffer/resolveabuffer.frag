/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014 - 2016                                                             *
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

#include "abufferfragment.glsl"
#include "abufferresources.glsl"
#include "fragment.glsl"
#include "PowerScaling/powerScalingMath.hglsl"
#include "blending.glsl"
#include "rand.glsl"

layout (location = 0) out vec4 finalColor;

uniform float blackoutFactor;
uniform int nAaSamples;
uniform sampler2DMS mainColorTexture;
uniform sampler2DMS mainDepthTexture;

#define RAYCASTING_ENABLED #{raycastingEnabled}
#define N_RAYCASTERS #{nRaycasters}
#define ALPHA_LIMIT 0.99
#define RAYCAST_MAX_STEPS 10000
#define INT_MAX 2147483647

/////////////////////////
#if RAYCASTING_ENABLED

#include "raycasterdata.glsl"

RaycasterData raycasterData[N_RAYCASTERS];
// Include all ray caster helpers
#for id, helperPath in helperPaths
#include <#{helperPath}>
#endfor

// Include all ray casters
#for id, raycaster in raycasters
#include <#{raycaster.raycastPath}>
#endfor

#endif
/////////////////////////

void sortFragments(uint nFrags) {
    ABufferFragment tmp;
    uint i, j;

    // Insertion sort
    for(i = 1; i < nFrags; ++i) {
        tmp = fragments[i];
        for(j = i; j > 0 && _depth_(tmp) < _depth_(fragments[j-1]); --j) {
            fragments[j] = fragments[j-1];
        }
        fragments[j] = tmp;
    }
}

uint countSamples(uint mask) {
    return ((mask >> 0) & 1)
        + ((mask >> 1) & 1)
        + ((mask >> 2) & 1)
        + ((mask >> 3) & 1)
        + ((mask >> 4) & 1)
        + ((mask >> 5) & 1)
        + ((mask >> 6) & 1)
        + ((mask >> 7) & 1);
}

uint depthFilterFragments(uint nFrags, float depthThreshold) {
    uint j = 0;
    for (uint i = 0; i < nFrags; i++) {
        if (_depth_(fragments[i]) < depthThreshold) {
            fragments[j] = fragments[i];
            j++;
        }
    }
    return j;
}


uint mergeFragments(uint nFrags) {
    uint outputIndex = 0;
    for (uint inputIndex = 0; inputIndex < nFrags; inputIndex++, outputIndex++) {

        ABufferFragment frag = fragments[inputIndex];
        uint accumulatedMask = _msaa_(fragments[inputIndex]);
        uint newMask = _msaa_(fragments[inputIndex]);
        int type = _type_(fragments[inputIndex]);

        // Accumulate sample mask
        for (uint j = inputIndex + 1;
             j < nFrags && ((newMask = _msaa_(fragments[j])) & accumulatedMask) == 0 && _type_(fragments[j]) == type;
             j++) {
            accumulatedMask |= newMask;
            inputIndex = j;
        }
        uint nSamples = countSamples(accumulatedMask);
        vec4 color = _color_(fragments[inputIndex]); // TODO: Possibly weigh all samples together?

        // Adjust the alpha by the ratio of accumulated samples
        float alpha = float(nSamples) / float(nAaSamples);
        color.a *= alpha;

        ABufferFragment outputFragment = fragments[inputIndex];
        _color_(outputFragment, color);

        fragments[outputIndex] = outputFragment;
    }

    // return number of outputted fragments
    return outputIndex;
}

#if RAYCASTING_ENABLED

/**
 * Iterate through list of sorted fragments,
 * and retrieve raycasting position, direction, scale
 */
void retrieveRaycasterData(uint nFrags) {
    float entryDepths[N_RAYCASTERS];
    for (int i = 0; i < N_RAYCASTERS; i++) {
        entryDepths[i] = -1;
    }
    for (int i = 0; i < nFrags; i++) {
        int type = _type_(fragments[i]); // - 1;
        vec3 position = _position_(fragments[i]);
        float depth = _depth_(fragments[i]);
        uint blend = _blend_(fragments[i]);
        if (type > 0) { // enter raycaster
            int raycasterId = type - 1;
            if (entryDepths[raycasterId] < 0) { // first entry
                raycasterData[raycasterId].position = position;
                raycasterData[raycasterId].previousJitterDistance = 0;
                raycasterData[raycasterId].blend = blend;
                entryDepths[raycasterId] = depth;
                raycasterData[raycasterId].scale = -1;
            }
        } else if (type < 0) { // exit raycaster
            int raycasterId = -type - 1;
            vec3 localDirection = position - raycasterData[raycasterId].position;
            raycasterData[raycasterId].direction = safeNormalize(localDirection);
            raycasterData[raycasterId].scale = safeLength(localDirection) / (depth - entryDepths[raycasterId]);
        }
    }
}

/**
 * Perform raycasting
 */
void raycast(float raycastDepth, uint raycasterMask, inout vec3 accumulatedColor, inout vec3 accumulatedAlpha) {
    float nextStepSize = raycastDepth;
    float currentStepSize = 0.0;
    float jitterFactor = 0.5 + 0.5 * rand(gl_FragCoord.xy); // should be between 0.5 and 1.0

#for index, raycaster in raycasters
    if ((raycasterMask & #{raycaster.bitmask}) != 0) {
        RaycasterData data = raycasterData[#{index}];
        float maxStepSizeLocal = stepSize#{raycaster.id}(data.position, data.direction);
        float maxStepSize = maxStepSizeLocal / data.scale;
        nextStepSize = min(nextStepSize, maxStepSize);
    }
#endfor

    float currentDepth = 0.0;

    for (int steps = 0;
         (accumulatedAlpha.x < ALPHA_LIMIT ||
          accumulatedAlpha.y < ALPHA_LIMIT ||
          accumulatedAlpha.z < ALPHA_LIMIT) &&
         steps < RAYCAST_MAX_STEPS;
         ++steps) {
        bool exceededDepth = currentDepth + nextStepSize * jitterFactor > raycastDepth;
        bool shortStepSize = nextStepSize < raycastDepth / 10000000000.0;

        if (exceededDepth || shortStepSize) {
            break;
        }

        currentStepSize = nextStepSize;
        currentDepth += currentStepSize;
        nextStepSize = raycastDepth - currentDepth;

#for index, raycaster in raycasters

        if ((raycasterMask & #{raycaster.bitmask}) != 0) {
            RaycasterData data = raycasterData[#{raycaster.id}];
            float stepSizeLocal = currentStepSize * data.scale;
            float jitteredStepSizeLocal = stepSizeLocal * jitterFactor;

            vec3 jitteredPosition = data.position + data.direction*jitteredStepSizeLocal;
            raycasterData[#{raycaster.id}].position += data.direction * stepSizeLocal;

            float maxStepSizeLocal;

            sample#{raycaster.id}(jitteredPosition,
                                  data.direction,
                                  accumulatedColor,
                                  accumulatedAlpha,
                                  maxStepSizeLocal);
            
            float sampleDistance = jitteredStepSizeLocal + data.previousJitterDistance;
            uint blend = raycasterData[#{raycaster.id}].blend;

            /*
            if (blend == BLEND_MODE_NORMAL) {
                normalBlendStep(finalColor, raycasterContribution, sampleDistance);
            } else if (blend == BLEND_MODE_ADDITIVE) {
                additiveBlendStep(finalColor, raycasterContribution, sampleDistance);
                }*/
            //finalColor = raycasterContribution;

            raycasterData[#{raycaster.id}].previousJitterDistance = stepSizeLocal - jitteredStepSizeLocal;
            float maxStepSize = maxStepSizeLocal/data.scale;
            nextStepSize = min(nextStepSize, maxStepSize);
        }
#endfor
    }
}
#endif // RAYCASTING_ENABLED

void main() {
    // TODO: disable multisampling for main fbo.
    float fboDepth = denormalizeFloat(texelFetch(mainDepthTexture, ivec2(gl_FragCoord), 0).x);
    vec4 fboRgba = texelFetch(mainColorTexture, ivec2(gl_FragCoord), 0);

    // RGB color values, premultiplied with alpha channels.
    vec3 accumulatedColor = vec3(0.0); 
    // One alpha channel per color channel to allow for
    // absorption of different wavelengths.
    // Always within the interval [0, 1]
    vec3 accumulatedAlpha = vec3(0.0); 

    uint nOriginalFrags = loadFragments();
    uint raycasterMask = 0;

    uint nFilteredFrags = nOriginalFrags;

    // discard all fragments in abuffer with higher depth value than the fbo
    nFilteredFrags = depthFilterFragments(nOriginalFrags, fboDepth);

    // sort remaining fragments from front to back
    sortFragments(nFilteredFrags);

    // merge fragments whose sample masks don't intersect
    // to get the correct alpha for fragments on borders between triangles
    uint nFrags = mergeFragments(nFilteredFrags);
    
#if RAYCASTING_ENABLED
    retrieveRaycasterData(nFrags);
#endif

    for (uint i = 0; i < nFrags; i++) {
        ABufferFragment frag = fragments[i];

        int type = _type_(frag);
        uint blend = _blend_(frag);

        if (type == 0) { // geometry fragment
            vec4 color = _color_(frag);
            if (blend == BLEND_MODE_NORMAL) {
                accumulatedColor += (1 - accumulatedAlpha) * color.rgb * color.a;
                accumulatedAlpha += (1 - accumulatedAlpha) * color.aaa;                
                //normalBlend(finalColor, color);
            } else if (blend == BLEND_MODE_ADDITIVE) {
                accumulatedColor += (1 - accumulatedAlpha) * color.rgb;
                //additiveBlend(finalColor, color);
            }
        }
#if RAYCASTING_ENABLED
        else if (type > 0) { // enter volume
            int raycasterId = type - 1;
            // only enter volume if a valid scale was detected
            if (raycasterData[raycasterId].scale > 0) {
                raycasterMask |= (1 << (raycasterId));
            }
        } else { // exit volume
            int raycasterId = -type - 1;
            raycasterMask &= INT_MAX - (1 << (raycasterId));
        }
        // Ray cast to next fragment
        if (i + 1 < nFrags && raycasterMask != 0) {
            float startDepth = _depth_(fragments[i]);
            float endDepth = _depth_(fragments[i + 1]);
            raycast(endDepth - startDepth, raycasterMask, accumulatedColor, accumulatedAlpha);
        }
#endif
    }
    
    // Always blend in fbo content behind a buffer data.
    accumulatedColor += (1 - accumulatedAlpha) * fboRgba.rgb;
    //accumulatedAlpha += (1 - accumulatedAlpha) * fboRgba.aaa;                
    
    
    finalColor = vec4(accumulatedColor.rgb * blackoutFactor, 1.0);
    

    // Gamma correction.
    finalColor.rgb = pow(finalColor.rgb, vec3(1.0 / 2.2));//  sqrt(finalColor.rgb);
}
