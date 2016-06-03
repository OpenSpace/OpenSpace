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

#include "fragment.glsl"
#include <#{fragmentPath}>
#include "abufferfragment.glsl"
#include "abufferresources.glsl"
#include "PowerScaling/powerScalingMath.hglsl"
#include "rand.glsl"
#include "resolveconstants.glsl"

#include "resolvehelpers.glsl"

#define RAYCASTING_ENABLED #{resolveData.raycastingEnabled}
uniform float blackoutFactor;
uniform sampler2DMS mainColorTexture;
uniform sampler2DMS mainDepthTexture;

out vec4 _out_color_;

void main() {

    Fragment newFrag = getFragment();
    int sampleMask = gl_SampleMaskIn[0];
    
    if (newFrag.depth < 0) {
        discard;
    }

    float fboDepth = denormalizeFloat(texelFetch(mainDepthTexture, ivec2(gl_FragCoord), 0).x);
    vec4 fboRgba = texelFetch(mainColorTexture, ivec2(gl_FragCoord), 0);
    
    if (newFrag.depth > fboDepth) {
        discard;
    }

    //newFrag.color *= countSamples(sampleMask) / nAaSamples;
/*
    vec3 accumulatedColor = vec3(0.0);
    
    // One alpha channel per color channel to allow for
    // absorption of different wavelengths.
    // Always within the interval [0, 1]
    vec3 accumulatedAlpha = vec3(0.0); 

    
    uint nFrags = loadFragments();
    uint raycasterMask;
#if RAYCASTING_ENABLED    
    bool insideAnyRaycaster = initRaycasterMask(raycasterMask);
#endif    

    
#if RAYCASTING_ENABLED
    retrieveRaycasterData(nFrags);

    if (insideAnyRaycaster) {
        //raycast to the first fragment
        float startDepth = 0;
        float endDepth = min(_depth_(fragments[0]), newFrag.depth);
        raycast(endDepth - startDepth, raycasterMask, accumulatedColor, accumulatedAlpha);
    }
#endif

    for (uint i = 0; i < nFrags; i++) {
        ABufferFragment frag = fragments[i];

        if (_depth_(frag) > newFrag.depth) {
            break;
        }

        int type = _type_(frag);
        uint blend = _blend_(frag);

        if (type == 0) { // geometry fragment
            vec4 color = _color_(frag);
            if (blend == BLEND_MODE_NORMAL) {
                accumulatedColor += (1 - accumulatedAlpha) * color.rgb * color.a;
                accumulatedAlpha += (1 - accumulatedAlpha) * color.aaa;                
            } else if (blend == BLEND_MODE_ADDITIVE) {
                accumulatedColor += (1 - accumulatedAlpha) * color.rgb;
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
            float endDepth = min(_depth_(fragments[i + 1]), newFrag.depth);            
            raycast(endDepth - startDepth, raycasterMask, accumulatedColor, accumulatedAlpha);
        }
#endif
    }
*/
    vec4 newColor = newFrag.color;
    vec3 contribution =  newColor.rgb * blackoutFactor;
    //vec3 contribution = newColor.rgb * blackoutFactor;    


    
    _out_color_ = vec4(contribution, 1.0);
    //    _out_color_ = vec4(1.0);

}

