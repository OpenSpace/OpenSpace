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


#include "resolveconstants.glsl"
#include "abufferfragment.glsl"
#include "abufferresources.glsl"
#include "fragment.glsl"
#include "PowerScaling/powerScalingMath.hglsl"
#include "blending.glsl"
#include "rand.glsl"

layout (location = 0) out vec4 finalColor;

uniform float blackoutFactor;
uniform sampler2DMS mainColorTexture;
uniform sampler2DMS mainDepthTexture;
uniform float gamma = 1.0;

#include "resolvehelpers.glsl"



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

    // merge fragments whose sample masks don't igntersect
    // to get the correct alpha for fragments on borders between triangles
    uint nFrags = mergeFragments(nFilteredFrags);

    

    
#if STORE_SORTED    
    //storeFragments(nFrags);
#endif

#if RAYCASTING_ENABLED

    
    retrieveRaycasterData(nFrags);
#if RAYCASTING_ENABLED    
    bool insideAnyRaycaster = initRaycasterMask(raycasterMask);
#endif    
    //debugColor = vec4(raycasterData[0].direction, 1.0);
     
    if (insideAnyRaycaster) {
        //raycast to the first fragment
//        discard;
        float startDepth = 0;
        float endDepth = min(_depth_(fragments[0]), fboDepth);
        raycast(endDepth - startDepth, raycasterMask, accumulatedColor, accumulatedAlpha);
        //accumulatedColor = vec3(1.0);        
    }
#endif
    
    for (uint i = 0; i < nFrags; i++) {
        ABufferFragment frag = fragments[i];

        int type = _type_(frag);
        uint blend = _blend_(frag);

        if (type == 0) { // geometry fragment
            vec4 color = _color_(frag);
            color.rgb = pow(color.rgb, vec3(gamma));
            
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
            //accumulatedColor += (1 - accumulatedAlpha) * _position_(frag);            
            // only enter volume if a valid scale was detected
            if (raycasterData[raycasterId].scale > 0) {
                raycasterMask |= (1 << (raycasterId));
            }
        } else { // exit volume
            int raycasterId = -type - 1;
            raycasterMask &= INT_MAX - (1 << (raycasterId));
            //accumulatedColor = vec3(1.0);
            //accumulatedColor += (1 - accumulatedAlpha) * _position_(frag);
        }
        // Ray cast to next fragment
        if (i + 1 < nFrags && raycasterMask != 0) {
            float startDepth = _depth_(fragments[i]);
            float endDepth = min(_depth_(fragments[i + 1]), fboDepth);
            if (endDepth < startDepth) {
                break;
            }
            
            raycast(endDepth - startDepth, raycasterMask, accumulatedColor, accumulatedAlpha);
            
        }
#endif
    }


    accumulatedAlpha = clamp(accumulatedAlpha, 0.0, 1.0);
    //maccumulatedAlpha = vec3(0.0);
    accumulatedColor += (1 - accumulatedAlpha) * pow(fboRgba.rgb, vec3(gamma));
    
    finalColor = vec4(accumulatedColor.rgb, 1.0);
    
    // Gamma correction.
    finalColor.rgb = pow(finalColor.rgb, vec3(1.0 / gamma));
    // Black out factor.

    
    finalColor = vec4(finalColor.rgb * blackoutFactor, 1.0);

    //finalColor = vec4(vec3(fboRgba.a), 1.0);
    //finalColor = fboRgba;
    //finalColor = vec4(0.0);

    //finalColor = vec4(0.0, acc/1000.0, acc/1000.0, 1.0);
    //finalColor = vec4(acc/1000.0, 0.0, 0.0, 01.0);
    //finalColor = vec4(vec3(float(nFrags) / 10), 1.0);
    //finalColor = vec4(vec3(float(_depth_(fragments[0])) / 10), 1.0);

    
    //finalColor = vec4(vec3(nFilteredFrags - nFrags) * 0.2, 1.0);
    //finalColor = vec4(vec3(nFilteredFrags) * 0.2, 1.0);    
    //finalColor = vec4(vec3(nFrags) * 0.05, 1.0);    
    
    //finalColor = vec4(raycasterData[0].position, 1.0);
    //finalColor = debugColor;
    //finalColor = vec4(gamma * 0.5);
    //finalColor = vec4(fboRgba);
}

