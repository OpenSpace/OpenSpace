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

#ifndef _ABUFFERRESOURCES_GLSL_
#define _ABUFFERRESOURCES_GLSL_

#include "abufferfragment.glsl"
#define MAX_LAYERS #{rendererData.maxLayers}

ABufferFragment fragments[MAX_LAYERS];
uint fragmentIndices[MAX_LAYERS];

layout (binding = 0, r32ui) uniform uimage2D anchorPointerTexture;
layout (binding = 1, rgba32ui) uniform uimageBuffer fragmentTexture;
layout (binding = 0, offset = 0) uniform atomic_uint atomicCounterBuffer;

const uint NULL_POINTER = 0;

void storeFragment(uint index, ABufferFragment aBufferFrag) {
    imageStore(fragmentTexture, int(index), _raw_(aBufferFrag));                
}

ABufferFragment loadFragment(uint index) {
    uvec4 raw = imageLoad(fragmentTexture, int(index));
    ABufferFragment aBufferFragment;
    _raw_(aBufferFragment, raw);
    return aBufferFragment;
}

/**
 * Load fragments into the #fragments array.
 */ 
uint loadFragments() {
    uint currentIndex = imageLoad(anchorPointerTexture, ivec2(gl_FragCoord.xy)).x;
    int nFrags = 0;
    while (currentIndex != NULL_POINTER && nFrags < MAX_LAYERS) { 
        ABufferFragment frag = loadFragment(currentIndex);
        fragments[nFrags] = frag;
        fragmentIndices[nFrags] = currentIndex;
        currentIndex = _next_(frag);
        nFrags++;
    }
    return nFrags;
}

/**
 * Store the current contents of the fragments array back into the abuffer.
 */
void storeFragments(uint nFrags) {
    if (nFrags == 0)
        return;
    uint maxFragIndex = nFrags - 1;
    for (int i = 0; i < maxFragIndex; i++) {
        _next_(fragments[i], fragmentIndices[i+1]);
        storeFragment(fragmentIndices[i], fragments[i]);
    }
    _next_(fragments[maxFragIndex], NULL_POINTER);
    storeFragment(fragmentIndices[maxFragIndex], fragments[maxFragIndex]);
            
}




#endif
