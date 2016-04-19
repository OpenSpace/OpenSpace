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

out vec4 _out_color_;

void main() {
    Fragment frag = getFragment();

    int sampleMask = gl_SampleMaskIn[0];

    uint newHead = atomicCounterIncrement(atomicCounterBuffer);
    if (newHead >= #{rendererData.maxTotalFragments}) {
        discard; // ABuffer is full!
    }
    uint prevHead = imageAtomicExchange(anchorPointerTexture, ivec2(gl_FragCoord.xy), newHead);
        
    ABufferFragment aBufferFrag;
    _color_(aBufferFrag, frag.color);
    _depth_(aBufferFrag, frag.depth);
    _blend_(aBufferFrag, frag.blend);

    _type_(aBufferFrag, 0); // 0 = geometry type
    _msaa_(aBufferFrag, gl_SampleMaskIn[0]);
    _next_(aBufferFrag, prevHead);

    storeFragment(newHead, aBufferFrag);
    discard;
}
