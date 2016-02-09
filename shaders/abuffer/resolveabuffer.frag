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

layout (location = 0) out vec4 finalColor;

uniform float blackoutFactor;
uniform int nAaSamples;

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

vec4 blend(vec4 front, vec4 back) {
    vec4 result;
    result.a = front.a + (1.0 - front.a) * back.a;
    result.rgb = ((front.rgb * front.a) + (back.rgb * back.a * (1.0 - front.a))) / result.a;
    result = clamp(result, 0.0, 1.0);
    return result;
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


void main() {
    finalColor = vec4(0.0);
    uint nFrags = loadFragments();
    
    sortFragments(nFrags);

    int realFrags = 0;

    for (uint i = 0; i < nFrags; i++) {
        ABufferFragment frag = fragments[i];

        uint accumulatedMask = _msaa_(fragments[i]);
        uint newMask = _msaa_(fragments[i]);

        vec4 color = vec4(0.0);
        float totalAlpha = 0.0;
        
        for (uint j = i + 1;
             j < nFrags
                 && ((newMask = _msaa_(fragments[j])) & accumulatedMask) == 0;
             j++) {

            accumulatedMask |= newMask;
            i = j;
        }

        uint nSamples = countSamples(accumulatedMask);
        color = _color_(fragments[i]); // TODO: Possibly weigh all samples together?
        color.a *= float(nSamples) / float(nAaSamples);

        finalColor = blend(finalColor, color);
    }

    finalColor.a *= blackoutFactor;
}
