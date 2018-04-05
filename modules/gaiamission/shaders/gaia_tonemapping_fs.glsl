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

#include "fragment.glsl"

in vec2 uv;

uniform sampler2D renderedTexture;

const float DEFAULT_DEPTH = 3.08567758e19; // 1000 Pc

Fragment getFragment() {

    vec4 color = vec4(1.0);
    vec4 intensity = texture( renderedTexture, uv );

    // Tonemap intensity to color!
    //color = vec4(intensity / (intensity + 1.0), 1.0f);
    //color = vec4(0.985663 - 0.9862771 * exp(-4.761438*intensity), 1.0);
    //intensity = 1.0 - 1.0 * exp(-9.0 * intensity);
    //color = (length(intensity.rgb) > 0.001) ? vec4(1.0) : vec4(0.0); // Check for any int.
    color = intensity;


    Fragment frag;
    frag.color = color;
    // Place stars at back to begin with. 
    frag.depth = DEFAULT_DEPTH;
    frag.gNormal = vec4(0.0, 0.0, 0.0, 1.0);
    frag.blend = BLEND_MODE_NORMAL;

    return frag;
}
