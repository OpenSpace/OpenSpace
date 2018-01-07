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
#include "PowerScaling/powerScaling_fs.hglsl"

in vec2 vs_st;
in vec4 vs_position;
in vec4 vs_gPosition;

uniform float time;
uniform sampler2D texture1;
uniform float alpha;


Fragment getFragment() {
    vec4 position = vs_position;
    vec2 texCoord = vs_st;
    // Why is this here? ---abock
    texCoord.s = 1 - texCoord.s;
    texCoord.t = 1 - texCoord.y;

    Fragment frag;
    frag.color = texture(texture1, texCoord) * vec4(1.0, 1.0, 1.0, alpha);
    frag.depth = pscDepth(position);

    // G-Buffer
    frag.gPosition  = vs_gPosition;
    // There is no normal here
    // TODO: Add the correct normal (JCC)
    frag.gNormal = vec4(0.0, 0.0, 0.0, 1.0);

    return frag;
    }
