/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

in vec2 vs_uv;
out vec4 color;

uniform sampler2D tex;
uniform sampler2D stencil;

vec2 offsets[8] = {
    vec2(-1.0, -1.0),
    vec2(-1.0,  0.0),
    vec2(-1.0,  1.0),
    vec2(0.0,  -1.0),
    vec2(0.0,   1.0),
    vec2(1.0,  -1.0),
    vec2(1.0,   0.0),
    vec2(1.0,   1.0)
};

vec3 gatherColors(vec2 position) {
    vec2 texSize = textureSize(tex, 0);
    vec2 h = vec2(1.0) / texSize;

    int nContributions = 0;
    vec3 totalColor = vec3(0.0);

    vec4 colors[8];
    for (int i = 0; i < 8; i++) {
        colors[i] = texture(tex, position + h * offsets[i]);
        float s = texture(stencil, position + h * offsets[i]).r;

        if (s != 0.0) {
            totalColor.rgb += colors[i].rgb;
            nContributions++;
        }
    }

    return totalColor / nContributions;
}

void main() {
    vec4 c = texture(tex, vs_uv); 
    float s = texture(stencil, vs_uv).r;

    if (s == 0.0) {
        // This means that the current fragment/texel we are looking at has not been
        // projected on and we only want to do the dilation into these texels

        color = vec4(gatherColors(vs_uv), 0.0);
    }
    else {
        // We are in a region where an image has been projected, so we can reuse the
        // already sampled version
        color = c;
    }
}