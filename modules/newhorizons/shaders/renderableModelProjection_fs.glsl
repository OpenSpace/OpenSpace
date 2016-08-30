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

in vec4 vs_position;
in vec4 vs_normal;
in vec2 vs_uv;
in vec4 ProjTexCoord;

layout (location = 0) out vec4 color;
// Even though the stencel texture is only a single channel, we still need to
// output a vec4, or the result will disappear
layout (location = 1) out vec4 stencil;

uniform sampler2D projectionTexture;

uniform mat4 ModelTransform;
uniform vec2 _scaling;
uniform vec3 boresight;

bool inRange(float x, float a, float b) {
    return (x >= a && x <= b);
} 

void main() {
    vec2 uv = vec2(0.5,0.5)*vs_uv+vec2(0.5,0.5);

    vec3 n = normalize(vs_normal.xyz);
    vec4 projected = ProjTexCoord;

    // normalize
    projected.x /= projected.w;
    projected.y /= projected.w;
    // invert gl coordinates
    projected.x = 1 - projected.x;

    if ((inRange(projected.x, 0, 1) && inRange(projected.y, 0, 1)) && (dot(n, boresight) < 0)) {
        color = texture(projectionTexture, projected.xy);
        color.a = 1.0;
        stencil = vec4(1.0);
    }
    else {
      color = vec4(vec3(0.0), 1.0);
      stencil = vec4(0.0);
    }
}
