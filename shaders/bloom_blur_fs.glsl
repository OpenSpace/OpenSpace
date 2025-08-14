/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

layout (location = 0) out vec4 finalColor;

in vec2 UV;

uniform sampler2D mainColorTexture;
uniform vec2 blurDirection;
uniform float blurMagnitude;

uniform int n = 3;
uniform float offset[3] = float[]( 0, 1.4983346041173402, 3.4961134994916776 );
uniform float weight[3] = float[]( 0.112759, 0.224271, 0.21934900000000002 );

void main() {
    vec4 sum = vec4(0.0);
    vec2 tc = UV;
    float mag = blurMagnitude;
    float hstep = blurDirection.x;
    float vstep = blurDirection.y;
    float h = mag*hstep;
    float v = mag*vstep;

    sum = texture( mainColorTexture, tc ) * weight[0];
    for (int i=1; i<n; i++) {
        sum += texture( mainColorTexture, tc + vec2( offset[i] * h, offset[i] * v ) ) * weight[i];
        sum += texture( mainColorTexture, tc - vec2( offset[i] * h, offset[i] * v ) ) * weight[i];
    }

    finalColor = vec4(sum.rgb, 1.0);
}