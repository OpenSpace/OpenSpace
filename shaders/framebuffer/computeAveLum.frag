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

#version __CONTEXT__

layout (location = 0) out vec4 finalColor;

uniform int bufferWidth;
uniform int bufferHeight;
uniform sampler2D hdrTexture;

in vec2 texCoord;

void main() {
    vec4 color = vec4(0.0);
    float fH = float(bufferHeight);
    float fW = float(bufferWidth);

    float sum = 0.f;
    for (int i = 0; i < bufferHeight; ++i) {
        for (int j = 0; j < bufferWidth; ++j) {
            vec2 texCoord = vec2(float(i) / fH, float(j) / fW);
            vec4 tmpColor = texture(hdrTexture, texCoord);    
            float lum = dot(tmpColor.xyz, vec3(0.2126f, 0.7152f, 0.0722f));
            sum += log(lum + 0.00001);
        }
    }

    finalColor = vec4(vec3(exp(sum / (fH * fW))), 1.0);
}