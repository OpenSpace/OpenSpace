 /*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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

out vec4 renderTableColor;

in vec2 psfCoords;
in vec2 texturesCoords;

uniform int psfTextureSize;
uniform sampler2D psfTexture;
uniform sampler2D shapeTexture;

void main(void) {
    vec4 fullColor = vec4(0.0, 0.0, 0.0, 1.0);
    
    // Kernel Center
    vec2 psfTextureCoords = vec2((float(psfTextureSize)/2.0 + 1.0) / float(psfTextureSize));
    fullColor += texture2D(shapeTexture, texturesCoords) * 
                texture2D(psfTexture, psfTextureCoords);

    float maxConvSize = float(psfTextureSize);
    float middleConvSize = maxConvSize / 2.0;
    float convStep = 1.0 / maxConvSize;
    for (float i = 0.0; i < maxConvSize; i += convStep) {
        for (float j = 0.0; j < maxConvSize; j += convStep) {
            vec2 newTexCoords = texturesCoords;
            newTexCoords.x = i < middleConvSize ? texturesCoords.x - i : texturesCoords.x + i;
            newTexCoords.y = j < middleConvSize ? texturesCoords.y - j : texturesCoords.y + j;
            newTexCoords.x = newTexCoords.x > 1.0 ? 1.0 : newTexCoords.x < 0.0 ? 0.0 : newTexCoords.x;
            newTexCoords.y = newTexCoords.y > 1.0 ? 1.0 : newTexCoords.y < 0.0 ? 0.0 : newTexCoords.y;
            fullColor += texture2D(shapeTexture, newTexCoords) * 
                texture2D(psfTexture, vec2(i, j));
        }
    }
    //fullColor = texture2D(shapeTexture, texturesCoords);

    // vec4 sum = vec4(0.0);
 
    // vec2 stepSize = 1.0/(uTextureSize);
    // vec2 coord = vTexCoord + uOrigin * stepSize;
   
    // sum += texture2D(uSampler, vec2(coord.x - stepSize.x, coord.y - stepSize.y)) * uKernel[0];
    // sum += texture2D(uSampler, vec2(coord.x, coord.y - stepSize.y)) * uKernel[1];
    // sum += texture2D(uSampler, vec2(coord.x + stepSize.x, coord.y - stepSize.y)) * uKernel[2];
 
    // sum += texture2D(uSampler, vec2(coord.x - stepSize.x, coord.y)) * uKernel[3];
    // sum += texture2D(uSampler, vec2(coord.x, coord.y)) * uKernel[4];
    // sum += texture2D(uSampler, vec2(coord.x + stepSize.x, coord.y)) * uKernel[5];
 
    // sum += texture2D(uSampler, vec2(coord.x - stepSize.x, coord.y + stepSize.y)) * uKernel[6];
    // sum += texture2D(uSampler, vec2(coord.x, coord.y + stepSize.y)) * uKernel[7];
    // sum += texture2D(uSampler, vec2(coord.x + stepSize.x, coord.y + stepSize.y)) * uKernel[8];
 
    // sum = sum * uFactor + uBias;
    // sum.a = 1.0;
    // gl_FragColor = sum;

    renderTableColor = fullColor;
}