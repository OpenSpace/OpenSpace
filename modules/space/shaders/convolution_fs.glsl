 /*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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
uniform int convolvedfTextureSize;
uniform sampler2D psfTexture;
uniform sampler2D shapeTexture;

void main() {
    float fullColor = 0.0;
    
    // Kernel Center
    //vec2 psfTextureCoords = vec2((float(psfTextureSize)/2.0 + 1.0) / float(psfTextureSize));
    // fullColor += texture2D(shapeTexture, texturesCoords) * 
    //             texture2D(psfTexture, psfTextureCoords);

    float maxConvSize = float(psfTextureSize);
    float convStep = 1.0 / maxConvSize;
    float textureStep = 1.0 / float(convolvedfTextureSize);
    for (float i = 0.0, ii = 0.0; i < maxConvSize; i += convStep, ii += textureStep) {
        for (float j = 0.0, jj = 0.0; j < maxConvSize; j += convStep, jj += textureStep) {
            vec2 newTexCoords = texturesCoords;
            newTexCoords.x = i < 0.5 ? texturesCoords.x - ii : texturesCoords.x + ii;
            newTexCoords.y = j < 0.5 ? texturesCoords.y - jj : texturesCoords.y + jj;
            newTexCoords.x = newTexCoords.x > 1.0 ? 1.0 : newTexCoords.x < 0.0 ? 0.0 : newTexCoords.x;
            newTexCoords.y = newTexCoords.y > 1.0 ? 1.0 : newTexCoords.y < 0.0 ? 0.0 : newTexCoords.y;
            fullColor += texture2D(shapeTexture, newTexCoords).x * 
                texture2D(psfTexture, vec2(i, j)).x;
        }
    }

    /*
     vec2 onePixel = vec2(1.0, 1.0) /  float(psfTextureSize);
    vec4 fullColor =
     texture2D(shapeTexture, texturesCoords + onePixel * vec2(-1, -1)) * texture2D(psfTexture, vec2(i, j)) +
     texture2D(shapeTexture, texturesCoords + onePixel * vec2( 0, -1)) * texture2D(psfTexture, vec2(i, j)) +
     texture2D(shapeTexture, texturesCoords + onePixel * vec2( 1, -1)) * texture2D(psfTexture, vec2(i, j)) +
     texture2D(shapeTexture, texturesCoords + onePixel * vec2(-1,  0)) * texture2D(psfTexture, vec2(i, j)) +
     texture2D(shapeTexture, texturesCoords + onePixel * vec2( 0,  0)) * texture2D(psfTexture, vec2(i, j)) +
     texture2D(shapeTexture, texturesCoords + onePixel * vec2( 1,  0)) * texture2D(psfTexture, vec2(i, j)) +
     texture2D(shapeTexture, texturesCoords + onePixel * vec2(-1,  1)) * texture2D(psfTexture, vec2(i, j)) +
     texture2D(shapeTexture, texturesCoords + onePixel * vec2( 0,  1)) * texture2D(psfTexture, vec2(i, j)) +
     texture2D(shapeTexture, texturesCoords + onePixel * vec2( 1,  1)) * texture2D(psfTexture, vec2(i, j));
     */


    renderTableColor = vec4(fullColor/40.0);
}
