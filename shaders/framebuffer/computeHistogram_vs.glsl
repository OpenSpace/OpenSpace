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

layout(location = 0) in vec4 pixelCoord;

uniform float maxWhite;
//uniform int numberOfPixels;
//uniform int numberOfBins;
uniform float imageWidth;
uniform float imageHeight;

uniform sampler2D renderedImage;

flat out vec3 pColor;

void main()
{
    vec2 texCoord;
    texCoord.x = float(int(pixelCoord.x / imageWidth)) / imageWidth;
    texCoord.y = float(int(pixelCoord.x) % int(imageWidth)) / imageHeight;
    vec3 pixelColor = texture(renderedImage, texCoord).xyz;
    pColor = pixelColor;
    float pixelLuminosity = dot(pixelColor, vec3(0.2126f, 0.7152f, 0.0722f));

    //gl_Position = vec4(-1.0 + (2.0 * pixelLuminosity / maxWhite), 0.0, 0.0, 1.0);    

    //gl_Position = vec4(2.0 * texCoord - vec2(1.0), 0.0, 1.0);

    //gl_Position = vec4(0.5, 2.0 * texCoord.y - 1.0, 0.0, 1.0);

    //gl_Position = vec4(-1.0 + (pixelLuminosity * 0.0078125), -1.0, 0.0, 1.0);

    //gl_Position = vec4(0.5, -1.0 + (pixelLuminosity * 0.0078125), 0.0, 1.0);

    //gl_Position = vec4(0.0, -1.0 + (2.0 * pixelLuminosity / maxWhite), 0.0, 1.0);    

    gl_Position = vec4((2.0 * pixelLuminosity / maxWhite) - 1.0, 2.0 * texCoord.y - 1.0, 0.0, 1.0);

    gl_PointSize = 1.0;
}