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

in vec4 gs_colorMap;
in float vs_screenSpaceDepth;
in vec2 texCoord;
in float ta;

uniform float alphaValue;
uniform vec3 color;
uniform sampler2D spriteTexture;
uniform bool hasColorMap;
uniform float fadeInValue;

Fragment getFragment() {      
   
    vec4 textureColor = texture(spriteTexture, texCoord);
    
    vec4 fullColor = vec4(1.0);
    
    if (hasColorMap) {
        fullColor = vec4(
            gs_colorMap.rgb * textureColor.rgb, 
            gs_colorMap.a * textureColor.a * alphaValue
            );
    } else {
        fullColor = vec4(color.rgb * textureColor.rgb, textureColor.a * alphaValue);
    }

    fullColor.a *= fadeInValue * ta;
    
    if (fullColor.a == 0.f || 
        fullColor.rgb == vec3(0.0)) {
        discard;
    }

    Fragment frag;
    frag.color      = fullColor;
    frag.depth      = vs_screenSpaceDepth;
    // Setting the position of the billboards to not interact 
    // with the ATM.
    frag.gPosition  = vec4(-1e32, -1e32, -1e32, 1.0);
    frag.gNormal    = vec4(0.0, 0.0, 0.0, 1.0);


    return frag;
}