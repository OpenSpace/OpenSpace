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

// keep in sync with renderablestars.h:ColorOption enum
const int COLOROPTION_COLOR = 0;
const int COLOROPTION_VELOCITY = 1; 
const int COLOROPTION_SPEED = 2;
 
uniform sampler2D psfTexture;
uniform sampler1D colorTexture;
uniform float minBillboardSize;

uniform float alphaValue;
uniform int colorOption;

in vec4 vs_position;
in vec4 ge_gPosition;
in vec3 ge_brightness;
in vec3 ge_velocity;
in float ge_speed;
in vec2 texCoord;
in float billboardSize;

#include "fragment.glsl"
//#include "PowerScaling/powerScaling_fs.hglsl"

uniform vec2 magnitudeClamp;


vec4 bv2rgb(float bv) {
    // BV is [-0.4,2.0]
    float t = (bv + 0.4) / (2.0 + 0.4);
    return texture(colorTexture, t);
}

Fragment getFragment() {
    // Something in the color calculations need to be changed because before it was dependent
    // on the gl blend functions since the abuffer was not involved
        
    vec4 color = vec4(0.0);
    switch (colorOption) {
        case COLOROPTION_COLOR: 
            color = bv2rgb(ge_brightness.x);
            break;
        case COLOROPTION_VELOCITY:
            color = vec4(abs(ge_velocity), 0.5); 
            break;
        case COLOROPTION_SPEED:
            // @TODO Include a transfer function here ---abock
            color = vec4(vec3(ge_speed), 0.5);
            break;
    }

    vec4 textureColor = texture(psfTexture, texCoord);
    vec4 fullColor = vec4(color.rgb, textureColor.a);
    fullColor.a *= alphaValue;

    vec4 position = vs_position;
    // This has to be fixed when the scale graph is in place ---emiax
    position.w = 15;

    Fragment frag;
    frag.color = fullColor;
    frag.depth = pscDepth(position);

    // G-Buffer
    frag.gPosition  = ge_gPosition;
    // There is no normal here
    frag.gNormal    = vec4(0.0, 0.0, 0.0, 1.0);
    
    if (fullColor.a == 0) {
        discard;
    }

    return frag;
}