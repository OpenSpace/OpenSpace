/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014                                                                    *
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

uniform mat4 modelViewProjection;
//uniform mat4 modelTransform;
uniform bool isMorphing;

uniform int time;
uniform int flParticleSize;
uniform int modulusDivider;
uniform int colorMethod;

uniform vec4 fieldlineColor;
uniform vec4 fieldlineParticleColor;

uniform float state_progression;

layout(location = 0) in vec3 in_position; // in meters
layout(location = 1) in vec3 in_pos_morph_to; // in meters
layout(location = 2) in float using_quick_morph; // in meters
layout(location = 3) in float unitIntensity;
// layout(location = 1) in vec4 in_color;

out vec4 vs_color;
out float vs_depth;

const int UNIFORM_COLOR = 0;
const int UNIT_DEPENDENT_COLOR = 1;
const int CLASSIFIED_COLOR = 2;

#include "PowerScaling/powerScaling_vs.hglsl"

void main() {
    // vec4 in_color = vec4(1.0,1.0,0.0,1.0);
    // Color every n-th vertex differently to show fieldline flow direction
    int modulus = (gl_VertexID + time) % modulusDivider;//5000;
    if ( modulus > 0 && modulus < flParticleSize) {
        if (colorMethod == UNIT_DEPENDENT_COLOR) {
            // TODO: FIX THIS. Its just temporary, until texture is provided
            if (unitIntensity < threshold) {
                vs_color = vec4(1.0,0.15,0.0,fieldlineParticleColor.a);
            }
        } else /*if (colorMethod == UNIFORM_COLOR)*/ {
            vs_color = fieldlineParticleColor;
        }
    } else {
        if (colorMethod == UNIT_DEPENDENT_COLOR) {
            // TODO: FIX THIS. Its just temporary, until texture is provided
            if (unitIntensity < threshold) {
                vs_color = vec4(0.0,1.0,1.0,fieldlineParticleColor.a);
            }
        } else /*if (colorMethod == UNIFORM_COLOR)*/ {
            vs_color = fieldlineColor;
        }
    }
    // vs_color = in_color;

    float scale = 1.0;//695700000.0;//150000000000.0;//6371000.0;

    vec3 offset;
    float local_state_progression = state_progression;
    if (isMorphing) {
        if (using_quick_morph > 0.99) { // quick_morph is == 1.0 if it should morph
            if (state_progression > 0.9999) {
                local_state_progression = (state_progression - 0.9999) / 0.0001;
            } else {
                local_state_progression = 0.0;
            }
        }
        offset = (in_pos_morph_to - in_position) * local_state_progression;// * 0.00001;
    } else {
        offset = vec3(0.0);
    }

    vec4 position_in_meters = vec4((in_position.xyz + offset)*scale, 1);

    vec4 positionClipSpace = modelViewProjection * position_in_meters;

    gl_Position = z_normalization(positionClipSpace);
    vs_depth = gl_Position.w;
}
