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
// uniform int time;
uniform double timeD;
uniform int flParticleSize;
uniform int modulusDivider;
uniform int colorMethod;

uniform vec2 transferFunctionLimits;
uniform vec2 domainLimR;
uniform vec2 domainLimX;
uniform vec2 domainLimY;
uniform vec2 domainLimZ;

uniform sampler1D colorMap;

uniform vec4 fieldlineColor;
uniform vec4 fieldlineParticleColor;

layout(location = 0) in vec3 in_position; // in meters
layout(location = 3) in float unitIntensity;

out vec4 vs_color;
out float vs_depth;
out flat float fragment_discard;

const int UNIFORM_COLOR         = 0;
const int UNIT_DEPENDENT_COLOR  = 1;
const int CLASSIFIED_COLOR      = 2;

#include "PowerScaling/powerScaling_vs.hglsl"

void main() {

    float radius = length(in_position);
    fragment_discard = 1.0;

    if ((in_position.x < domainLimX.x) || (in_position.x > domainLimX.y) ||
        (in_position.y < domainLimY.x) || (in_position.y > domainLimY.y) ||
        (in_position.z < domainLimZ.x) || (in_position.z > domainLimZ.y) ||
        (radius        < domainLimR.x) || (radius        > domainLimR.y)) {
        fragment_discard = 0.0;
    }

    // Color every n-th vertex differently to show fieldline flow direction
    int modulus = (gl_VertexID + int(timeD)) % modulusDivider;
    // int modulus = (gl_VertexID + time) % modulusDivider;
    if ( modulus > 0 && modulus < flParticleSize) {
        if (colorMethod == UNIT_DEPENDENT_COLOR) {
            float lookUpValue = (unitIntensity - transferFunctionLimits.x )
                    / (transferFunctionLimits.y - transferFunctionLimits.x);
            vec4 color = texture(colorMap, lookUpValue);
            vs_color = vec4(color.xyz,fieldlineParticleColor.a);
        } else /*if (colorMethod == UNIFORM_COLOR)*/ {
            vs_color = fieldlineParticleColor;
        }
    } else {
        if (colorMethod == UNIT_DEPENDENT_COLOR) {
            float lookUpValue = (unitIntensity - transferFunctionLimits.x)
                    / (transferFunctionLimits.y - transferFunctionLimits.x);
            vec4 color = texture(colorMap, lookUpValue);
            vs_color = vec4(color.xyz,fieldlineColor.a);
        } else /*if (colorMethod == UNIFORM_COLOR)*/ {
            vs_color = fieldlineColor;
        }
    }

    vec4 position_in_meters = vec4(in_position.xyz, 1);
    vec4 positionClipSpace = modelViewProjection * position_in_meters;
    gl_Position = z_normalization(positionClipSpace);
    vs_depth = gl_Position.w;
}
