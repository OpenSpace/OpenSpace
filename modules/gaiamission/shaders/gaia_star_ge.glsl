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

#include "floatoperations.glsl"

// Keep in sync with renderablegaiastars.h:ColumnOption enum
const int COLUMNOPTION_STATIC = 0;
const int COLUMNOPTION_MOTION = 1; 
const int COLUMNOPTION_COLOR = 2;

layout(points) in;
in vec3 vs_velocity[];
in float vs_brightness[];
in vec4 vs_gPosition[];

layout(triangle_strip, max_vertices = 4) out;
out vec4 vs_position;
out vec3 ge_velocity;
out float ge_brightness;
out vec4 ge_gPosition;               
out vec2 texCoord;
out float ge_observationDistance;

uniform float viewScaling;
uniform float closeUpBoostDist;
uniform float billboardSize;
uniform vec2 screenSize;
uniform int columnOption;

const vec2 corners[4] = vec2[4]( 
    vec2(0.0, 1.0), 
    vec2(0.0, 0.0), 
    vec2(1.0, 1.0), 
    vec2(1.0, 0.0) 
);

const float EPS = 1e-5;

void main() {

    ge_brightness = vs_brightness[0];
    ge_velocity = vs_velocity[0];

    // Make closer stars look a bit bigger.
    float observedDistance = safeLength(vs_gPosition[0] / viewScaling);
    float closeUpBoost = closeUpBoostDist / observedDistance;

    // Take magnitude into account if that option is chosen.
    if ( columnOption == COLUMNOPTION_COLOR ) {
        float absoluteMagnitude = vs_brightness[0];
        closeUpBoost *= absoluteMagnitude / 5.0;
    }

    vs_position = gl_in[0].gl_Position;
    vec2 starSize = vec2(billboardSize + closeUpBoost) / screenSize * vs_position.w;

    // Discard geometry if star has no position (but wasn't a nullArray). 
    // Or if size is smaller than one pixel.
    if( length(vs_position) < EPS || length(starSize) < 1.5 ){
        return;
    }

    for (int i = 0; i < 4; i++) {
        gl_Position = vs_position + vec4(starSize * (corners[i] - 0.5), 0.0, 0.0);
        gl_Position.z = 0.0;
        texCoord = corners[i];

        // G-Buffer
        ge_gPosition  = vs_gPosition[0];
        ge_observationDistance = observedDistance;
        
        EmitVertex();
    }

    EndPrimitive();
}
