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
const float EPS = 1e-5;

layout(points) in;
in vec2 vs_brightness[];
in vec4 vs_gPosition[];
in float vs_starDistFromSun[];
in float vs_cameraDistFromSun[];

layout(triangle_strip, max_vertices = 4) out;
out vec2 ge_brightness;
out vec4 ge_gPosition;               
out vec2 texCoord;
out float ge_starDistFromSun;
out float ge_cameraDistFromSun;

uniform float viewScaling;
uniform float closeUpBoostDist;
uniform float billboardSize;
uniform vec2 screenSize;
uniform int columnOption;
uniform float cutOffThreshold;
uniform float magnitudeBoost;

const vec2 corners[4] = vec2[4]( 
    vec2(0.0, 1.0), 
    vec2(0.0, 0.0), 
    vec2(1.0, 1.0), 
    vec2(1.0, 0.0) 
);


void main() {

    ge_brightness = vs_brightness[0];
    ge_starDistFromSun = vs_starDistFromSun[0];
    ge_cameraDistFromSun = vs_cameraDistFromSun[0];

    // Make closer stars look a bit bigger.
    float observedDistance = safeLength(vs_gPosition[0] / viewScaling);
    float closeUpBoost = closeUpBoostDist / observedDistance;
    float initStarSize = billboardSize;

    // Use magnitude for size boost as well.
    if ( columnOption == COLUMNOPTION_COLOR ) {
        // DR1 magnitudes are [4, 20], but could be [-15, 20] according to this chart:
        // https://qph.fs.quoracdn.net/main-qimg-317a18e3b228efc7d7f67a1632a55961
        // Negative magnitude => Giants
        // Big positive magnitude => Dwarfs
        float absoluteMagnitude = vs_brightness[0].x;
        float normalizedMagnitude = (absoluteMagnitude - 20) / -1; // (-15 - 20);
        
        // TODO: A linear scale is prabably not the best!
        initStarSize += normalizedMagnitude * (magnitudeBoost / 100);
    }

    vec4 position = gl_in[0].gl_Position;
    vec2 starSize = vec2(initStarSize + closeUpBoost) / screenSize * position.w;

    float distThreshold = cutOffThreshold - log(observedDistance) / log(4.0);

    // Discard geometry if star has no position (but wasn't a nullArray).
    // Or if observed distance is above threshold set by cutOffThreshold.
    // By discarding in gs instead of fs we save computations for when nothing is visible.
    if( length(position) < EPS || distThreshold <= 0){
        return;
    }

    for (int i = 0; i < 4; i++) {
        gl_Position = position + vec4(starSize * (corners[i] - 0.5), 0.0, 0.0);
        gl_Position.z = 0.0;
        texCoord = corners[i];
        ge_gPosition  = vs_gPosition[0];
        
        EmitVertex();
    }

    EndPrimitive();
}
