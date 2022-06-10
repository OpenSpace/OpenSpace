/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#include "PowerScaling/powerScaling_vs.hglsl"

const float Parsec = 3.0856776e16;

layout(location = 0) in vec3 in_position;
in float in_colormapAttributeScalar;
in float in_linearSizeAttributeScalar;

in vec3 in_velocity;
// out vec4 vs_gPosition;

out float vs_colormapAttributeScalar;
flat out float vs_linearSizeAttributeScalar;

uniform bool motionEnabled;
uniform float theTime;

void main() {
    vs_colormapAttributeScalar = in_colormapAttributeScalar;
    vs_linearSizeAttributeScalar = in_linearSizeAttributeScalar;
    
    // Don't show points with no value for velocity
    // if (motionEnabled && (isnan(in_velocity[0]) || isnan(in_velocity[1]) || isnan(in_velocity[2]))) {
    //     vs_gPosition = vec4(0.0);    
    //     gl_Position = vec4(0.0);
    //     return;
    // }

    vec4 objectPosition = vec4(in_position, 1.0);

    // Add velocity if applicable
    if (motionEnabled) {
        // TODO: How to handle NaN velocity values???
        if (!isnan(in_velocity[0]) || !isnan(in_velocity[1]) || !isnan(in_velocity[2])) {
            // vec4 objectPosition = vec4(in_position * 1000 * Parsec, 1.0);
            objectPosition.xyz += 10*theTime/Parsec * in_velocity;
        }
        // objectPosition.xyz += theTime * vec3(500000.0, 500000.0, 500000.0); 
    }

    gl_Position = objectPosition;
}
