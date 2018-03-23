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

// Keep in sync with renderablegaiastars.h:ColumnOption enum
const int COLUMNOPTION_STATIC = 0;
const int COLUMNOPTION_MOTION = 1; 
const int COLUMNOPTION_COLOR = 2;
const float EPS = 1e-5;
const float Parsec = 3.0856776e16;

in vec3 in_position;
in vec3 in_velocity;
in vec2 in_brightness;

out vec3 vs_velocity;
out vec2 vs_brightness;
out vec4 vs_gPosition;

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;
uniform float time; 
uniform int columnOption;

void main() {
    vs_velocity = in_velocity;
    vs_brightness = in_brightness;
    
    // Convert kiloParsec to meter.
    vec4 modelPosition = vec4(in_position * 1000 * Parsec, 1.0);

    if ( columnOption != COLUMNOPTION_STATIC ) {
        modelPosition.xyz += time * in_velocity;
    } 

    vec4 viewPosition = view * model * modelPosition;

    // Remove stars without position, but still wasn't nullArrays.
    // Has to be done in Geometry shader because Vertices cannot be discarded here.
    if ( length(in_position) > EPS ){
        vs_gPosition = viewPosition;    
        gl_Position = projection * viewPosition;
    } else {
        vs_gPosition = vec4(0.0);    
        gl_Position = vec4(0.0);
    }
}
