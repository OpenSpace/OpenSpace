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

#include "airtraffic_utilities.glsl"

layout (location = 0) in vec3 vertexPosition; // lat, long, alt
layout (binding = 5, offset = 0) uniform atomic_uint counter;

uniform mat4 modelViewProjection;
uniform float trailSize;
uniform ivec2 resolution;
uniform vec3 color;
uniform float opacity;
uniform vec2 latitudeThreshold;
uniform vec2 longitudeThreshold;

noperspective out vec2 mathLine;
out vec4 vs_position;
out vec4 vs_interpColor;
out vec2 vs_latlon;
out float vs_vertexID;

void main() {
    vs_vertexID = float(gl_VertexID);
    vs_interpColor = vec4(color, opacity * pow(1.0 - mod(vs_vertexID, trailSize)/(trailSize-1), 2.0));
    vec4 position;

    if(latitudeThreshold.x < vertexPosition.x && vertexPosition.x < latitudeThreshold.y 
        && longitudeThreshold.x < vertexPosition.y && vertexPosition.y < longitudeThreshold.y
        && vertexPosition.z > -EPSILON)
    {
        position = geoToCartConversion(radians(vertexPosition.x), radians(vertexPosition.y), vertexPosition.z);
    }
    else 
    {
        position = vec4(0.0);
    }

    atomicCounterIncrement(counter);
    
    vs_latlon = vec2(radians(vertexPosition.x), radians(vertexPosition.y));

    vs_position = modelViewProjection * position;
    vec4 vs_positionNDC = vs_position / vs_position.w;
    gl_Position = vs_position;
    
    mathLine = 0.5 * (vs_positionNDC.xy + vec2(1.0)) * vec2(resolution);
}



