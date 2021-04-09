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

// max_vertices seems toi be limited by hardware
// more segments would be preffered for a smoother line
layout (lines) in;
layout (line_strip, max_vertices = 64) out;

const float EPSILON = 1e-5;

uniform mat4 modelViewProjection;
uniform vec2 latitudeThreshold;
uniform vec2 longitudeThreshold;

in vec4 vs_interpColor[];
in vec2 vs_latlon[];
in float vs_vertexID[];

out vec4 ge_position;
out vec4 ge_interpColor;
out vec4 position;

 void main(){

    // Remove seam
    if(length(latitudeThreshold - vec2(-90.0, 90.0)) < EPSILON &&
       length(longitudeThreshold - vec2(-180., 180.0)) < EPSILON) return;

    vec2 position1 = radians(vec2(latitudeThreshold.x, longitudeThreshold.x));
    vec2 position2 = radians(vec2(latitudeThreshold.y, longitudeThreshold.x));
    vec2 position3 = radians(vec2(latitudeThreshold.y, longitudeThreshold.y));
    vec2 position4 = radians(vec2(latitudeThreshold.x, longitudeThreshold.y));

    // Sets the number of points per line segment
    float nPoints = 15; // Total #vertices is 4 * nPoints
    float alt = 0.0; // zero altitude i.e on the surface

    for(int i = 0; i <= nPoints; ++i) {
        vec2 point = position1 + float(i)/nPoints*(position2-position1);
        position = geoToCartConversion(point.x, point.y, alt);
        ge_position = modelViewProjection * position;
        ge_interpColor = vs_interpColor[0];
        gl_Position = ge_position;
        EmitVertex();
    }

    for(int i = 0; i <= nPoints; ++i) {
        vec2 point = position2 + float(i)/nPoints*(position3-position2);
        position = geoToCartConversion(point.x, point.y, alt);
        ge_position = modelViewProjection * position;
        ge_interpColor = vs_interpColor[0];
        gl_Position = ge_position;
        EmitVertex();
    }

    for(int i = 0; i <= nPoints; ++i) {
        vec2 point = position3 + float(i)/nPoints*(position4-position3);
        position = geoToCartConversion(point.x, point.y, alt);
        ge_position = modelViewProjection * position;
        ge_interpColor = vs_interpColor[0];
        gl_Position = ge_position;
        EmitVertex();
    }

    for(int i = 0; i <= nPoints; ++i) {
        vec2 point = position4 + float(i)/nPoints*(position1-position4);
        position = geoToCartConversion(point.x, point.y, alt);
        ge_position = modelViewProjection * position;
        ge_interpColor = vs_interpColor[0];
        gl_Position = ge_position;
        EmitVertex();
    }
    
    EndPrimitive();
 }
