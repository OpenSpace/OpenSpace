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

layout (lines) in;
layout (line_strip, max_vertices = 88) out;
layout(std430, binding = 5) buffer buf
{
    int nFlights;
};


uniform mat4 modelViewProjection;
uniform float opacity;
uniform vec2 latitudeThreshold;
uniform vec2 longitudeThreshold;
uniform int time;

//in float vs_vertexID[];
in vec4 vs_position[];
in vec4 vs_interpColor[];
in vec2 vs_latlon[];
in float vs_vertexID[];
in ivec2 vs_vertexInfo[];

out vec4 ge_position;
out vec4 ge_interpColor;
out vec4 position;

vec2 findIntermediatePoint(vec2 latlon1, vec2 latlon2, float f) {

    float phi1 = latlon1.x; float lambda1 = latlon1.y;
    float phi2 = latlon2.x; float lambda2 = latlon2.y;

    float delta = greatCircleDistance(phi1, lambda1, phi2, lambda2) / RADII; 
    
    float a = (sin(1-f)*delta) / sin(delta);
    float b = sin(f*delta) / sin(delta);

    float x = a*cos(phi1)*cos(lambda1) +  b*cos(phi2)*cos(lambda2);
    float y = a*cos(phi1)*sin(lambda1) +  b*cos(phi2)*sin(lambda2);
    float z = a*sin(phi1) + b*sin(phi2);
     
    float phi3 = atan(z,sqrt(x*x+y*y));
    float lambda3 = atan(y,x);

    return vec2(phi3, lambda3);
}


 void main(){
    
    // Discard erronous positions
    if(length(gl_in[0].gl_Position) < EPSILON || length(gl_in[1].gl_Position) < EPSILON) {
            return;
    }

    float firstSeen = vs_vertexInfo[0].x;
    float lastSeen = vs_vertexInfo[0].y;

    if(firstSeen < float(time) && float(time) < lastSeen) {
        // Start point
        gl_Position = gl_in[0].gl_Position;
        position = vs_position[0];
        ge_position = vs_position[0];
        vec4 startColor = vs_interpColor[0];
        ge_interpColor = startColor;
        EmitVertex();

        // Calculate current position
        float t = clamp((float(time) - firstSeen) / (lastSeen - firstSeen), 0.0, 1.0);
        vec2 pointCurrent = findIntermediatePoint(vs_latlon[0], vs_latlon[1], t);
        
        // Interpolate color for current position
        vec4 endColor = vs_interpColor[1];
        endColor = vec4(startColor * (1.0-t) +  t * endColor);
        
        // Mid points
        for(int i = 1; i < 20; ++i) {
            vec2 point = findIntermediatePoint(vs_latlon[0], pointCurrent, float(i)/20.0);
            position = geoToCartConversion(point.x, point.y, 0.0);
            ge_position = modelViewProjection * position;
            ge_interpColor = vec4(startColor * (1.0-float(i)/20.0) +  float(i)/20.0 * endColor);
            ge_interpColor.w = 0.2 * opacity;
            gl_Position = ge_position;
            EmitVertex();
        }

        // Point for current position
        position = geoToCartConversion(pointCurrent.x, pointCurrent.y, 0.0);
        ge_position = modelViewProjection * position;
        ge_interpColor = vec4(1.0);
        gl_Position = ge_position;
        EmitVertex();

        atomicAdd(nFlights, 1);

        EndPrimitive();
    }
    else return;
 }
