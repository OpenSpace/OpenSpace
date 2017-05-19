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
uniform bool isSpherical;
//uniform mat4 modelTransform;
// uniform int time;


layout(location = 0) in vec3 in_position; // in meters
// layout(location = 1) in vec4 in_color;

// out vec4 vs_color;
// out vec4 vs_position;
out float gs_depth;

#include "PowerScaling/powerScaling_vs.hglsl"

void main() {
    // vec4 in_color = vec4(0.0,1.0,0.0,0.0);
    // // Color every n-th vertex differently to show fieldline flow direction
    // int modulus = (gl_VertexID + time) % 100;
    // if ( modulus > 0 && modulus < 99) {
    //     vs_color = vec4(in_color.rgb * 0.99, 1.0);
    // } else {
    //     vs_color = vec4(in_color.rgb * 0.99, 1.0);
    // }
    // vs_color = in_color;
    //vec4 tmp = vec4(in_position, 0);

    //vec4 position_meters = pscTransform(tmp, modelTransform);
    //vs_position = tmp;

    // project the position to view space
    //position_meters =  modelViewProjection * position_meters;
    //gl_Position = z_normalization(position_meters);
    vec4 position_in_meters;
    float scale = 1.0;//695700000.0;//150000000000.0;//6371000.0;
    if (!isSpherical) {
        scale = 6371000.0;
        position_in_meters = vec4(in_position.xyz * scale, 1);
    } else {
        float radiusInMeters = in_position.x * 149597871000.0; // AU to METERS
        float rad_x_sinLat = radiusInMeters * cos(radians(in_position.y));
        // float rad_x_sinLat = radiusInMeters * sin(radians(90.0 - sphericalPoint.y)); sin(90-x) == cos(x)

        position_in_meters = vec4(rad_x_sinLat * cos(radians(in_position.z)),
                                  rad_x_sinLat * sin(radians(in_position.z)),
                                  radiusInMeters * sin(radians(in_position.y)),
                                  1.0);
    }
    // //vs_position = vec4(in_position.xyz * scale, 1); // TODO powerscaleify?
    // vec4 position_in_meters = vec4(in_position.xyz * scale, 1);
    vec4 positionClipSpace = modelViewProjection * position_in_meters;
    gl_Position = z_normalization(positionClipSpace);
    gs_depth = gl_Position.w;
    // gl_Position = vec4(in_position,1.0);
}
