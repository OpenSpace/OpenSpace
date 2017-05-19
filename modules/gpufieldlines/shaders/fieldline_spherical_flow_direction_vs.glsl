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
uniform int time;
uniform float radiusScale; // Enlil uses astronomical units.. OpenSpace needs meters!

layout(location = 0) in vec3 in_position; // input is in spherical coordinates, where x = radius, y = latitude (range -90 to 90), z = longitude (0 to 360)
layout(location = 1) in vec4 in_color;

out vec4 vs_color;
out vec4 vs_position;

const float DEG_TO_RAD = 3.14159265359 / 180.0;

#include "PowerScaling/powerScaling_vs.hglsl"

void main() {
    // Color every n-th vertex differently to show fieldline flow direction
    int modulus = (gl_VertexID + time) % 100;
    if ( modulus > 0 && modulus < 99) {
        vs_color = vec4(in_color.rgb * 0.99, 0.75   );
    } else {
        vs_color = vec4(in_color.rgb * 0.99, 0.05);
    }
    // vs_color = in_color;
    //vec4 tmp = vec4(in_position, 0);

    //vec4 position_meters = pscTransform(tmp, modelTransform);
    //vs_position = tmp;

    // project the position to view space
    //position_meters =  modelViewProjection * position_meters;
    //gl_Position = z_normalization(position_meters);

    // if input coordinates are in spherical coordinates convert to cartesian
    float r         = radiusScale * in_position.x; // radius in meters
    float lat_rad   = DEG_TO_RAD   * in_position.y;
    float lon_rad   = DEG_TO_RAD   * in_position.z;
    float r_cosLat  = r * cos(lat_rad);

    vec4 position_in_meters(r_cosLat * cos(lon_rad), r_cosLat * sin(lon_rad), r * sin(lat_rad),1.0);

    // float scale = 1.0;//695700000.0;//150000000000.0;//6371000.0;
    //vs_position = vec4(in_position.xyz * scale, 1); // TODO powerscaleify?
    // vec4 position_in_meters = vec4(in_position.xyz * scale, 1);
    vec4 positionClipSpace = modelViewProjection * position_in_meters;
    vs_position = z_normalization(positionClipSpace);
    gl_Position = vs_position;
}
