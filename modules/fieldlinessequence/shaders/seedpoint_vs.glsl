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

uniform bool    isSpherical;
uniform float   scaleFactor;
uniform mat4    modelViewProjection;

// as provided in seed point files! Needs conversion to render properly in OpenSpace
layout(location = 0) in vec3 in_position;

out float vs_depth;

#include "PowerScaling/powerScaling_vs.hglsl"

void main() {
    
    vec4 position_in_meters;
    if (!isSpherical) {
        position_in_meters = vec4(in_position.xyz * scaleFactor, 1);
    } else {
        // TODO MOVE CONVERTION FROM SPHERICAL TO CARTESIAN TO A SHADER UTILS FILE!
        float radiusInMeters = in_position.x * scaleFactor; // AU to METERS
        float rad_x_sinLat = radiusInMeters * cos(radians(in_position.y));
        // float rad_x_sinLat = radiusInMeters * sin(radians(90.0 - sphericalPoint.y)); sin(90-x) == cos(x)

        position_in_meters = vec4(rad_x_sinLat * cos(radians(in_position.z)),
                                  rad_x_sinLat * sin(radians(in_position.z)),
                                  radiusInMeters * sin(radians(in_position.y)),
                                  1.0);
    }

    vec4 positionClipSpace = modelViewProjection * position_in_meters;
    gl_Position = z_normalization(positionClipSpace);
    vs_depth = gl_Position.w;
}
