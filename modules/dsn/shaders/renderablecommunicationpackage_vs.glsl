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

#include "PowerScaling/powerScaling_vs.hglsl"

layout(location = 0) in vec3 in_point_position;
layout(location = 1) in vec4 in_color;

in int gl_VertexID;

out vec4 vs_positionScreenSpace;
out vec4 vs_gPosition;
out vec4 vs_color;

uniform dmat4 modelViewStation;
uniform dmat4 modelViewSpacecraft;
uniform mat4 projectionTransform;

void main() {
    
    //we use different modelviews depending on if the vertex is a stationposition or a spacecraftposition
    if(mod(gl_VertexID,2) == 0 ){
        vs_gPosition = vec4(modelViewStation * dvec4(in_point_position, 1));
    }else{
        vs_gPosition = vec4(modelViewSpacecraft * dvec4(in_point_position, 1));
    }

    vs_positionScreenSpace = z_normalization(projectionTransform * vs_gPosition);
    gl_Position  = vs_positionScreenSpace;
    vs_color = in_color;
}
