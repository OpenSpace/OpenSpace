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

#include "D:\OpenSpace\shaders\PowerScaling\powerScalingMath.hglsl"

layout (location = 0) in vec4 vertex_data;

uniform dmat4 modelViewTransform;
uniform mat4 projectionTransform;

uniform int numberOfSegments;
uniform float lineFade;
uniform vec3 debrisPosition;
// uniform int* VertexIDs;
uniform int numberOfOrbits;
uniform double inGameTime;

out vec4 viewSpacePosition;
out vec4 vs_position;
out float fade;

void main() {    


    int vertexID = gl_VertexID;
    double timeOffset = vertex_data.w;  // epoch + period fraction
    float id = float(vertexID) / float(numberOfSegments);
    fade = clamp(id * lineFade, 0.0, 1.0); 



    int orbit = vertexID/numberOfSegments;
    // will this iterate or add onto the value in vertexIDs?:  VertexIDs = VertexIDs + orbit;
    // should it be VertexIDs[orbit] - gl_VertexID, OR gl_VertexID - VertexIDs[orbit]:
                        // int offset = VertexIDs[orbit] - gl_VertexID
                        // to know the direction of the debris
    // if(debrisPosition == vs_position)
    
    viewSpacePosition = vec4(modelViewTransform * dvec4(vertex_data.xyz, 1));
    vs_position = z_normalization( projectionTransform * viewSpacePosition);
    gl_Position = vs_position;      

}





