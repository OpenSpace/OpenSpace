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


#include "PowerScaling/powerScalingMath.hglsl"
// #include "C:\Users\Jonathan\Documents\exjobb\OpenSpace\shaders\PowerScaling\powerScalingMath.hglsl"


layout (location = 0) in vec4 vertex_data; // 1: x, 2: y, 3: z, 4: time
// This doesn't work, plz help
layout (location = 1) in vec2 orbit_data; // 1: epoch, 2: period


layout(location = 0) out vec4 vertex; // 1: x, 2: y, 3: z, 4: time
// This doesn't work, plz help
layout(location = 1) out vec2 orbit; // 1: epoch, 2: period


uniform dmat4 modelViewTransform;
uniform mat4 projectionTransform;

//uniform int numberOfSegments;
//uniform float lineFade;
//uniform vec3 debrisPosition;
//uniform int* VertexIDs;
//uniform int numberOfOrbits;
uniform float inGameTime;

out vec4 viewSpacePosition;
out vec4 vs_position;
//out float nrOfPeriods;
//out float offsetPeriods;
//out float fade;

void main() {    

    // calculate nr of periods, get fractional part to know where
    // the vertex closest to the debris part is right now
    //float nrOfPeriods = (inGameTime - orbit_data.x) / orbit_data.y;
    //float periodFraction = fract(nrOfPeriods); //mod(nrOfPeriods, 1.0);

    // same procedure for the current vertex
    //float offsetPeriods = vertex_data.w / orbit_data.y;
    //float offsetFraction = offsetPeriods;                //mod(offsetPeriods, 1.0);

    // check difference of these two locations
    //float vertexDistance = periodFraction - offsetFraction;

    //if(vertexDistance < 0.0) {
    //  vertexDistance += 1.0;
    //}
  
    // int vertexID = gl_VertexID;
    // float id = float(vertexID) / float(numberOfSegments*numberOfOrbits);

    //float test = 1.0 - vertexDistance; // * lineFade;
//    if (test < 1.0 ) {
//        test = 0.4;
//    }
//    if (test >= 1.0) {
//        test = 1.0;
//    }
    
    //fade = clamp(test * lineFade, 0.0, 1.0) ;

    //fade = 0.5 * lineFade;

    // int orbit = vertexID/numberOfSegments;
    // will this iterate or add onto the value in vertexIDs?:  VertexIDs = VertexIDs + orbit;
    // should it be VertexIDs[orbit] - gl_VertexID, OR gl_VertexID - VertexIDs[orbit]:
                        // int offset = VertexIDs[orbit] - gl_VertexID
                        // to know the direction of the debris
    // if(debrisPosition == vs_position)
    vertex = vertex_data;
    orbit = orbit_data;
    
    viewSpacePosition = vec4(modelViewTransform * dvec4(vertex_data.xyz, 1));
    vs_position = z_normalization( projectionTransform * viewSpacePosition);
    gl_Position = vs_position;      

}





