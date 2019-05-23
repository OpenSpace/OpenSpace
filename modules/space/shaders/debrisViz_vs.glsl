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

layout (location = 0) in vec4 vertex_data; // 1: x, 2: y, 3: z, 4: timeOffset
layout (location = 1) in vec2 orbit_data; // 1: epoch, 2: period

uniform dmat4 modelViewTransform;
uniform mat4 projectionTransform;

//uniform float lineFade;
uniform double inGameTime;
// uniform int numberOfSegments;


out vec4 viewSpacePosition;
out vec4 vs_position;
//out float fade;

//out vec4 vertex_data_out;
//out vec2 orbit_data_out;
out float periodFraction_f;
out float offsetPeriods;
out float vertexID_f;


void main() {
  
    // calculate nr of periods, get fractional part to know where
    // the vertex closest to the debris part is right now
    double nrOfPeriods = (inGameTime - orbit_data.x) / orbit_data.y;
    double periodFraction = fract(nrOfPeriods); //mod(nrOfPeriods, 1.0);
    periodFraction_f = float(periodFraction);

    // same procedure for the current vertex
    offsetPeriods = vertex_data.w / orbit_data.y;

    vertexID_f = float(gl_VertexID);



    /*// check difference of these two locations
    float vertexDistance = periodFraction_f - offsetPeriods; 
    
    if(vertexDistance < 0.0) { 
      vertexDistance += 1.0;
    }
  
    float invert = 1.0 - vertexDistance; // * lineFade;
    fade = clamp(invert * lineFade, 0.0, 1.0) ;*/
    //vertex_data_out = vertex_data;
    //orbit_data_out = orbit_data;
    
    viewSpacePosition = vec4(modelViewTransform * dvec4(vertex_data.xyz, 1));
    vs_position = z_normalization( projectionTransform * viewSpacePosition);
    gl_Position = vs_position;

}





