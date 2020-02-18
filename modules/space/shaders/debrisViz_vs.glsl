/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

layout (location = 0) in vec4 vertex_data; // 1: x, 2: y, 3: z, 4: timeOffset, 
layout (location = 1) in vec2 orbit_data; // 1: epoch, 2: period

uniform dmat4 modelViewTransform;
uniform mat4 projectionTransform;

//uniform float lineFade;
uniform double inGameTime;

out vec4 viewSpacePosition;
out float vs_position_w;

out float periodFraction_f;
out float offsetPeriods;
out float vertexID_f;

// debugers :
// out float offset;
// out float epoch;
// out float period;
// out double tajm;

void main() {

    // tajm = inGameTime;
    /** The way the position and line fade is calculated is:
    *   By using inGameTime, epoch and period of this orbit, 
    *   we get how many revolutions it has done since epoch.
    *   The fract of that, is how far into a revolution it has traveld since epoch.
    *   Similarly we do the same but for this vertex, but calculating offsetPeriods.
    *   In the fragment shader the difference between 
    *   periodFraction_f and offsetPeriods is calculated to know how much to fade
    *   that specific fragment.
    */

    // If orbit_data is doubles, cast to float first
    float offset = vertex_data.w;
    double epoch = orbit_data.x;
    double period = orbit_data.y;
  
    // calculate nr of periods, get fractional part to know where
    // the vertex closest to the debris part is right now
    double nrOfRevolutions = (inGameTime - epoch) / period;
    //double periodFraction = fract(nrOfRevolutions); //mod(nrOfRevolutions, 1.0);
    int intfrac = int(nrOfRevolutions);
    double doublefrac = double(intfrac);
    double periodFraction = nrOfRevolutions - doublefrac;
    if (periodFraction < 0.0) {
        periodFraction += 1.0;
    }
    periodFraction_f = float(periodFraction);

    // same procedure for the current vertex
    float period_f = float(period);
    offsetPeriods = offset / period_f;

    // offsetPeriods can also be calculated by passing the vertexID as a float
    // to the fragment shader and deviding it by nrOfSegments.
    vertexID_f = float(gl_VertexID);
    dvec3 positions = dvec3(vertex_data.x, vertex_data.y, vertex_data.z); 
    dvec4 vertexPosition = dvec4(positions, 1);
    viewSpacePosition = vec4(modelViewTransform * vertexPosition);
    vec4 vs_position = z_normalization( projectionTransform * viewSpacePosition);
    gl_Position = vs_position;
    vs_position_w = vs_position.w;

}





