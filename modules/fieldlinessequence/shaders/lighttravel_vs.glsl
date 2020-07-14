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
uniform mat4      modelViewProjection;

layout(location = 0) in vec3 in_position;

out vec4 vs_color;
out float vs_depth;
/*
layout(location = 1) in float in_dist_from_start;
layout(location = 2) in float in_time_since_start;
layout(location = 3) in float in_transmission_time;
layout(location = 4) in float in_light_travel_time;


out float distanceFromStart;
out float timeSinceStart;
out float transmissionTime;
out float lightTravelTime;
out vec4 vs_positionScreenSpace;
out vec4 vs_gPosition;
out vec4 vs_color;
*/
void main() {
   
   if(gl_VertexID < 50000){
   vs_color = vec4(1.0, 1.0, 1.0, 1.0);
   }
   else{
   vs_color = vec4(0.2, 0.3, 0.4, 1.0);
   }
   vec4 position_in_meters = vec4(in_position, 1);
   vec4 positionClipSpace = modelViewProjection * position_in_meters;
   gl_Position = vec4(positionClipSpace.xy, 0, positionClipSpace.w);
   vs_depth = gl_Position.w;
   /*
   lightTravelTime = in_light_travel_time;
    timeSinceStart = in_time_since_start;   
    transmissionTime = in_transmission_time;
    distanceFromStart = in_dist_from_start;
*/
}
