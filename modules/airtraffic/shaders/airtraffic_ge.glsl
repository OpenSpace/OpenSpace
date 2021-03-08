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
#define PI 3.1415926538

layout (lines) in;
layout (line_strip, max_vertices = 2) out;

const float EPSILON = 1e-5;
const float Parsec = 3.0856776e16;
const float RADII = 6378137.0; // Earth is approximated as a sphere update if changed. 

//in float vs_vertexID[];
in vec4 vs_position[];
in vec4 vs_interpColor[];
in vec2 vs_latlon[];
in float vs_vertexID[];
noperspective in vec2 mathLine[];

out vec4 ge_position;
out vec4 ge_interpColor;
noperspective out vec2 ge_mathLine;

uniform float trailSize; 

// Uses haversine formula, in meters
float greatCircleDistance(float lat1, float lon1, float lat2, float lon2) {
    // distance between latitudes 
    // and longitudes 
    float dLat = (lat2 - lat1) * PI / 180.0; 
    float dLon = (lon2 - lon1) * PI / 180.0; 
  
    // convert to radians 
    lat1 = (lat1) * PI / 180.0; 
    lat2 = (lat2) * PI / 180.0; 
  
    // apply formulae 
    float a = pow(sin(dLat / 2), 2) +  pow(sin(dLon / 2), 2) *  cos(lat1) * cos(lat2); 
     
    float c = 2 * asin(sqrt(a)); 
 
    return RADII * c; 
}

 void main(){


    float dist = greatCircleDistance(vs_latlon[0].x, vs_latlon[0].y, vs_latlon[1].x, vs_latlon[1].y);
    vec4 color = vs_interpColor[0];
    
    if(length(gl_in[0].gl_Position) < EPSILON || length(gl_in[1].gl_Position) < EPSILON || dist > 20000.0) {
         return;
    }
    
    
    // lines_adjacency: 0 -- 1 -- 2 -- 3
    /*vec4 startLinePos = gl_in[1].gl_Position;
    vec4 endLinePos = gl_in[2].gl_Position;

    for(int i = 1; i < 3; ++i){ 
        float dist = greatCircleDistance(vs_latlon[i].x, vs_latlon[i].y, vs_latlon[i+1].x, vs_latlon[i+1].y);

        if(length(gl_in[i].gl_Position) < EPSILON || length(gl_in[i+1].gl_Position) < EPSILON || dist > 60000.0 ){
            if(int(mod(vs_vertexID[i], trailSize)) == 0) {
                // First point of the trail
                return;
            }
            else if (int(mod(vs_vertexID[i], trailSize)) == trailSize-1) {
                // End point of the trail
                return;
            }
            else {
                // Line between 1 & 2 is too long, draw line between 0 & 3.
                if(length(gl_in[i-1].gl_Position - gl_in[i+1].gl_Position) < 60000.0 ){
                    startLinePos = gl_in[i-1].gl_Position;
                    endLinePos = gl_in[i+1].gl_Position;
                }
                else return;
            }
        }
    }*/

    //gl_Position = gl_in[i].gl_Position;
   /* ge_position = vs_position[1];
    ge_interpColor = vs_interpColor[1];
    ge_mathLine = mathLine[1];
    gl_Position = startLinePos;
    EmitVertex();

    ge_position = vs_position[2];
    ge_interpColor = vs_interpColor[2];
    ge_mathLine = mathLine[2];
    gl_Position = endLinePos;
    EmitVertex();
    */

    ge_position = vs_position[0];
    ge_interpColor = vs_interpColor[0];
    ge_mathLine = mathLine[0];
    gl_Position = gl_in[0].gl_Position;
    EmitVertex();

    ge_position = vs_position[1];
    ge_interpColor = vs_interpColor[1];
    ge_mathLine = mathLine[1];
    gl_Position = gl_in[1].gl_Position;
    EmitVertex();

    EndPrimitive();
 }

