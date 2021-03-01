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

layout (location = 0) in vec3 vertexPosition; // lat, long, alt
layout (location = 1) in vec2 vertexInfo; // velocity, true_track
layout (location = 2) in int vertexLastContact; //   

uniform mat4 modelViewProjection;
uniform float trailSize;
//uniform int timeStamp;
out float vertexID;
out vec4 vs_position;
out float trailS;
out vec4 interpColor;

const float RADII = 6378137.0; // Eart is approximated as a sphere update if changed. 

vec4 geoToCartConversion(float lat, float lon, float alt){

    float lat_rad = lat * PI / 180.0;
    float lon_rad = lon * PI / 180.0;

    float x = (RADII + alt) * cos(lat_rad) * cos(lon_rad);
    float y = (RADII + alt) * cos(lat_rad) * sin(lon_rad);
    float z = (RADII + alt) * sin(lat_rad);



    return vec4(x, y, z, 1.0);
}

void main() {

    vertexID = float(gl_VertexID);
    interpColor = vec4( 1.0 , 0.0, 0.0, 1.0 - mod(vertexID, trailSize)/(trailSize-1));
    vec4 position = geoToCartConversion(vertexPosition.x, vertexPosition.y, vertexPosition.z);
    vs_position = modelViewProjection * position;
    gl_Position = vs_position;
}



