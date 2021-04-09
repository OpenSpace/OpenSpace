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
#include "airtraffic_utilities.glsl"

/*
Continents:       lon1,  lat1,    lon2, lat2
Europe:          -24.7,  35.7,    52.1, 71.7
Asia:             46.2, -10.2,  180.0, 71.7 
Oceania:         113.1, -54.1,  180.0, -5.3 
Africa:          -20.1, -39.7,    48.8, 36.9 --- NOT USED
South America:   -92.7, -56.8,   -23.8, 13.6 
North America:  -168.3,  12.5,   -25.8, 74.1 

Africa1 -20.5,-39.2,32.4,37.6
Africa2 32.4,-39.2,53.5,12.8

*/

uniform mat4 modelViewProjection;
uniform float opacity;
uniform vec2 latitudeThreshold;
uniform vec2 longitudeThreshold;

out vec4 vs_position;
out vec2 vs_latlon;
out float vs_vertexID;
out ivec2 vs_vertexInfo;
out vec4 vs_interpColor;

layout (location = 0) in vec2 vertexPosition; // lat, lon
layout (location = 1) in ivec2 vertexInfo; // firstSeen, lastSeen

//const float RADII = 6378137.0; // Earth is approximated as a sphere update if changed. 

/*
vec4 geoToCartConversion(float lat, float lon, float alt){
    if(latitudeThreshold.x < lat && lat < latitudeThreshold.y 
    && longitudeThreshold.x < lon && lon < longitudeThreshold.y) {

        float lat_rad = lat * PI / 180.0;
        float lon_rad = lon * PI / 180.0;

        float x = (RADII + alt) * cos(lat_rad) * cos(lon_rad);
        float y = (RADII + alt) * cos(lat_rad) * sin(lon_rad);
        float z = (RADII + alt) * sin(lat_rad);

        return vec4(x, y, z, 1.0);
    }
    else return vec4(0.f);
}
*/

// Set color based on continent
vec4 continentColor(vec2 latlon) {

    vec4 color;
    
    // Europe  -24.7,  35.7,    52.1, 71.7
    if(latlon.x < 71.7 && latlon.x > 35.7 && latlon.y < 52.1 && latlon.y > -24.7)
        color = vec4(0.0, 0.0, 1.0, opacity);
    // Asia  46.2, -10.2,  -169.0, 71.7
    else if(latlon.x < 71.7 && latlon.x > -10.2 && latlon.y < 180.0 && latlon.y > 46.2)
        color = vec4(1.0, 0.0, 0.0, opacity);
    // Oceania
    else if(latlon.x < -5.3 && latlon.x > -54.1 && latlon.y < 180 && latlon.y > 113.0)
        color = vec4(1.0, 1.0, 0.0, opacity);
        // Africa box one: -20.5,-39.2,32.4,37.6
    else if(latlon.x < 37.6 && latlon.x > -39.2 && latlon.y < 32.4 && latlon.y > -20.5)
        color = vec4(0.0, 1.0, 1.0, opacity);
         // Africa box two: 32.4,-39.2,53.5,12.8
    else if(latlon.x < 12.8 && latlon.x > -39.2 && latlon.y < 53.5 && latlon.y > 32.4)
        color = vec4(0.0, 1.0, 1.0, opacity);
    // North America: -168.3,  12.5,   -25.8, 74.1
    else if(latlon.x < 74.1 && latlon.x > 12.5 && latlon.y < -25.8 && latlon.y > -168.3)
        color = vec4(0.0, 1.0, 0.0, opacity);
    // South America
    else if(latlon.x < 13.6 && latlon.x > -56.8 && latlon.y < -23.8 && latlon.y > -92.7)
        color = vec4(1.0, 0.0, 1.0, opacity);
    else 
        color = vec4(1.0, 1.0, 1.0, opacity);

    return color;
}

void main() {

    vs_vertexID = float(gl_VertexID);
    vec4 position;

    if(latitudeThreshold.x < vertexPosition.x && vertexPosition.x < latitudeThreshold.y 
        && longitudeThreshold.x < vertexPosition.y && vertexPosition.y < longitudeThreshold.y) 
    {
        position = geoToCartConversion(radians(vertexPosition.x), radians(vertexPosition.y), 0.0);
    }
    else position = vec4(0.0);
   
    vs_interpColor = continentColor(vertexPosition);
    vs_latlon = radians(vertexPosition);
    vs_position = modelViewProjection * position;
    vs_vertexInfo = vertexInfo;
    gl_Position = vs_position;
}
