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

uniform mat4 modelViewProjection;
uniform vec3 maximumColor;
uniform vec3 minimumColor;
uniform float opacity;
uniform vec2 latitudeThreshold;
uniform vec2 longitudeThreshold;
uniform int dailyFlights;

out vec4 vs_position;
out vec4 vs_interpColor;
out vec2 vs_latlon;
out float vs_vertexID;
out ivec2 vs_vertexInfo;

layout (location = 0) in vec2 vertexPosition; // lat, lon
layout (location = 1) in ivec2 vertexInfo; // firstSeen, lastSeen

const float RADII = 6378137.0; // Earth is approximated as a sphere update if changed. 

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

void main() {

    const float maxFlights = 103637.0f; // Maximum daily flights in dataset
    const float minFlights = 19732.0f; // Minimum daily flights in dataset (not counting erronous data)

    vs_vertexID = float(gl_VertexID);
    vec4 position;

    position = geoToCartConversion(vertexPosition.x, vertexPosition.y, 10000.0);

    float t = clamp((float(dailyFlights) - minFlights) / (maxFlights - minFlights), 0.0, 1.0);

    vs_interpColor = vec4(minimumColor * (1.0-t) +  t * maximumColor, opacity/1000.0);
    
    vs_latlon = vertexPosition * PI / 180.0;
    vs_position = modelViewProjection * position;
    vs_vertexInfo = vertexInfo;
    gl_Position = vs_position;
}
