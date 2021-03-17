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
layout (line_strip, max_vertices = 88) out;

const float EPSILON = 1e-5;
const float RADII = 6378137.0; // Earth is approximated as a sphere, update if changed. 
const float THRESHOLD = -9998;

uniform mat4 modelViewProjection;
uniform vec2 latitudeThreshold;
uniform vec2 longitudeThreshold;

in vec4 vs_interpColor[];
in vec2 vs_latlon[];
in float vs_vertexID[];


out vec4 ge_position;
out vec4 ge_interpColor;


float greatCircleDistance(float lat1, float lon1, float lat2, float lon2) {
    // distance between latitudes 
    // and longitudes 
    float dLat = (lat2 - lat1); 
    float dLon = (lon2 - lon1);
  
    // apply formulae 
    float a = pow(sin(dLat / 2.f), 2.f) +  pow(sin(dLon / 2.f), 2.f) *  cos(lat1) * cos(lat2); 
     
    float c = 2 * asin(sqrt(a)); 
 
    return RADII * c; 
}

vec2 findIntermediatePoint(vec2 latlon1, vec2 latlon2, float f) {
    vec2 latlonR1 = latlon1 * PI / 180.0;
    vec2 latlonR2 = latlon2 * PI / 180.0;

    float phi1 = latlonR1.x; float lambda1 = latlonR1.y;
    float phi2 = latlonR2.x; float lambda2 = latlonR2.y;

    float delta = greatCircleDistance(phi1, lambda1, phi2, lambda2) / RADII; 
    
    float a = (sin(1-f)*delta) / sin(delta);
    float b = sin(f*delta) / sin(delta);

     float x = a*cos(phi1)*cos(lambda1) +  b*cos(phi2)*cos(lambda2);
     float y = a*cos(phi1)*sin(lambda1) +  b*cos(phi2)*sin(lambda2);
     float z = a*sin(phi1) + b*sin(phi2);
     
     float phi3 = atan(z,sqrt(x*x+y*y));
     float lambda3 = atan(y,x);

     return vec2(phi3, lambda3);
}

vec4 geoToCartConversion(float lat, float lon, float alt){

    float x = (RADII + alt) * cos(lat) * cos(lon);
    float y = (RADII + alt)* cos(lat) * sin(lon);
    float z = (RADII + alt)* sin(lat);

    return vec4(x, y, z, 1.0);
}


 void main(){

    // remove seam
    if(length(latitudeThreshold - vec2(-90.f, 90.f)) < EPSILON &&
       length(longitudeThreshold - vec2(-180.f, 180.f)) < EPSILON) return;

    vec2 position1 = vec2(latitudeThreshold.x, longitudeThreshold.x) * PI / 180.0;
    vec2 position2 = vec2(latitudeThreshold.y, longitudeThreshold.x) * PI / 180.0;
    vec2 position3 = vec2(latitudeThreshold.y, longitudeThreshold.y) * PI / 180.0;
    vec2 position4 = vec2(latitudeThreshold.x, longitudeThreshold.y) * PI / 180.0;

    // Sets the "altitude" of the boundaries
    float alt = 100000;

    // Sets the number of points per line segment
    float nPoints = 20;

    // Mid points
    for(int i = 0; i <= nPoints; ++i) {
        vec2 point = position1 + float(i)/nPoints*(position2-position1);
        vec4 position = geoToCartConversion(point.x, point.y, alt);
        ge_position = modelViewProjection * position;
        ge_interpColor = vs_interpColor[0];
        gl_Position = ge_position;
        EmitVertex();
    }

    for(int i = 0; i <= nPoints; ++i) {
        vec2 point = position2 + float(i)/nPoints*(position3-position2);
        vec4 position = geoToCartConversion(point.x, point.y, alt);
        ge_position = modelViewProjection * position;
        ge_interpColor = vs_interpColor[0];
        gl_Position = ge_position;
        EmitVertex();
    }

    for(int i = 0; i <= nPoints; ++i) {
        vec2 point = position3 + float(i)/nPoints*(position4-position3);
        vec4 position = geoToCartConversion(point.x, point.y, alt);
        ge_position = modelViewProjection * position;
        ge_interpColor = vs_interpColor[0];
        gl_Position = ge_position;
        EmitVertex();
    }

    for(int i = 0; i <= nPoints; ++i) {
        vec2 point = position4 + float(i)/nPoints*(position1-position4);
        vec4 position = geoToCartConversion(point.x, point.y, alt);
        ge_position = modelViewProjection * position;
        ge_interpColor = vs_interpColor[0];
        gl_Position = ge_position;
        EmitVertex();
    }
    
    EndPrimitive();
 }
