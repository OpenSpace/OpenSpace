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
layout (line_strip, max_vertices = 44) out;

const float EPSILON = 1e-5;
const float RADII = 6378137.0; // Earth is approximated as a sphere update if changed. 
const float THRESHOLD = -9998;

uniform mat4 modelViewProjection;
uniform float opacity;
uniform vec2 latitudeThreshold;
uniform vec2 longitudeThreshold;

//in float vs_vertexID[];
in vec4 vs_position[];
in vec4 vs_interpColor[];
in vec2 vs_latlon[];
in float vs_vertexID[];
in int vs_identifier[];

out vec4 ge_position;
out vec4 ge_interpColor;

/*vec2 findMidPoint(vec2 latlon1, vec2 latlon2) {
    
    vec2 latlonR1 = latlon1 * PI / 180.0;
    vec2 latlonR2 = latlon2 * PI / 180.0;

    float phi1 = latlonR1.x; float lambda1 = latlonR1.y;
    float phi2 = latlonR2.x; float lambda2 = latlonR2.y;


    float Bx = cos(phi2) * cos(lambda2 - lambda1);
    float By = cos(phi2) * sin(lambda2 - lambda1);
    float phi3 = atan( sin(phi1) + sin(phi2), sqrt( (cos(phi1) + Bx) * (cos(phi1) + Bx) + By * By ) );
    float lambda3 = lambda1 + atan(By, cos(phi1) + Bx);

    return vec2(phi3, lambda3);
}*/

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
    
    // Box
    if(vs_identifier[0] == 1 && vs_identifier[1] == 1) {
        vec2 position1 = vec2(latitudeThreshold.x, longitudeThreshold.x) * PI / 180.0;
        vec2 position2 = vec2(latitudeThreshold.y, longitudeThreshold.x) * PI / 180.0;
        vec2 position3 = vec2(latitudeThreshold.y, longitudeThreshold.y) * PI / 180.0;
        vec2 position4 = vec2(latitudeThreshold.x, longitudeThreshold.y) * PI / 180.0;

        float alt = 100000;

        // Mid points
        for(int i = 0; i <= 10; ++i) {
            //vec2 point = position1 * (1-i/10) + position2 * i/10;
            vec2 point = position1 + float(i)/10*(position2-position1);
            //vec2 point = findIntermediatePoint(position1, position2, float(i)/10.f);
            vec4 position = geoToCartConversion(point.x, point.y, alt);
            ge_position = modelViewProjection * position;
            ge_interpColor = vs_interpColor[0];
            gl_Position = ge_position;
            EmitVertex();
        }

        for(int i = 0; i <= 10; ++i) {
            //vec2 point = position2 * (1-i/10) + position3 * i/10;
            vec2 point = position2 + float(i)/10.f*(position3-position2);
            //vec2 point = findIntermediatePoint(position2, position3, float(i)/10.f);
            vec4 position = geoToCartConversion(point.x, point.y, alt);
            ge_position = modelViewProjection * position;
            ge_interpColor = vs_interpColor[0];
            gl_Position = ge_position;
            EmitVertex();
        }

        for(int i = 0; i <= 10; ++i) {
            //vec2 point = position3 * (1-i/10) + position4 * i/10;
            vec2 point = position3 + float(i)/10.f*(position4-position3);
            //vec2 point = findIntermediatePoint(position3, position4, float(i)/10.f);
            vec4 position = geoToCartConversion(point.x, point.y, alt);
            ge_position = modelViewProjection * position;
            ge_interpColor = vs_interpColor[0];
            gl_Position = ge_position;
            EmitVertex();
        }

        for(int i = 0; i <= 10; ++i) {
            //vec2 point = position4 * (1-i/10) + position1 * i/10;
            vec2 point = position4 + float(i)/10.f*(position1-position4);
            //vec2 point = findIntermediatePoint(position4, position1, float(i)/10.f);
            vec4 position = geoToCartConversion(point.x, point.y, alt);
            ge_position = modelViewProjection * position;
            ge_interpColor = vs_interpColor[0];
            gl_Position = ge_position;
            EmitVertex();
        }
    }
    else {
        if(length(gl_in[0].gl_Position) < EPSILON || length(gl_in[1].gl_Position) < EPSILON) {
             return;
        }

        // Start point
        ge_position = vs_position[0];
        ge_interpColor = vec4(vec3(vs_interpColor[0]), opacity);
        gl_Position = gl_in[0].gl_Position;
        EmitVertex();

        float alt = 10000; 
        // Mid points
        for(int i = 1; i < 10; ++i) {
            vec2 point = findIntermediatePoint(vs_latlon[0], vs_latlon[1], float(i)/10.f);
            vec4 position = geoToCartConversion(point.x, point.y, alt);
            ge_position = modelViewProjection * position;
            float midOpacity = 0.2;
            ge_interpColor = vec4(vec3(vs_interpColor[0]), midOpacity*opacity);
            gl_Position = ge_position;
            EmitVertex();
        }

        // End point
        ge_position = vs_position[1];
        ge_interpColor = vec4(vec3(vs_interpColor[1]), opacity);
        gl_Position = gl_in[1].gl_Position;
        EmitVertex();
    }

    EndPrimitive();
 }
