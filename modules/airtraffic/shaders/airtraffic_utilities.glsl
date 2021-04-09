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

#include "fragment.glsl"
#include "floatoperations.glsl"

const float RADII = 6378137.0;

bool visible(vec4 pos, mat4 modelTransform, vec3 cameraPosition) {
    
    vec3 earthPosition = vec3(0.0); // Earth it positioned at 0,0,0 in its local coordinate system

    vec3 cameraInLocal = vec3(inverse(modelTransform) * vec4(cameraPosition, 1.0));

    float cameraToFrag = length(vec3(vec3(pos) - cameraInLocal));
    float cameraToEarth = length(cameraInLocal);
    float L = sqrt(cameraToEarth * cameraToEarth - RADII * RADII);

    return cameraToFrag < L;
}

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

vec4 geoToCartConversion(float lat, float lon, float alt){

    float x = (RADII + alt) * cos(lat) * cos(lon);
    float y = (RADII + alt) * cos(lat) * sin(lon);
    float z = (RADII + alt) * sin(lat);

    return vec4(x, y, z, 1.0);
}
