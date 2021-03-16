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
//uniform mat4 modelViewProjectionTransform;
//uniform mat4 modelViewTransform;

layout(location = 0) in vec3 in_position;
layout(location = 1) in vec2 in_st;

out vec2        vs_st;
out vec4        vs_color;
out float       vs_depth;

uniform float   in_time_since_start;

uniform vec3    normalizedVectorFromSunToEarth;
uniform int     renderMode;
uniform float   pointSize;
uniform vec4    defaultColor;
uniform vec4    lightColor;
uniform float   distanceFactor;

out float   distanceFromStart;
out float   timeSinceStart;
out float   transmissionTime;
out float   lightTravelTime;
out vec4    vs_positionScreenSpace;
out vec4    vs_gPosition;
out int     render_mode;

float       maxDistance = distanceFactor * 1000000000.f;
const float lightSpeed = 299792458.0;

bool calculateDistance(vec3 inposition) {
    vec3 newpos = vec3(0, 0, 0);

    newpos.x = normalizedVectorFromSunToEarth.x * in_time_since_start * lightSpeed;
    newpos.y = normalizedVectorFromSunToEarth.y * in_time_since_start * lightSpeed;
    newpos.z = normalizedVectorFromSunToEarth.z * in_time_since_start * lightSpeed;

    if(newpos.y < inposition.y && newpos.x < inposition.x && newpos.z > inposition.z){
        return false;
    }
    if(distance(newpos, inposition) < maxDistance){
        return true;
    }

    return false;
}

float smoothmotion(){
    vec3 newpos = vec3(0,0,0);
    newpos.x = normalizedVectorFromSunToEarth.x * in_time_since_start * lightSpeed;
    newpos.y = normalizedVectorFromSunToEarth.y * in_time_since_start * lightSpeed;
    newpos.z = normalizedVectorFromSunToEarth.z * in_time_since_start * lightSpeed; 
    
    float smoothFront = 1 - smoothstep(0, maxDistance, distance(newpos, in_position));

    if(smoothFront < 0.95f){
        float alphaV = 0.1 * smoothFront * smoothFront;
        if(alphaV < 0.01){
            return 0; 
        }
        return alphaV;
    }
    return smoothFront;
}

vec4 z_normalization(vec4 v_in) {
    vec4 v_out = v_in;
    v_out.z = 0;
    return v_out;
}

void main() {
   if(calculateDistance(in_position)){
       vs_color = lightColor;
       vs_color.a = vs_color.a * smoothmotion();
   }
   else{
       if(renderMode == 3){
            vs_color = vec4(0);
       }
       else{
            vs_color = defaultColor;
       }
   }
 
   if(renderMode == 2 || renderMode == 3){
        gl_PointSize = pointSize;
   }
   
   vec4 position_in_meters = vec4(in_position.xyz, 1);
   vec4 positionClipSpace = modelViewProjection * position_in_meters;

   gl_Position = positionClipSpace;

   vs_depth = gl_Position.w;
   timeSinceStart = in_time_since_start;  
   render_mode = renderMode;
   vs_st = in_st;
}
