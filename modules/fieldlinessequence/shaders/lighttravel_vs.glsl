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

out vec2 vs_st;
out vec4 vs_color;
out float vs_depth;
uniform float in_time_since_start;
/*uniform float in_dist_from_start;
uniform float in_transmission_time;
uniform float in_light_travel_time;
*/
uniform vec3 normalizedvectorFromSuntoEarth;
uniform int renderMode;
uniform float pointSize;
uniform vec4 defaultColor;
uniform vec4 LightColor;
uniform float DistanceFactor;
/*
layout(location = 1) in float in_dist_from_start;
layout(location = 2) in float in_time_since_start;
layout(location = 3) in float in_transmission_time;
layout(location = 4) in float in_light_travel_time;
*/

out float distanceFromStart;
out float timeSinceStart;
out float transmissionTime;
out float lightTravelTime;
out vec4 vs_positionScreenSpace;
out vec4 vs_gPosition;
out int rendermode;
float maxdistance = DistanceFactor * 1000000000.f;
const float lightSpeed = 299792458.0;

bool calculateDistance(vec3 inposition) {
 vec3 newpos = vec3(0, 0, 0);
 //float temptime = 200;
 //float temptime = in_time_since_start;
 newpos.x = normalizedvectorFromSuntoEarth.x * in_time_since_start * lightSpeed;
 newpos.y = normalizedvectorFromSuntoEarth.y * in_time_since_start * lightSpeed;
 newpos.z = normalizedvectorFromSuntoEarth.z * in_time_since_start * lightSpeed;
 if(distance(newpos, inposition) < maxdistance){
 //if(inposition.x > 50000000000.f){
 return true;
 }

 return false;
 
}

float smoothmotion(){
    vec3 newpos = vec3(0,0,0);
    newpos.x = normalizedvectorFromSuntoEarth.x * in_time_since_start * lightSpeed;
    newpos.y = normalizedvectorFromSuntoEarth.y * in_time_since_start * lightSpeed;
    newpos.z = normalizedvectorFromSuntoEarth.z * in_time_since_start * lightSpeed; 

    float smoothFront = smoothstep(0.0, maxdistance, distance(newpos, in_position));
    return 1 - smoothFront;
}
vec4 z_normalization(vec4 v_in) {
    vec4 v_out = v_in;
    v_out.z = 0;
    //if ( v_out.z < -v_out.w )
    //  v_out.z = -v_out.w;
    //else if (v_out.z > v_out.w)
    //  v_out.z = v_out.w;
    return v_out;
}
void main() {
    

   if(calculateDistance(in_position)){
   //float smoothFront = smoothstep(0, 199999999999.0f, in_position.x);
  // vs_color = vec4(1.0, 1.0, 1.0, 1.0);
   vs_color = LightColor;
   //vs_color.x = vs_color.x * smoothmotion();
   //vs_color.y = vs_color.y * smoothmotion();
   //vs_color.z = vs_color.z * smoothmotion();
   vs_color.a = vs_color.a * smoothmotion();
   //vs_color = vec4(0.2, 0.3, 0.4, 1.0);
   }
   else{
   //float smoothFront = smoothstep(0, 199999999999.0f, in_position.x);
   //vs_color = vec4(1.0, 1.0, 1.0, smoothFront*1.0);
   //vs_color = vec4(0.2, 0.3, 0.4, 1.0);
   if(rendermode == 3){
   vs_color = vec4(0);
   }
   else{
   vs_color = defaultColor;
   }
   }
 
   if(renderMode == 2 || rendermode == 3){
   gl_PointSize = pointSize;
   //gl_PointSize = 4;
   }
   
   //vs_color = vec4(1.0, 1.0, 1.0, 1.0);
   vec4 position_in_meters = vec4(in_position.xyz, 1);
   vec4 positionClipSpace = modelViewProjection * position_in_meters;
   //vec4 positionClipSpace = modelViewProjectionTransform * position_in_meters;
   //vec4 positionScreenSpace = z_normalization(positionClipSpace);


   //gl_Position = vec4(positionClipSpace.xy, 0, positionClipSpace.w);
   gl_Position = positionClipSpace;

   vs_depth = gl_Position.w;
   timeSinceStart = in_time_since_start;  
   /*
    lightTravelTime = in_light_travel_time;
     
    transmissionTime = in_transmission_time;
    distanceFromStart = in_dist_from_start;
    */
    //vs_gPosition = vec4(modelViewTransform * position_in_meters);
    rendermode = renderMode;
    vs_st = in_st;
}
