/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014                                                                    *
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

uniform sampler2D texture1;
uniform sampler2D texture2;
uniform mat4 ProjectorMatrix;
uniform mat4 ModelTransform;
uniform vec2 _scaling;
uniform vec4 _radius;
uniform int _segments;

uniform float projectionFading;

in vec4 vs_position;

uniform vec3 boresight;

out vec4 color;

#define M_PI 3.14159265358979323846

vec4 uvToModel( float u, float v, vec4 radius, float segments){
    float fj = u * segments;
    float fi = (1.0 - v) * segments;

    float theta = fi * float(M_PI) / segments;  // 0 -> PI
    float phi   = fj * float(M_PI) * 2.0f / segments;

    float x = radius[0] * sin(phi) * sin(theta);  //
    float y = radius[1] * cos(theta);             // up 
    float z = radius[2] * cos(phi) * sin(theta);  //

    return vec4(x, y, z, radius[3]);

    return vec4(0.0); 
}

#include "PowerScaling/powerScaling_vs.hglsl"

bool inRange(float x, float a, float b){
    return (x >= a && x <= b);
} 

void main() {
  vec2 uv = vec2(0.5,0.5)*vs_position.xy+vec2(0.5,0.5);
  
  vec4 vertex = uvToModel(uv.x, uv.y, _radius, _segments);
  
  vec4 raw_pos   = psc_to_meter(vertex, _scaling);
  vec4 projected = ProjectorMatrix * ModelTransform * raw_pos;
    
  projected.x /= projected.w;
  projected.y /= projected.w;
  
  vec3 normal = normalize((ModelTransform*vec4(vertex.xyz,0)).xyz);
  
  vec3 v_b = normalize(boresight);
  
  if((inRange(projected.x, 0, 1) &&  
      inRange(projected.y, 0, 1)) &&
      dot(v_b, normal) < 0 ) {
        color = texture(texture1, projected.xy);
  }else{
      color = texture(texture2, uv);
     color.a = projectionFading;
  }
  
  // color.a  = 0.1f;//1.f - abs(uv.x - 0.55) / (0.6 - 0.5); // blending
}