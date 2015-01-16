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

#version 430

uniform sampler2D texture1;
uniform mat4 ProjectorMatrix;
uniform mat4 ModelTransform;
uniform vec2 _scaling;
uniform vec2 radius;
flat in uint vs_segments;

in vec4 vs_position;
uniform vec3 boresight;

out vec4 color;

#define M_PI 3.14159265358979323846

vec4 uvToModel( float u, float v, vec2 radius, float segments){
	const float fj = u * segments;
	const float fi = v * segments;

	const float theta = fi * float(M_PI) / segments;  // 0 -> PI
	const float phi   = fj * float(M_PI) * 2.0f / segments;

	const float x = radius[0] * sin(phi) * sin(theta);  //
	const float y = radius[0] * cos(theta);             // up
	const float z = radius[0] * cos(phi) * sin(theta);  //

	return vec4(x, y, z, radius[1]);
}

#include "PowerScaling/powerScaling_vs.hglsl"

bool inRange(float x, float a, float b){
	return (x >= a && x <= b);
} 

void main() {
  vec2 uv = vec2(0.5,0.5)*vs_position.xy+vec2(0.5,0.5);
  
  vec4 vertex = uvToModel(uv.x, uv.y, radius, vs_segments);
  
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
 	 color = vec4(1,1,1,0);
  }
  // color.a  = 0.1f;//1.f - abs(uv.x - 0.55) / (0.6 - 0.5); // blending
}