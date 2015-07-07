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

in vec4 in_point_position;
uniform vec3 color;

out vec4 vs_point_position;
flat out int isHour;
out vec4 vs_point_color;

uniform mat4 ViewProjection;
uniform mat4 ModelTransform;
uniform vec4 objectVelocity;
uniform vec4 lastPosition;

//this function does not consider cases where w component is negative
float psc_distance(vec4 v1, vec4 v2) {
	// reduce position numbers
	/*while(v1.w > 1 && v2.w > 1) {
		v1.w -= 1;
		v2.w -= 1;
	} */
	// get position in vec3
	while(v1.w > 1) {
		v1.xyz *= 10;
		v1.w -= 1;
	} 
	while(v2.w > 1 ) {
		v2.xyz *= 10;
		v2.w -= 1;
	}
	// using native distance function 	
	return distance(v1.xyz, v2.xyz); 
}

#include "PowerScaling/powerScaling_vs.hglsl"

void main() { 

	vec4 gray = vec4(0.6f, 0.6f, 0.6f, 0.8f);
	float cameraTooFar = 1 * pow(k, 10);
	float bigPoint = 5.f;
	float smallPoint = 2.f;
	
	vec4 tmp = in_point_position; 
	vec4 position = pscTransform(tmp, ModelTransform);
	vs_point_position = tmp;
	position = ViewProjection * position;
	gl_Position =  z_normalization(position);
	
	
	int id = gl_VertexID;
	float hour = mod(id, 4);
	
	vs_point_color.xyz = color;
	vs_point_color[3] = 1.f;
	
	vec4 v1 = campos;
	vec4 v2 = vs_point_position;
	float cameraDistance = psc_distance(v1,v2);

	vec4 temp = in_point_position;
	vec4 templast = lastPosition;
	while(temp.w > 1) {
		temp.xyz *= 10;
		temp.w -= 1;
	} 
	while(templast.w > 1) {
		templast.xyz *= 10;
		templast.w -= 1;
	} 
	float observerDistance = length(temp.xyz);
	float lastDistance = length(templast.xyz);
	
	if(hour > 0.1f)	{
		isHour = 0;
		vs_point_color = gray;
		gl_PointSize = bigPoint;	
	}
	else {	
		isHour = 1;
		gl_PointSize = bigPoint;
	}
	if (observerDistance > (lastDistance/20)) {
			gl_PointSize = smallPoint;
			//vs_point_color = gray;
	}
	/*if (cameraDistance > cameraTooFar ) {
			vs_point_color[3] = 0.0f;
			
	}*/

}