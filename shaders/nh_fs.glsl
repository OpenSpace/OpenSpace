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

uniform vec4 campos;
uniform vec4 objpos;
uniform vec3 cam_dir; // add this for specular

uniform vec3 sun_pos;

uniform bool _performShading = true;

uniform int shadows;

uniform float fading;

uniform float time;
uniform sampler2D texture1;
uniform sampler2D texture2;

in vec2 vs_st;
in vec4 vs_normal;
in vec4 vs_position;

#include "ABuffer/abufferStruct.hglsl"
#include "ABuffer/abufferAddToBuffer.hglsl"
#include "PowerScaling/powerScaling_fs.hglsl"

//#include "PowerScaling/powerScaling_vs.hglsl"
void main()
{
	vec4 position = vs_position;
	float depth = pscDepth(position);
	vec4 diffuse = texture(texture1, vs_st);
	
    vec3 normal = normalize(texture2D(texture2, vs_st).rgb * 2.0 - 1.0); 

	if (fading > 0 || fading < 1){
		diffuse.a = fading;
	}

	if (_performShading) {
		vec4 spec = vec4(0.0);
		
		vec3 n = normalize(vs_normal.xyz);
		//vec3 n = normalize(normal.xyz);
		vec3 l_pos = vec3(sun_pos); // sun.
		vec3 l_dir = normalize(l_pos-objpos.xyz);
		float intensity = min(max(1*dot(n,l_dir), 0.0), 1);
		
		float diff =  max( dot(l_dir, n), 0.0 );
		
		float shine = 100;
		/*if(normal.x > 0 && normal.y > 0 && normal.z > 0){
		    n = normalize(normal.xyz);
		}*/
		
		vec4 specular = vec4(1.0);
		vec4 ambient = diffuse*0.4;//vec4(0.0,0.0,0.0,1);
	
		if(intensity > 0.0f){
			// halfway vector
			vec3 h = normalize(l_dir + normalize(cam_dir));
			// specular factor
			float intSpec = max(dot(n,h),0.0);
			spec = specular * pow(intSpec, shine);
		}
	
		diffuse = vec4(max(intensity * diffuse , ambient).xyz,1) +spec*1.5*diffuse ;

		if (fading > 0 || fading < 1){
			diffuse.a = fading;
		}


		//diffuse = vec4(1);

		ABufferStruct_t frag = createGeometryFragment(diffuse, position, depth);
		addToBuffer(frag);
	}
	else {
		ABufferStruct_t frag = createGeometryFragment(diffuse, position, depth);
		addToBuffer(frag);	
	}
	
}