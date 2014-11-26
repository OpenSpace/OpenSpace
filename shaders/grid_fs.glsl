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

uniform mat4 ViewProjection;
uniform mat4 ModelTransform;
uniform vec4 gridColor;

in vec2 vs_st;
//in vec3 vs_stp;
in vec4 vs_normal;
in vec4 vs_position;


#include "ABuffer/abufferStruct.hglsl"
#include "ABuffer/abufferAddToBuffer.hglsl"
#include "PowerScaling/powerScaling_fs.hglsl"

// out vec4 diffuse;
void main()
{
	
	// set the depth
	//gl_FragDepth = depth;
	//gl_FragDepth = 0.5;

	// color 
	//   diffuse = texture(texture1, vs_st);
    //diffuse = vec4(vs_position.z,0.0, 0.0, 1.0);
    // diffuse = vec4(vs_position.xyz * pow(10, vs_position.w), 1.0);
	//diffuse = vec4(vs_st, 0.0, 1.0);
    //diffuse = vec4(1.0,1.0, 0.0, 1.0);
    //diffuse = vec4(depth*5,0.0, 0.0, 1.0);
    //diffuse = vec4(vs_position.w,0.0, 0.0, 1.0);
	
	vec4 diffuse = vec4(0.4,0.4,0.4,1);
	/*if( floor(vs_st[0]) == -2){
		diffuse = gridColor*2.f;
	}else{
		diffuse = gridColor;
	}*/
	diffuse = gridColor;


	vec4 position = vs_position;
	float depth = pscDepth(position);
	// gl_FragDepth = depth;

	//ABufferStruct_t frag = createGeometryFragment(vec4(1,0,0,1), position, depth);
	ABufferStruct_t frag = createGeometryFragment(diffuse, position, depth);
	addToBuffer(frag);
	
}