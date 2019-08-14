/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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

//#include "PowerScaling/powerScaling_vs.hglsl"

layout(location = 0) in vec3 in_position;
layout(location = 1) in vec3 in_color;

//out vec3 vs_position;
out vec3 vs_color;
//out float vs_screenSpaceDepth;

//uniform mat4 model;
//uniform mat4 view;
//uniform mat4 projection;
//uniform float scaleFactor;

//uniform mat4 viewProjection;
//uniform mat4 modelViewTransform;

void main() {
    /*vec4 worldPosition = model * vec4(in_position, 1.0);
    worldPosition.w = 0.0;
    vec4 position = worldPosition; //pscTransform(worldPosition, model);
    position = pscTransform(position, mat4(1.0));
    vs_position = position.xyz;
    position = projection * view * position;
    gl_Position =  position;*/

	/*vec4 tmp = vec4(inPosition, 0.0);
    vsPosition = inPosition;
    vec4 position = pscTransform(tmp, model);
    position = projection * view * position;
    gl_Position =  z_normalization(position);*/

		//vs_position = (modelViewTransform * vec4(in_position, 1.0)).xyz;
		//vs_position = in_position;
		vs_color = in_color;

	/*vec4 positionClipSpace = vec4(projection * view * model * vec4(in_position, 1.0));
	vec4 positionScreenSpace = vec4(z_normalization(positionClipSpace));
    vs_screenSpaceDepth = positionScreenSpace.w;*/
	//vs_screenSpaceDepth = 1.0;

    //gl_PointSize = scaleFactor;
    //gl_Position = positionScreenSpace;

    // project the position to view space
    //gl_Position = viewProjection * vec4(vs_position, 1.0);
		gl_Position = vec4(in_position, 1.0);
    //gl_Position.z = 1.0;
}
