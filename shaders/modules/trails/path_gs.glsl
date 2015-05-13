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

const vec2 corners[4] = vec2[4]( 
    vec2(0.0, 1.0), 
    vec2(0.0, 0.0), 
    vec2(1.0, 1.0), 
    vec2(1.0, 0.0) 
);

#include "PowerScaling/powerScalingMath.hglsl"
#include <${SHADERS_GENERATED}/constants.hglsl>:notrack

layout(points) in;
layout(points, max_vertices = 4) out;

uniform mat4 ViewProjection;
layout(location = 0) in vec4 vs_point_position[];

//out flat int isHour;
layout(location = 1) in flat int isHour[];
layout(location = 2) in vec4 vs_point_color[];

layout(location = 0) out vec4 gs_point_position;
layout(location = 1) out vec4 gs_point_color;


//out flat int isHour;

//out float billboardSize;

uniform mat4 projection;

//uniform float scaleFactor;

void main() {
  
	gs_point_color = vs_point_color[0];
	gs_point_position = vs_point_position[0];
	//gs_point_position = gl_in[0].gl_Position;
	if (isHour[0] == 1) {
	/*vec4 projPos[4];
    for (int i = 0; i < 4; ++i) {
        vec4 p1     = gl_in[0].gl_Position;
        p1.xy      += vec2(99999999*(corners[i] - vec2(0.5))); 
        projPos[i] = ViewProjection * p1;
    }

    // Calculate the positions of the lower left and upper right corners of the
    // billboard in screen-space
    const vec2 screenSize = vec2(SCREEN_WIDTH, SCREEN_HEIGHT);
    vec2 ll = (((projPos[1].xy / projPos[1].w) + 1) / 2) * screenSize;
    vec2 ur = (((projPos[2].xy / projPos[2].w) + 1) / 2) * screenSize;

    // The billboard is smaller than one pixel, we can discard it
    vec2 distance = abs(ll - ur);
    float sizeInPixels = length(distance);
    //if (sizeInPixels < 3)
    //    return;

    for(int i = 0; i < 4; i++) {
		gs_point_position = gl_in[0].gl_Position;
        gl_Position = projPos[i];
       // billboardSize = sizeInPixels;
		EmitVertex();
    }

		
	    for(int i = 0; i < 4; i++) {
        gl_Position = gl_in[0].gl_Position;;
       // billboardSize = sizeInPixels;
      
		}*/
		gl_Position =  gl_in[0].gl_Position;
		EmitVertex();
		EndPrimitive();
	}
	else {
		gl_Position =  gl_in[0].gl_Position;
		EmitVertex();
		EndPrimitive();
	return;
	}
}
