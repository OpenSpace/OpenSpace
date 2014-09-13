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

#version 400 core

uniform sampler2D texBack, texFront;
uniform sampler3D texVolume;
uniform float stepSize;

in vec3 vPosition;
in vec2 texCoords;
out vec4 fragColor;
 
void main() {
	vec3 front  = texture(texFront, texCoords).xyz;
	vec3 back 	= texture(texBack, texCoords).xyz;
	vec3 direction = back-front;
	float directionLength = length(direction);
	direction = normalize(direction);
	vec3 position = front;
	vec4 tmp, color = vec4(0);

	while (length(position-front) < directionLength && color.r != 1.0) {
		tmp = texture(texVolume, position);
		color = max(color, tmp); // MIP
		position = position + direction * stepSize;
	}

	fragColor = vec4(color.rrr, 1.0);
	
	// // DEBUG DEBUG DEBUG
	// fragColor = vec4(front, 1.0);
	// if (front.x < 0.1)
	//     fragColor = vec4(1.0);

	// if (front.y < 0.1)
	//     fragColor = vec4(1.0);

	// if (front.x > 0.9)
	//     fragColor = vec4(1.0);

	// if (front.y > 0.9)
	//     fragColor = vec4(1.0);

	// if (front.x > 0.45 && front.x < 0.55)
	//     fragColor = vec4(0.0);

	// if (front.y > 0.45 && front.y < 0.55)
	//     fragColor = vec4(0.0);
}