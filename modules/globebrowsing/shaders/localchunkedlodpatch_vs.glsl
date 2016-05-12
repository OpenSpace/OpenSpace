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

uniform mat4 projectionTransform;

// Input points in camera space
uniform vec3 p00;
uniform vec3 p10;
uniform vec3 p01;
uniform vec3 p11;

uniform vec3 patchNormalCameraSpace;

uniform sampler2D textureSamplerHeight;
uniform vec2 heightSamplingScale;
uniform vec2 heightSamplingOffset;

uniform float heightSamplingDepthScale;
uniform float heightSamplingDepthOffset;

layout(location = 1) in vec2 in_uv;

out vec2 fs_uv;
out vec4 fs_position;

#include "PowerScaling/powerScaling_vs.hglsl"
#include <${MODULE_GLOBEBROWSING}/shaders/ellipsoid.hglsl>

vec3 bilinearInterpolation(vec2 uv) {
	// Bilinear interpolation
	vec3 p0 = (1 - uv.x) * p00 + uv.x * p10;
	vec3 p1 = (1 - uv.x) * p01 + uv.x * p11;
	vec3 p = (1 - uv.y) * p0 + uv.y * p1;
	return p;
}

void main()
{
	fs_uv = in_uv;

	// Position in cameraspace
	vec3 p = bilinearInterpolation(fs_uv);
	
	// Transform uv coordinates to texture space
	vec2 samplePos = heightSamplingScale * in_uv + heightSamplingOffset;

	float sampledHeight = texture(textureSamplerHeight, samplePos).r;
	
	// Translate the point along normal
	p += patchNormalCameraSpace * (sampledHeight * heightSamplingDepthScale + heightSamplingDepthOffset);

	vec4 positionClippingSpace = projectionTransform * vec4(p, 1);

	gl_Position = z_normalization(positionClippingSpace);
	fs_position = gl_Position;
}