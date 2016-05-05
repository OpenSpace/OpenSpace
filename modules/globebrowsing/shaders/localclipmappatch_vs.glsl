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

uniform ivec2 contraction; // [-1, 1]

uniform vec3 p00;
uniform vec3 p10;
uniform vec3 p01;
uniform vec3 p11;
uniform vec3 patchNormal;

uniform int segmentsPerPatch;

uniform mat3 uvTransformPatchToTileHeight00;
uniform mat3 uvTransformPatchToTileHeight10;
uniform mat3 uvTransformPatchToTileHeight01;
uniform mat3 uvTransformPatchToTileHeight11;
uniform sampler2D textureSamplerHeight00;
uniform sampler2D textureSamplerHeight10;
uniform sampler2D textureSamplerHeight01;
uniform sampler2D textureSamplerHeight11;
uniform uvec2 texture00DimensionsHeight;
uniform uvec2 texture10DimensionsHeight;
uniform uvec2 texture01DimensionsHeight;
uniform uvec2 texture11DimensionsHeight;

layout(location = 1) in vec2 in_uv;

out vec4 vs_position;
out vec3 fs_position;
out vec2 fs_uv;


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

	// Contract
	vec2 scaledContraction = contraction / float(segmentsPerPatch);
	fs_uv += scaledContraction;
	fs_uv = clamp(fs_uv, 0, 1);
	fs_uv -= scaledContraction;

	// Position in cameraspace
	vec3 p = bilinearInterpolation(fs_uv);

	float sampledHeight00 = 0;
	float sampledHeight10 = 0;
	float sampledHeight01 = 0;
	float sampledHeight11 = 0;
		
	vec2 uv00 = vec2(uvTransformPatchToTileHeight00 * vec3(fs_uv.s, fs_uv.t, 1));
	vec2 uv10 = vec2(uvTransformPatchToTileHeight10 * vec3(fs_uv.s, fs_uv.t, 1));
	vec2 uv01 = vec2(uvTransformPatchToTileHeight01 * vec3(fs_uv.s, fs_uv.t, 1));
	vec2 uv11 = vec2(uvTransformPatchToTileHeight11 * vec3(fs_uv.s, fs_uv.t, 1));

	vec2 d00 = vec2(1,1) / texture00DimensionsHeight;
	vec2 d10 = vec2(1,1) / texture10DimensionsHeight;
	vec2 d01 = vec2(1,1) / texture01DimensionsHeight;
	vec2 d11 = vec2(1,1) / texture11DimensionsHeight;

	if (uv00.x > -d00.x && uv00.x < 1 + d00.x && uv00.y > -d00.y && uv00.y < 1 + d00.y)
		sampledHeight00 = texture(textureSamplerHeight00, uv00).r;
	if (uv10.x > -d10.x && uv10.x < 1 + d10.x && uv10.y > -d10.y && uv10.y < 1 + d10.y)
		sampledHeight10 = texture(textureSamplerHeight10, uv10).r;
	if (uv01.x > -d01.x && uv01.x < 1 + d01.x && uv01.y > -d01.y && uv01.y < 1 + d01.y)
		sampledHeight01 = texture(textureSamplerHeight01, uv01).r;	
	if (uv11.x > -d11.x && uv11.x < 1 + d11.x && uv11.y > -d11.y && uv11.y < 1 + d11.y)
		sampledHeight11 = texture(textureSamplerHeight11, uv11).r;

	float sampledHeight = max(sampledHeight00, max(sampledHeight10, max(sampledHeight01, sampledHeight11)));

	p += patchNormal * sampledHeight * pow(2,15);

	vec4 position = projectionTransform * vec4(p, 1);
	fs_position = p;

	gl_Position = z_normalization(position);
	vs_position = gl_Position;
}