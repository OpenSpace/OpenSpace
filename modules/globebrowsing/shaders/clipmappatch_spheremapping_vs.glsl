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

uniform mat4 modelViewProjectionTransform;
uniform vec3 radiiSquared;

uniform vec2 minLatLon;
uniform vec2 lonLatScalingFactor;
uniform ivec2 contraction; // [-1, 1]

uniform mat3 uvTransformPatchToTile00;
uniform mat3 uvTransformPatchToTile10;
uniform mat3 uvTransformPatchToTile01;
uniform mat3 uvTransformPatchToTile11;

uniform int segmentsPerPatch;

uniform sampler2D textureSampler00;
uniform sampler2D textureSampler10;
uniform sampler2D textureSampler01;
uniform sampler2D textureSampler11;

layout(location = 1) in vec2 in_uv;

out vec4 vs_position;
out vec3 fs_position;
out vec2 fs_uv;


#include "PowerScaling/powerScaling_vs.hglsl"
#include <${MODULE_GLOBEBROWSING}/shaders/ellipsoid.hglsl>

PositionNormalPair globalInterpolation(vec2 uv) {
	vec2 lonLatInput;
	lonLatInput.y = minLatLon.y + lonLatScalingFactor.y * uv.y; // Lat
	lonLatInput.x = minLatLon.x + lonLatScalingFactor.x * uv.x; // Lon
	return geodetic2ToCartesian(lonLatInput.y, lonLatInput.x, radiiSquared);;
}

void main()
{
	fs_uv = in_uv;

	// Contract
	vec2 scaledContraction = contraction / float(segmentsPerPatch);
	fs_uv += scaledContraction;
	fs_uv = clamp(fs_uv, 0, 1);
	fs_uv -= scaledContraction;

	PositionNormalPair pair = globalInterpolation(fs_uv);

	float sampledHeight00, sampledHeight10, sampledHeight01, sampledHeight11;

	vec2 uv00 = vec2(uvTransformPatchToTile00 * vec3(fs_uv.s, fs_uv.t, 1));
	sampledHeight00 = texture(textureSampler00, uv00).r;
	vec2 uv10 = vec2(uvTransformPatchToTile10 * vec3(fs_uv.s, fs_uv.t, 1));
	sampledHeight10 = texture(textureSampler10, uv10).r;
	vec2 uv01 = vec2(uvTransformPatchToTile01 * vec3(fs_uv.s, fs_uv.t, 1));
	sampledHeight01 = texture(textureSampler01, uv01).r;
	vec2 uv11 = vec2(uvTransformPatchToTile11 * vec3(fs_uv.s, fs_uv.t, 1));
	sampledHeight11 = texture(textureSampler11, uv11).r;

	float sampledHeight = max(sampledHeight00, max(sampledHeight10, max(sampledHeight01, sampledHeight11)));

	pair.position += pair.normal * sampledHeight * 1e5;

	vec4 position = modelViewProjectionTransform * vec4(pair.position, 1);
	fs_position = pair.position;

	gl_Position = z_normalization(position);
	vs_position = gl_Position;
}