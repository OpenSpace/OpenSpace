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

uniform mat3 uvTransformPatchToTile;

uniform int segmentsPerPatch;

uniform sampler2D textureSampler;

layout(location = 1) in vec2 in_uv;

out vec4 vs_position;
out vec3 fs_position;
out vec2 fs_uv;

#include "PowerScaling/powerScaling_vs.hglsl"

struct PositionNormalPair {
	vec3 position;
	vec3 normal;
};

vec3 geodeticSurfaceNormal(float latitude, float longitude)
{
	float cosLat = cos(latitude);
	return vec3(
		cosLat * cos(longitude),
		cosLat * sin(longitude), 
		sin(latitude));
}

PositionNormalPair geodetic3ToCartesian(
	float latitude,
	float longitude,
	float height,
	vec3 radiiSquared)
{
	vec3 normal = geodeticSurfaceNormal(latitude, longitude);
	vec3 k = radiiSquared * normal;
	float gamma = sqrt(dot(k, normal));
	vec3 rSurface = k / gamma;
	PositionNormalPair toReturn;
	toReturn.position = rSurface + height * normal;
	toReturn.normal = normal;
	return toReturn;
}

PositionNormalPair geodetic2ToCartesian(float latitude, float longitude, vec3 radiiSquared)
{
	// Position on surface : height = 0
	return geodetic3ToCartesian(latitude, longitude, 0, radiiSquared);
}

vec3 latLonToCartesian(float latitude, float longitude, float radius) {
	return radius * vec3(
		cos(latitude) * cos(longitude),
		cos(latitude) * sin(longitude),
		sin(latitude));
}

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

	vec4 textureColor = texture(textureSampler, vec2(uvTransformPatchToTile * vec3(fs_uv.s, fs_uv.t, 1)));

	pair.position += pair.normal * textureColor.r * 3e4;

	vec4 position = modelViewProjectionTransform * vec4(pair.position, 1);
	fs_position = pair.position;

	gl_Position = z_normalization(position);
	vs_position = gl_Position;
}