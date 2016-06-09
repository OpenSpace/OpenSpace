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

#include "PowerScaling/powerScaling_vs.hglsl"
#include <${MODULE_GLOBEBROWSING}/shaders/ellipsoid.hglsl>
#include <${MODULE_GLOBEBROWSING}/shaders/tile.hglsl>
#include <${MODULE_GLOBEBROWSING}/shaders/texturetilemapping.hglsl>

uniform mat4 modelViewProjectionTransform;
uniform mat4 modelViewTransform;
uniform vec3 radiiSquared;

uniform vec2 minLatLon;
uniform vec2 lonLatScalingFactor;

uniform int xSegments;
uniform float skirtLength;

#if USE_HEIGHTMAP
uniform Tile heightTiles[NUMLAYERS_HEIGHTMAP];
uniform Tile heightTilesParent1[NUMLAYERS_HEIGHTMAP];
uniform Tile heightTilesParent2[NUMLAYERS_HEIGHTMAP];
#endif // USE_HEIGHTMAP

uniform vec3 cameraPosition;
uniform float distanceScaleFactor;
uniform int chunkLevel;

layout(location = 1) in vec2 in_uv;

out vec2 fs_uv;
out vec4 fs_position;
out vec3 ellipsoidNormalCameraSpace;
// tileInterpolationParameter is used to interpolate between a tile and its parent tiles
// The value increases with the distance from the vertex (or fragment) to the camera
out float tileInterpolationParameter;

PositionNormalPair globalInterpolation() {
	vec2 lonLatInput;
	lonLatInput.y = minLatLon.y + lonLatScalingFactor.y * in_uv.y; // Lat
	lonLatInput.x = minLatLon.x + lonLatScalingFactor.x * in_uv.x; // Lon
	PositionNormalPair positionPairModelSpace = geodetic2ToCartesian(lonLatInput.y, lonLatInput.x, radiiSquared);
	return positionPairModelSpace;
}

void main()
{
	PositionNormalPair pair = globalInterpolation();

	float distToVertexOnEllipsoid = length(pair.position - cameraPosition);
    float projectedScaleFactor = distanceScaleFactor / distToVertexOnEllipsoid;
	float desiredLevel = log2(projectedScaleFactor);

	tileInterpolationParameter = chunkLevel - desiredLevel;

	float height = 0;

#if USE_HEIGHTMAP
	
	// Calculate desired level based on distance to the vertex on the ellipsoid
    // Before any heightmapping is done
	height = calculateHeight(
		in_uv,
		tileInterpolationParameter, 							// Variable to determine which texture to sample from
		heightTiles, heightTilesParent1, heightTilesParent2);	// Three textures to sample from

#endif // USE_HEIGHTMAP

	// Skirts
	int vertexIDx = gl_VertexID % (xSegments + 3);
	int vertexIDy = gl_VertexID / (xSegments + 3);
	if (vertexIDx == 0 ||
		vertexIDy == 0 ||
		vertexIDx == (xSegments + 2) ||
		vertexIDy == (xSegments + 2) ) {
		height -= skirtLength;
	}
	
	// Add the height in the direction of the normal
	pair.position += pair.normal * height;
	
	vec4 positionClippingSpace = modelViewProjectionTransform * vec4(pair.position, 1);

	// Write output
	fs_uv = in_uv;
	fs_position = z_normalization(positionClippingSpace);
	gl_Position = fs_position;
	ellipsoidNormalCameraSpace = mat3(modelViewTransform) * pair.normal;
}