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

#include <${MODULE_GLOBEBROWSING}/shaders/texturetile.hglsl>
#include <${MODULE_GLOBEBROWSING}/shaders/texturetilemapping.hglsl>
#include "PowerScaling/powerScaling_fs.hglsl"
#include "fragment.glsl"

uniform TextureTile colorTiles[NUMLAYERS_COLORTEXTURE];
uniform TextureTile colorTilesParent1[NUMLAYERS_COLORTEXTURE];
uniform TextureTile colorTilesParent2[NUMLAYERS_COLORTEXTURE];

// tileInterpolationParameter is used to interpolate between a tile and its parent tiles
// The value increases with the distance from the vertex (or fragment) to the camera
in float tileInterpolationParameter;

in vec4 fs_position;
in vec2 fs_uv;

Fragment getFragment() {
	Fragment frag;

	frag.color = vec4(1,1,1,1);

#if USE_COLORTEXTURE

	frag.color = calculateColor(
		fs_uv,
		tileInterpolationParameter,
		colorTiles,
		colorTilesParent1,
		colorTilesParent2);

#endif // USE_COLORTEXTURE

	frag.depth = fs_position.w;

	return frag;
}

