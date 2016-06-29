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

#include <${MODULE_GLOBEBROWSING}/shaders/tile.hglsl>
#include <${MODULE_GLOBEBROWSING}/shaders/texturetilemapping.hglsl>
#include "PowerScaling/powerScaling_fs.hglsl"

#include "fragment.glsl"

#if USE_COLORTEXTURE
uniform Tile ColorTextures[NUMLAYERS_COLORTEXTURE];
uniform Tile ColorTexturesParent1[NUMLAYERS_COLORTEXTURE];
uniform Tile ColorTexturesParent2[NUMLAYERS_COLORTEXTURE];
#endif // USE_COLORTEXTURE

#if USE_NIGHTTEXTURE
uniform Tile NightTextures[NUMLAYERS_NIGHTTEXTURE];
uniform Tile NightTexturesParent1[NUMLAYERS_NIGHTTEXTURE];
uniform Tile NightTexturesParent2[NUMLAYERS_NIGHTTEXTURE];
#endif // USE_NIGHTTEXTURE

#if USE_OVERLAY
uniform Tile Overlays[NUMLAYERS_OVERLAY];
uniform Tile OverlaysParent1[NUMLAYERS_OVERLAY];
uniform Tile OverlaysParent2[NUMLAYERS_OVERLAY];
#endif // USE_OVERLAY

#if USE_GRAYSCALE_OVERLAY
uniform Tile GrayScaleOverlays[NUMLAYERS_GRAYSCALE_OVERLAY];
uniform Tile GrayScaleOverlaysParent1[NUMLAYERS_GRAYSCALE_OVERLAY];
uniform Tile GrayScaleOverlaysParent2[NUMLAYERS_GRAYSCALE_OVERLAY];
#endif // USE_GRAYSCALE_OVERLAY

#if USE_WATERMASK
uniform Tile WaterMasks[NUMLAYERS_WATERMASK];
uniform Tile WaterMasksParent1[NUMLAYERS_WATERMASK];
uniform Tile WaterMasksParent2[NUMLAYERS_WATERMASK];
#endif // USE_WATERMASK

#if USE_ATMOSPHERE
// TODO atmosphere uniforms here
#endif // USE_ATMOSPHERE

in vec3 ellipsoidNormalCameraSpace;
in vec4 fs_position;
in vec2 fs_uv;

in LevelWeights levelWeights;

Fragment getFragment() {
	Fragment frag;

	frag.color = vec4(0.1,0.1,0.1,1);

#if USE_COLORTEXTURE
	
	frag.color = calculateColor(
		fs_uv,
		levelWeights,//levelInterpolationParameter,
		ColorTextures,
		ColorTexturesParent1,
		ColorTexturesParent2);

#else 
	vec2 vertexResolution = vec2(64.0);
	frag.color = calculateDebugColor(fs_uv, fs_position, vertexResolution);
#endif // USE_COLORTEXTURE

#if USE_GRAYSCALE_OVERLAY
	
	frag.color = calculateGrayScaleOverlay(
		frag.color,
		fs_uv,
		levelWeights,
		GrayScaleOverlays,
		GrayScaleOverlaysParent1,
		GrayScaleOverlaysParent2);

#endif // USE_COLORTEXTURE

#if USE_WATERMASK
	// TODO: This function needs more parameters and should update the fragment color for water
	frag.color = calculateWater(
		frag.color,
		fs_uv,
		levelWeights,
		WaterMasks,
		WaterMasksParent1,
		WaterMasksParent2);

#endif // USE_WATERMASK

#if USE_NIGHTTEXTURE
	// TODO: This function needs more parameters and should update the fragment color for night texture
	frag.color = calculateNight(
		frag.color,
		fs_uv,
		levelWeights,
		NightTextures,
		NightTexturesParent1,
		NightTexturesParent2,
		ellipsoidNormalCameraSpace);

#endif // USE_NIGHTTEXTURE

#if USE_ATMOSPHERE
	// TODO: Jonathas magic goes here here
	frag.color = frag.color + vec4(0.5,0.5,1,0) * 0.3; // Just to see something for now
#endif // USE_ATMOSPHERE

#if USE_OVERLAY
	frag.color = calculateOverlay(
		frag.color,
		fs_uv,
		levelWeights,
		Overlays,
		OverlaysParent1,
		OverlaysParent2);

#endif // USE_OVERLAY

#if SHOW_CHUNK_EDGES
	frag.color += patchBorderOverlay(fs_uv, vec3(0,1,0), 0.005);
#endif // SHOW_CHUNK_EDGES

	frag.depth = fs_position.w;

	return frag;
}

