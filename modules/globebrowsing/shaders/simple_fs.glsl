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

uniform vec4 campos;
uniform vec4 objpos;

uniform vec3 sun_pos;

uniform bool _performShading = true;
uniform float transparency;
uniform int shadows;

uniform float time;
uniform sampler2D texture1;
uniform sampler2D nightTex;

uniform sampler2D textureSampler00;
uniform sampler2D textureSampler10;
uniform sampler2D textureSampler01;
uniform sampler2D textureSampler11;
uniform mat3 uvTransformPatchToTile00;
uniform mat3 uvTransformPatchToTile10;
uniform mat3 uvTransformPatchToTile01;
uniform mat3 uvTransformPatchToTile11;

//uniform int segmentsPerPatch;

in vec4 vs_position;
in vec3 fs_position;
in vec2 fs_uv;


#include "PowerScaling/powerScaling_fs.hglsl"
#include "fragment.glsl"

Fragment getFragment() {
	Fragment frag;

	frag.color = vec4(0);
	vec4 color00, color10, color01, color11;

	vec2 uv00 = vec2(uvTransformPatchToTile00 * vec3(fs_uv.s, fs_uv.t, 1));
	color00 = texture(textureSampler00, uv00);

	vec2 uv10 = vec2(uvTransformPatchToTile10 * vec3(fs_uv.s, fs_uv.t, 1));
	color10 += texture(textureSampler10, uv10);
	
	vec2 uv01 = vec2(uvTransformPatchToTile01 * vec3(fs_uv.s, fs_uv.t, 1));
	color01 += texture(textureSampler01, uv01);
	
	vec2 uv11 = vec2(uvTransformPatchToTile11 * vec3(fs_uv.s, fs_uv.t, 1));
	color11 += texture(textureSampler11, uv11);

	frag.color = max(color00, max(color10, max(color01, color11))) * 10;

	//vec4 uvColor = vec4(fract(fs_uv * segmentsPerPatch), 0.4,1);
	//frag.color = frag.color.a < 0.1 ? uvColor * 0.5 : frag.color;

	frag.color = vec4(frag.color.r, frag.color.r, frag.color.r, 1);

	frag.depth =  vs_position.w;

	return frag;
}

