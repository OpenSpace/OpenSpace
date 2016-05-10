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

// Colortexture coverage
uniform sampler2D textureSamplerColor;
uniform vec2 colorSamplingScale;
uniform vec2 colorSamplingOffset;

in vec4 vs_position;
in vec3 fs_position;
in vec2 fs_uv;

#include "PowerScaling/powerScaling_fs.hglsl"
#include "fragment.glsl"

vec4 borderOverlay(vec2 uv, vec3 borderColor, float borderSize){

	vec2 uvOffset = uv - vec2(0.5);
	float thres = 0.5 - borderSize/2;
	bool isBorder = abs(uvOffset.x) > thres || abs(uvOffset.y) > thres;
	vec3 color = isBorder ? borderColor : vec3(0);
	return vec4(color, 0);
}



Fragment getFragment() {
	Fragment frag;

	vec2 samplePos = colorSamplingScale*fs_uv + colorSamplingOffset;
	frag.color = texture(textureSamplerColor, samplePos);

	// Sample position overlay
	//frag.color = frag.color * 0.9 + 0.2*vec4(samplePos, 0, 1);

	// Border overlay
	frag.color = frag.color;// + borderOverlay(fs_uv, vec3(0.5, 0.5, 0.5), 0.02);

	frag.depth = vs_position.w;

	return frag;
}

