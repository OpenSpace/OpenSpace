/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014-2016                                                               *
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
//#version __CONTEXT__
uniform sampler2D texture1;
uniform float OcclusionDepth;

in vec2 vs_st;
in vec4 vs_position;

#include "fragment.glsl"
#include "PowerScaling/powerScaling_fs.hglsl"

Fragment getFragment(){
	Fragment frag;

	// power scale coordinates for depth. w value is set to 1.0.
	float depth = (1.0 + log(abs(OcclusionDepth) + 1/pow(k, 1.0))/log(k)) / 27.0;
	frag.color = texture(texture1, vs_st);
	frag.depth = depth;

	return frag;
}

/* out vec4 _out_color_;

void main() {
     Fragment f = getFragment();
     _out_color_ = f.color;
     gl_FragDepth = f.depth;
} */