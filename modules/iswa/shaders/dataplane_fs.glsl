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

#include "../modules/iswa/ext/glsl-colormap/shaders/MATLAB_parula.frag";

uniform float time;
uniform sampler2D texture1;

uniform vec4 top;
uniform vec4 mid;
uniform vec4 bot;
uniform vec4 tfValues;
uniform float background;

in vec2 vs_st;
in vec4 vs_position;

#include "PowerScaling/powerScaling_fs.hglsl"
#include "fragment.glsl"

Fragment getFragment() {
    vec4 position = vs_position;
    float depth = pscDepth(position);
    vec4 diffuse;
    // diffuse = top;
    // diffuse = texture(texture1, vec2(vs_st.s, 1-vs_st.t));
    // diffuse = vec4(1,0,0,1);
    float v = texture(texture1, vec2(vs_st.s, 1-vs_st.t)).r;
    diffuse = colormap(v);

    float p = background+0.1;
    float b = background-0.1;
    if((v < p) && (p > b)){
        diffuse = vec4(0.0f, 0.0f, 0.0f, 0.0f);
    }
    //float x = tfValues.x;
    //float y = tfValues.y;

    //if(v > (x+y)){
    //    v = v - (x+y);
    //     v = v / (x-y);
    //     diffuse = mix(mid, top, v);
    // }else if( v < (x-y)){
    //     v = v / (x-y);
    //     diffuse = mix(bot, mid, v);
    // }else{
    //     diffuse = mid;
    // }

    // // vec4 diffuse = vec4(1,vs_st,1);
    // //vec4 diffuse = vec4(1,0,0,1);
    // // if(position.w > 9.0) {
    // //     diffuse = vec4(1,0,0,1);
    // // }

    // //diffuse.a = diffuse.r;
    // // float tot = diffuse.r + diffuse.g + diffuse.b;
    // // tot /= 3.0;
    // if (diffuse.a <= 0.05)
    //     discard;

    Fragment frag;
    frag.color = diffuse;
    frag.depth = depth;
    return frag;

}
