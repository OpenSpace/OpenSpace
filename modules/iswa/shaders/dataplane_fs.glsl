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
uniform float time;

// uniform sampler2D texture1;
uniform sampler2D textures[6];
uniform sampler2D transferFunctions[6];
// uniform sampler2D tf;
// uniform sampler2D tfs[6];

uniform int numTextures;
uniform int numTransferFunctions;
uniform bool averageValues;
uniform vec2 backgroundValues;

// uniform float background;

in vec2 vs_st;
in vec4 vs_position;

#include "PowerScaling/powerScaling_fs.hglsl"
#include "fragment.glsl"

Fragment getFragment() {
    vec4 position = vs_position;
    float depth = pscDepth(position);
    vec4 transparent = vec4(0.0f);
    vec4 diffuse = transparent;
    float v;

    for(int i=0; i<numTextures; i++){
        int j = i;
        if(numTextures > numTransferFunctions){
            j = 0;
        }
    // if(numTextures > 1){
        // for(uint i=0; i<numTextures; i++){
        v = texture(textures[i], vec2(vs_st.s, 1-vs_st.t)).r;
        
        float x = backgroundValues.x;
        float y = backgroundValues.y;
        
        vec4 color = texture(transferFunctions[j], vec2(v,0));
        if((v<(x+y)) && v>(x-y))
            color = mix(transparent, color, abs(v-x));
        diffuse += color;
    }
    // diffuse += texture(textures[1], vec2(vs_st.s, 1-vs_st.t));
    // diffuse += texture(textures[2], vec2(vs_st.s, 1-vs_st.t));
            // diffuse += texture(tf, vec2(v,0));
        // }
    // }else{
        // v = texture(texture1, vec2(vs_st.s, 1-vs_st.t)).r;
        // diffuse = texture(tf, vec2(v,0));
    // }

    if(averageValues){
        diffuse /= numTextures;
    }
    
    // diffuse = texture(tf, vec2(1-vs_st.s, 0));
    // diffuse = texture(tf, texture(texture1, vec2(vs_st.s,1-vs_st.t)).r);

    if (diffuse.a <= backgroundValues.y)
        discard;

    Fragment frag;
    frag.color = diffuse;
    frag.depth = depth;
    return frag;

}
