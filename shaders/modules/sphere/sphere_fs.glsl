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

uniform float time;
uniform sampler2D texture1;
uniform float alpha;

in vec2 vs_st;
in vec4 vs_position;

#include "ABuffer/abufferStruct.hglsl"
#include "ABuffer/abufferAddToBuffer.hglsl"
#include "PowerScaling/powerScaling_fs.hglsl"

void main()
{
    vec4 position = vs_position;
    // This has to be fixed with the ScaleGraph in place (precision deficiency in depth buffer) ---abock
    // float depth = pscDepth(position);
    float depth = 1000.0;
    vec4 diffuse;

    vec2 texCoord = vs_st;
    texCoord.s = 1 - texCoord.s;
    texCoord.t = 1 - texCoord.y;

    // if(gl_FrontFacing)
        diffuse = texture(texture1, texCoord);
    // else
        // diffuse = texture(texture1, vec2(1-vs_st.s,vs_st.t));

    diffuse.a *= alpha;

    //vec4 diffuse = vec4(1,vs_st,1);
    //vec4 diffuse = vec4(1,0,0,1);
    // if(position.w > 9.0) {
    //  diffuse = vec4(1,0,0,1);
    // }


    // #if 0
    // diffuse = vec4(vs_position.xyz / 10, 1.0);
    // #else
    // // if (abs(vs_st.r - 0.75) <= 0.01 && abs(vs_st.g - 0.5) <= 0.01)
    // if (abs(vs_st.g - 0.5) <= 0.01)
    //     diffuse = vec4(vec2(vs_st), 0.0, 1.0);
    // else
    //     diffuse = vec4(0.0);
    // #endif

    // diffuse = vec4(1.0, 0.0, 0.0, 1.0);

    ABufferStruct_t frag = createGeometryFragment(diffuse, position, depth);
    addToBuffer(frag);

}