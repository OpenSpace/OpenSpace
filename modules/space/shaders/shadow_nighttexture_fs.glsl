/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014 - 2017                                                             *
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

#include "PowerScaling/powerScaling_fs.hglsl"
#include "fragment.glsl"

in vec2 vs_st;
in vec2 vs_nightTex;
in vec4 vs_normal;
in vec4 vs_position;
in vec4 vs_posWorld;

const uint numberOfShadows = 1;

struct ShadowRenderingStruct {
        float xu, xp;
        float rs, rc;
        vec3 sourceCasterVec;
        vec3 casterPositionVec;
        bool isShadowing;
};

uniform ShadowRenderingStruct shadowDataArray[numberOfShadows];

uniform vec4 campos;
uniform vec4 objpos;

uniform vec3 sun_pos;

uniform bool _performShading = true;
uniform float transparency;
uniform int shadows;

uniform float time;
uniform sampler2D texture1;
uniform sampler2D nightTex;


vec4 butterworthFunc(const float d, const float r, const float n) {
    return vec4(vec3(sqrt(r/(r + pow(d, 2*n)))), 1.0);
}

vec4 calcShadow(const ShadowRenderingStruct shadowInfoArray[numberOfShadows],
                const vec3 position)
{
    if (shadowInfoArray[0].isShadowing) {
        vec3 pc = shadowInfoArray[0].casterPositionVec - position;
        // we can pass this normalized to the shader
        vec3 sc_norm = normalize(shadowInfoArray[0].sourceCasterVec);
        vec3 pc_proj = dot(pc, sc_norm) * sc_norm;
        vec3 d = pc - pc_proj;

        float length_d = length(d);
        float length_pc_proj = length(pc_proj);

        float r_p_pi = shadowInfoArray[0].rc * (length_pc_proj + shadowInfoArray[0].xp) / shadowInfoArray[0].xp;

        //float r_u_pi = shadowInfoArray[0].rc * (length_pc_proj + shadowInfoArray[0].xu) / shadowInfoArray[0].xu;
        float r_u_pi = shadowInfoArray[0].rc * (shadowInfoArray[0].xu - length_pc_proj) / shadowInfoArray[0].xu;

        if ( length_d < r_u_pi ) { // umbra
            //return vec4(0.0, 0.0, 0.0, 1.0);
            //return vec4(1.0, 0.0, 0.0, 1.0);
            return butterworthFunc(length_d, r_u_pi, 4.0);
        }
        else if ( length_d < r_p_pi ) {// penumbra
            //return vec4(0.5, 0.5, 0.5, 1.0);
            //return vec4(0.0, 1.0, 0.0, 1.0);
            return vec4(vec3(length_d/r_p_pi), 1.0);
        }
    }

    return vec4(1.0);
}

Fragment getFragment() {
    vec4 position = vs_position;
    float depth = pscDepth(position);
    vec4 diffuse = texture(texture1, vs_st);
    vec4 diffuse2 = texture(nightTex, vs_st);

    Fragment frag;
    if (_performShading) {
        const vec3 n = normalize(vs_normal.xyz);
        const vec3 l_pos = vec3(sun_pos); // sun.
        const vec3 l_dir = normalize(l_pos - objpos.xyz);
        const float intensity = min(max(5.0 * dot(n, l_dir), 0.0), 1.0);
        const float darkSide  = min(max(5.0 * dot(n, -l_dir), 0.0), 1.0);
        
        const vec4 ambient = vec4(0.0, 0.0, 0.0, transparency);
        
        const vec4 daytex = max(intensity * diffuse, ambient);
        const vec4 mixtex = mix(diffuse, diffuse2, (1.0 + dot(n, -l_dir)) / 2.0);
        
        diffuse = ((daytex*2 + mixtex) / 3) * calcShadow(shadowDataArray, vs_posWorld.xyz);
    }
    
    frag.color = vec4(diffuse.rgb, transparency);
    frag.depth = depth;

    return frag;
}
