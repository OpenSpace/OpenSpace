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
in vec4 vs_normal;
in vec4 vs_position;

uniform vec4 campos;
uniform vec4 objpos;
uniform vec3 sun_pos;
uniform bool _performShading = true;
uniform float transparency;
uniform int shadows;
uniform float time;
uniform sampler2D texture1;


Fragment getFragment() {
    vec4 position = vs_position;
    float depth = pscDepth(position);
    vec4 diffuse = texture(texture1, vs_st);

    Fragment frag;
    if (_performShading) {
        const vec3 n = normalize(vs_normal.xyz);
        const vec3 l_pos = vec3(sun_pos); // sun.
        const vec3 l_dir = normalize(l_pos - objpos.xyz);
        const float intensity = min(max(5 * dot(n,l_dir), 0.0), 1);
        
        // float shine = 0.0001;

        const vec4 specular = vec4(0.5);
        const vec4 ambient = vec4(0.0, 0.0, 0.0, transparency);
        /*
        if(intensity > 0.f){
            // halfway vector
            vec3 h = normalize(l_dir + e);
            // specular factor
            float intSpec = max(dot(h,n),0.0);
            spec = specular * pow(intSpec, shine);
        }
        */
        diffuse = max(intensity * diffuse, ambient);
    }

    frag.color.rgb = diffuse.rgb;
    frag.color.a = transparency;
    frag.depth = depth;

    return frag;
}
