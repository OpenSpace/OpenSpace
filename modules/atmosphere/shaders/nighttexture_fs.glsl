/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

in vec2 vs_st;
in vec2 vs_nightTex;
in vec4 vs_normal;
in vec4 vs_position;
in vec4 test;
in vec4 vs_gPosition;
in vec3 vs_gNormal;

#include "PowerScaling/powerScaling_fs.hglsl"
#include "fragment.glsl"

Fragment getFragment() {
    vec4 diffuse = texture(texture1, vs_st);
    vec4 diffuse2 = texture(nightTex, vs_st);

    Fragment frag;

    if (_performShading) {
        // directional lighting
        vec3 origin = vec3(0.0);
        vec4 spec = vec4(0.0);
        
        vec3 n = normalize(vs_normal.xyz);
        //vec3 e = normalize(camdir);
        vec3 l_pos = vec3(sun_pos); // sun.
        vec3 l_dir = normalize(l_pos-objpos.xyz);
        float intensity = min(max(5*dot(n,l_dir), 0.0), 1);
        float darkSide  = min(max(5*dot(n,-l_dir), 0.0), 1);
        
        float shine = 0.0001;

        vec4 specular = vec4(0.5);
        vec4 ambient = vec4(0.0,0.0,0.0,transparency);
        
        vec4 daytex = max(intensity * diffuse, ambient);
        vec4 mixtex = mix(diffuse, diffuse2,  (1+dot(n,-l_dir))/2);
        
        diffuse = (daytex*2 + mixtex)/3;
    }

    diffuse[3] = transparency;
    frag.color = diffuse;
    frag.depth = vs_position.w;

    // G-Buffer
    // TODO: get the write reflectance from the texture
    frag.gOtherData = vec4(diffuse.xyz, 1.0);
    frag.gPosition  = vs_gPosition;
    frag.gNormal    = vec4(vs_gNormal, 1.0);
    


    return frag;
}

