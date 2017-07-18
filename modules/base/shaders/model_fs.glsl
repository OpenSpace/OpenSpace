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

uniform float fading;
uniform bool performShading = true;

uniform vec3 directionToSunViewSpace;

uniform sampler2D texture1;

// Input from the vertex shader
in vec2 vs_st;
in vec3 vs_normalViewSpace;
in vec4 vs_positionCameraSpace;
in vec4 vs_positionScreenSpace;

#include "PowerScaling/powerScaling_fs.hglsl"
#include "fragment.glsl"

Fragment getFragment() {
    vec4 textureSample = texture(texture1, vs_st);
    
    vec3 diffuseAlbedo = textureSample.rgb;
    vec3 specularAlbedo = vec3(1);

    vec3 color;

    if (performShading) {
        // Some of these values could be passed in as uniforms
        vec3 lightColorAmbient = vec3(1);    
        vec3 lightColor = vec3(1);    
        
        vec3 n = normalize(vs_normalViewSpace);
        vec3 l = directionToSunViewSpace;
        vec3 c = normalize(vs_positionCameraSpace.xyz);
        vec3 r = reflect(l, n);

        float ambientIntensity = 0.2;
        float diffuseIntensity = 1;
        float specularIntensity = 1;

        float diffuseCosineFactor = dot(n,l);
        float specularCosineFactor = dot(c,r);
        float specularPower = 100;

        vec3 ambientColor = ambientIntensity * lightColorAmbient * diffuseAlbedo;
        vec3 diffuseColor = diffuseIntensity * lightColor * diffuseAlbedo * max(diffuseCosineFactor, 0);
        vec3 specularColor = specularIntensity * lightColor * specularAlbedo * pow(max(specularCosineFactor, 0), specularPower);

        color = ambientColor + diffuseColor + specularColor;
    }
    else {
        color = diffuseAlbedo;
    }

    Fragment frag;
    frag.color = vec4(color, fading);
    frag.depth = vs_positionScreenSpace.w;

    return frag;
}
