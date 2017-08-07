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

#include "fragment.glsl"

in vec2 vs_st;
in vec3 vs_normalViewSpace;
in vec4 vs_positionCameraSpace;
in float vs_screenSpaceDepth;

uniform bool performShading = true;
uniform vec3 directionToSunViewSpace;
uniform sampler2D texture1;

const vec3 SpecularAlbedo = vec3(1.0);

Fragment getFragment() {
    vec3 diffuseAlbedo = texture(texture1, vs_st).rgb;

    Fragment frag;

    if (performShading) {
        // Some of these values could be passed in as uniforms
        const vec3 lightColorAmbient = vec3(1.0);
        const vec3 lightColor = vec3(1.0);
        
        vec3 n = normalize(vs_normalViewSpace);
        vec3 l = directionToSunViewSpace;
        vec3 c = normalize(vs_positionCameraSpace.xyz);
        vec3 r = reflect(l, n);

        const float ambientIntensity = 0.2;
        const float diffuseIntensity = 1;
        const float specularIntensity = 1;

        float diffuseCosineFactor = dot(n,l);
        float specularCosineFactor = dot(c,r);
        const float specularPower = 100.0;

        vec3 ambientColor = ambientIntensity * lightColorAmbient * diffuseAlbedo;
        vec3 diffuseColor = diffuseIntensity * lightColor * diffuseAlbedo * max(diffuseCosineFactor, 0);
        vec3 specularColor = specularIntensity * lightColor * SpecularAlbedo * pow(max(specularCosineFactor, 0), specularPower);

        frag.color.rgb = ambientColor + diffuseColor + specularColor;
    }
    else {
        frag.color.rgb = diffuseAlbedo;
    }

    frag.color.a = 1.0;
    frag.depth = vs_screenSpaceDepth;

    return frag;
}
