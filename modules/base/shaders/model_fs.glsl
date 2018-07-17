/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

uniform float ambientIntensity = 0.2;
uniform float diffuseIntensity = 1.0;
uniform float specularIntensity = 1.0;

uniform bool performShading = true;

uniform sampler2D texture1;

uniform int nLightSources;
uniform vec3 lightDirectionsViewSpace[8];
uniform float lightIntensities[8];

uniform float opacity = 1.0;

const vec3 SpecularAlbedo = vec3(1.0);

Fragment getFragment() {
    vec3 diffuseAlbedo = texture(texture1, vs_st).rgb;

    Fragment frag;

    if (performShading) {
        // Some of these values could be passed in as uniforms
        const vec3 lightColorAmbient = vec3(1.0);
        const vec3 lightColor = vec3(1.0);
        
        vec3 n = normalize(vs_normalViewSpace);
        vec3 c = normalize(vs_positionCameraSpace.xyz);

        vec3 color = ambientIntensity * lightColorAmbient * diffuseAlbedo;

        for (int i = 0; i < nLightSources; ++i) {
            vec3 l = lightDirectionsViewSpace[i];
            vec3 r = reflect(l, n);

            float diffuseCosineFactor = dot(n,l);
            float specularCosineFactor = dot(c,r);
            const float specularPower = 100.0;

            vec3 diffuseColor =
                diffuseIntensity * lightColor * diffuseAlbedo *
                max(diffuseCosineFactor, 0);

            vec3 specularColor =
                specularIntensity * lightColor * SpecularAlbedo *
                pow(max(specularCosineFactor, 0), specularPower);

            color += lightIntensities[i] * (diffuseColor + specularColor);
        }
        frag.color.rgb = color;
    }
    else {
        frag.color.rgb = diffuseAlbedo;
    }

    frag.color.a    = opacity;
    frag.depth      = vs_screenSpaceDepth;
    frag.gPosition  = vs_positionCameraSpace;
    frag.gNormal    = vec4(vs_normalViewSpace, 1.0);


    return frag;
}
