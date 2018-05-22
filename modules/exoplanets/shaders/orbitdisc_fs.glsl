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

#include "PowerScaling/powerScaling_fs.hglsl"
#include "fragment.glsl"

in vec2 vs_st;
in vec4 vs_position;
//in vec4 vs_gPosition;
//in vec3 vs_gNormal;

uniform sampler1D texture1;
uniform vec2 textureOffset;
uniform float transparency;
uniform float eccentricity;
uniform float semiMajorAxis;

uniform bool hasSunPosition;
uniform vec3 sunPosition;
//uniform float _nightFactor;


Fragment getFragment() {
    // Moving the origin to the center
    vec2 st = (vs_st - vec2(0.5)) * 2.0;
    

    // The length of the texture coordinates vector is our distance from the center
    float radius;
    float radius_inner;

    float semiMinorAxis = semiMajorAxis * sqrt(1.0 - pow(eccentricity, 2.0));
    float focus = sqrt(pow(semiMajorAxis, 2.0) - pow(semiMinorAxis, 2.0));
    float apoapsisDistance = semiMajorAxis * (1 + eccentricity);

    float semiMajorAxis_inner = semiMajorAxis - textureOffset.x * 149597870700.0 - textureOffset.y * 149597870700.0;
    float semiMinorAxis_inner = semiMajorAxis_inner * sqrt(1.0 - pow(eccentricity, 2.0));
    float focus_inner = sqrt(pow(semiMajorAxis_inner, 2.0) - pow(semiMinorAxis_inner, 2.0));

    if(eccentricity <= 0.0){
        radius = length(st);
        radius_inner = pow(st.x , 2.0) / pow(semiMajorAxis_inner/apoapsisDistance, 2.0)   +   ( pow(st.y, 2.0) / (pow(semiMajorAxis_inner/apoapsisDistance, 2.0)) );
    }  
    else {
        radius = ( pow((st.x + focus/apoapsisDistance), 2.0) ) / pow(semiMajorAxis/apoapsisDistance, 2.0)   +   ( pow(st.y, 2.0) / (pow(semiMinorAxis/apoapsisDistance, 2.0)) );
        radius_inner = ( pow((st.x + focus_inner/apoapsisDistance), 2.0) ) / pow(semiMajorAxis_inner/apoapsisDistance, 2.0)   +   ( pow(st.y, 2.0) / (pow(semiMinorAxis_inner/apoapsisDistance, 2.0)) );
    }
    
    // We only want to consider ring-like objects so we need to discard everything outside the largest radius
    if (radius > 1.0  )
        discard;
    // We also need to discard ecerthing inside the smallest radius
    if (radius_inner < 1.0 )
        discard;

    // Remapping the texture coordinates
    // Radius \in [0,1],  texCoord \in [textureOffset.x, textureOffset.y]
    // textureOffset.x -> 0
    // textureOffset.y -> 1

    // textureOffset is not the same as it was when the following row was written.
    // But as long as the texture is one color it doesnt seem to matter.
    float texCoord = (radius - textureOffset.x) / (textureOffset.y - textureOffset.x);

    vec4 diffuse = texture(texture1, texCoord);
    float colorValue = length(diffuse.rgb);
    // times 3 as length of vec3(1.0, 1.0, 1.0) will return 3 and we want
    // to normalize the transparency value to [0,1]
    if (colorValue < 3.0 * transparency) {
        diffuse.a = pow(colorValue / (3.0 * transparency), 1);
    }

    // The normal for the one plane depends on whether we are dealing
    // with a front facing or back facing fragment
    vec3 normal;
    // The plane is oriented on the xz plane
    // WARNING: This might not be the case for Uranus
    if (gl_FrontFacing) {
        normal = vec3(-1.0, 0.0, 0.0);
    }
    else {
        normal = vec3(1.0, 0.0, 0.0);
    }


    Fragment frag;
    frag.color = diffuse;
    frag.depth = vs_position.w;

    //frag.gPosition  = vs_gPosition;
    //frag.gNormal    = vs_gNormal;

    return frag;
}
