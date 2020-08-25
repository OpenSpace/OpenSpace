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
    float outer;
    float inner;
    
    float E = eccentricity;
    float AU = semiMajorAxis;
    float BU = AU * sqrt(1.0 - pow(E, 2.0)); // semi-minor axis
    float CU = sqrt(pow(AU, 2.0) - pow(BU, 2.0));
    float apo = AU * (1 + E);

    float AL = AU - textureOffset.x * 149597870700.0 - textureOffset.y * 149597870700.0;
    float BL = AL * sqrt(1.0 - pow(E, 2.0));
    float CL = sqrt(pow(AL, 2.0) - pow(BL, 2.0));
    float apo_inner = AL * (1 + E);

    if(eccentricity <= 0.000000){
        outer = pow(st.x , 2.0) / pow(AU/apo, 2.0)  +  ( pow(st.y, 2.0) / (pow(BU/apo, 2.0)) );
        inner = pow(st.x , 2.0) / pow(AL/apo, 2.0)  +  ( pow(st.y, 2.0) / (pow(BL/apo, 2.0)) );
    }  
    else {
        outer = ( pow((st.x + CU/apo), 2.0) ) / pow(AU/apo, 2.0)  +  ( pow(st.y, 2.0) / (pow(BU/apo, 2.0)) );
        inner = ( pow((st.x + CL/apo), 2.0) ) / pow(AL/apo, 2.0)  +  ( pow(st.y, 2.0) / (pow(BL/apo, 2.0)) );
    }
    
    if (outer > 1.0 ) // point is outside outer ellipse
        discard;
    if (inner < 1.0 ) // point is inside inner ellipse
        discard;

    // Remapping the texture coordinates
    vec2 dir = normalize(st);

    // Find outer ellipse: where along the direction is the equation = 1
    float scale;
    if(eccentricity <= 0.000000){
        scale = sqrt(  (   pow((AU/apo)*(BU/apo),2)   ) / ((pow((BU/apo)*dir.x,2))+(pow((AU/apo)*dir.y,2)))   );
    }
    else{
        float first = -( pow(BU/apo, 2.0)*dir.x*(CU/apo) ) / ( pow((BU/apo)*dir.x, 2.0) + pow((AU/apo)*dir.y, 2.0) );
        float second = pow( ( pow(BU/apo, 2.0)*dir.x*(CU/apo) )  /  ( pow((BU/apo)*dir.x, 2.0) + pow( (AU/apo)*dir.y, 2.0 ) ) , 2.0);
        float third = (  pow( (BU/apo)*(CU/apo) , 2.0 ) - pow( (AU/apo)*(BU/apo), 2.0 )   ) / (   pow( (BU/apo)*dir.x, 2.0 ) + pow( (AU/apo)*dir.y, 2.0 )   );
        scale = first + sqrt( second - third);
    }
    
    vec2 max = dir * scale;
    vec2 min = max * (apo_inner/apo);

    float distance1 = distance(max, min);
    float distance2 = distance(max, st);
    float textureCoord = distance2/distance1;

    vec4 diffuse = texture(texture1, textureCoord);
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
