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
#include "floatoperations.glsl"

// keep in sync with renderablestars.h:ColorOption enum
const int COLOROPTION_COLOR    = 0;
const int COLOROPTION_VELOCITY = 1; 
const int COLOROPTION_SPEED    = 2;
 
uniform sampler1D colorTexture;

uniform float magnitudeExponent;
uniform float colorContribution;
uniform int colorOption;

uniform int psfMethod;

uniform float p0Param;
uniform float p1Param;
uniform float p2Param;
uniform float alphaConst;

uniform float FWHM;
uniform float betaConstant;

in vec4 vs_position;
flat in vec3 ge_bvLumAbsMag;
flat in vec3 ge_velocity;
in vec2 psfCoords;
flat in float ge_speed;
flat in float ge_observationDistance;
flat in float gs_screenSpaceDepth;

vec4 bv2rgb(float bv) {
    // BV is [-0.4,2.0]
    float t = (bv + 0.4) / (2.0 + 0.4);
    return texture(colorTexture, t);
}

Fragment getFragment() {
    // Something in the color calculations need to be changed because before it was dependent
    // on the gl blend functions since the abuffer was not involved

    vec4 color = vec4(0.0);
    switch (colorOption) {
        case COLOROPTION_COLOR: 
            color = bv2rgb(ge_bvLumAbsMag.x);
            break;
        case COLOROPTION_VELOCITY:
            color = vec4(abs(ge_velocity), 0.5); 
            break;
        case COLOROPTION_SPEED:
            // @TODO Include a transfer function here ---abock
            color = vec4(vec3(ge_speed), 0.5);
            break;
    }

    vec4 fullColor = vec4(color.rgb, 1.0);
    
    if (psfMethod == 0) { 
        // PSF Functions from paper: Physically-Based Galre Effects for Digital
        // Images - Spencer, Shirley, Zimmerman and Greenberg.
        float theta = sqrt((psfCoords.y*psfCoords.y + psfCoords.x*psfCoords.x)) * 90.0;
        float f0  = 2.61E6 * exp(-pow(theta/alphaConst, 2.0));
        float f1  = 20.91/pow(theta + alphaConst, 3.0);
        float f2  = 72.37/pow(theta + alphaConst, 2.0);
        float psf_p = p0Param * f0 + p1Param * f1 + p2Param * f2;
        fullColor = vec4((colorContribution * color.rgb + psf_p) / (colorContribution + 1.0), psf_p);
        //fullColor = vec4((ge_bvLumAbsMag.z * color.rgb + psf_p) / (colorContribution + 1.0), psf_p);
        //fullColor = vec4(color.rgb * ge_bvLumAbsMag.y * 3.828f, psf_p * 800.0);
        //fullColor = color;
    } else if (psfMethod == 1) {
        // Moffat
        float r = sqrt((psfCoords.y*psfCoords.y + psfCoords.x*psfCoords.x)) * 90.0;
        float alpha = FWHM / (2.f * sqrt(pow(2.f, 1.f/betaConstant) - 1));
        float moffat_psf = pow(1.f + (r/alpha)*(r/alpha), -betaConstant);
        
        fullColor = vec4((colorContribution * color.rgb + moffat_psf) / (colorContribution + 1.0), moffat_psf);
    }
    
    
    if (fullColor.a == 0) {
        discard;
    }

    Fragment frag;
    frag.color     = fullColor;
    frag.depth     = gs_screenSpaceDepth;
    frag.gPosition = vs_position;
    frag.gNormal   = vec4(0.0, 0.0, 0.0, 1.0);
    
    return frag;
}