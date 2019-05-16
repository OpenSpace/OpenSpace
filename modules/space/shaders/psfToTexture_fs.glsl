/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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

//layout(location = 1) out vec4 renderTableColor;
out vec4 renderTableColor;

uniform int psfMethod;

uniform float p0Param;
uniform float p1Param;
uniform float p2Param;
uniform float alphaConst;

uniform float FWHM;
uniform float betaConstant;

in vec2 psfCoords;

void main(void) {
    vec4 fullColor = vec4(0.0, 0.0, 0.0, 1.0);
    if (psfMethod == 0) { 
        // PSF Functions from paper: Physically-Based Galre Effects for Digital
        // Images - Spencer, Shirley, Zimmerman and Greenberg.
        float theta = sqrt((psfCoords.y*psfCoords.y + psfCoords.x*psfCoords.x)) * 90.0;
        float f0  = 2.61E6 * exp(-pow(theta/alphaConst, 2.0));
        float f1  = 20.91/pow(theta + alphaConst, 3.0);
        float f2  = 72.37/pow(theta + alphaConst, 2.0);
        float psf_p = p0Param * f0 + p1Param * f1 + p2Param * f2;
        fullColor = vec4(psf_p);
    } else if (psfMethod == 1) {
        // Moffat
        float r = sqrt((psfCoords.y*psfCoords.y + psfCoords.x*psfCoords.x)) * 90.0;
        float alpha = FWHM / (2.f * sqrt(pow(2.f, 1.f/betaConstant) - 1));
        float moffat_psf = pow(1.f + (r/alpha)*(r/alpha), -betaConstant);
        
        fullColor = vec4(moffat_psf);
    }
    
    if (fullColor.a == 0) {
        discard;
    }
    
    renderTableColor = fullColor;
}
