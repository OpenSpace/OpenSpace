/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

in vec2 vFragPos;
in vec4 vColor;
in vec3 vConic;
in vec4 vViewPos;

Fragment getFragment() {
    // Evaluate full 2D Gaussian quadratic form: -0.5 * (a*x^2 + 2*b*x*y + c*y^2)
    float power = -0.5 * (
        vConic.x * vFragPos.x * vFragPos.x +
        2.0 * vConic.y * vFragPos.x * vFragPos.y +
        vConic.z * vFragPos.y * vFragPos.y
    );

    if (power > 0.0) {
        discard;
    }

    // vColor.a already includes globalOpacity from splat_vs.glsl
    float alpha = min(0.99, vColor.a * exp(power));
    if (alpha < 1.0 / 255.0) {
        discard;
    }

    Fragment frag;
    // Premultiplied alpha output matching GL_ONE, GL_ONE_MINUS_SRC_ALPHA
    frag.color = vec4(vColor.rgb * alpha, alpha);
    frag.depth = gl_FragCoord.z;
    frag.gPosition = vViewPos;
    frag.gNormal = vec4(0.0, 0.0, 1.0, 0.0);
    return frag;
}