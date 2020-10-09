/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

uniform sampler1D discTexture;
uniform vec2 textureOffset;
uniform float opacity;
uniform float eccentricity;
uniform float semiMajorAxis;

const float AstronomicalUnit = 149597870700.0; // m
const float Epsilon = 0.0000001;

// Compute semi minor axis from major axis, a, and eccentricity, e
float semiMinorAxis(float a, float e) {
    return a * sqrt(1.0 - e * e);
}

// If returned value <= 1, the point is insdie or on the ellipse specified by the input:
// a and b are the semi-major and semi-minor axes, respectively.
// cx is the displacement of the center of the ellipse along the x-axis (for an orbit,
// the y-displacement is always zero)
float ellipseTest(vec2 point, float a, float b, float cx) {
    float x = point.x;
    float y = point.y;
    return (pow(x - cx, 2.0) / (a*a)) + ((y*y) / (b*b));
}

Fragment getFragment() {
    // Moving the origin to the center
    vec2 st = (vs_st - vec2(0.5)) * 2.0;

    float AUpper = semiMajorAxis;
    float BUpper = semiMinorAxis(AUpper, eccentricity);
    float CUpper = sqrt(AUpper*AUpper - BUpper*BUpper);
    float outerApoapsisDistance = AUpper * (1 + eccentricity);

    float ALower = AUpper - AstronomicalUnit * (textureOffset.x + textureOffset.y);
    float BLower = semiMinorAxis(ALower, eccentricity);
    float CLower = sqrt(ALower*ALower - BLower*BLower);
    float innerApoapsisDistance = ALower * (1 + eccentricity);

    // Normalize based on outer apoapsis distance (size of plane)
    float AU_n = AUpper / outerApoapsisDistance;
    float BU_n = BUpper / outerApoapsisDistance;
    float CU_n = CUpper / outerApoapsisDistance;
    float AL_n = ALower / outerApoapsisDistance;
    float BL_n = BLower / outerApoapsisDistance;
    float CL_n = CLower / outerApoapsisDistance;

    if (eccentricity <= Epsilon) {
        CU_n = 0.0;
        CL_n = 0.0;
    }

    float outer = ellipseTest(st, AU_n, BU_n, -CU_n);
    float inner = ellipseTest(st, AL_n, BL_n, -CL_n);
    if (outer > 1.0 || inner < 1.0) {
        // point is outside outer ellipse or inside inner eliipse
        discard;
    }

    // Remapping the texture coordinates
    vec2 dir = normalize(st);

    // Find outer ellipse: where along the direction does the equation == 1?
    float denominator = pow(BU_n * dir.x, 2.0) + pow(AU_n * dir.y, 2.0);
    float first = -(pow(BU_n, 2.0) * dir.x * CU_n) / denominator;
    float second = pow((pow(BU_n, 2.0) * dir.x * CU_n)  /  denominator, 2.0);
    float third = (pow(BU_n * CU_n, 2.0) - pow(AU_n * BU_n, 2.0)) / denominator;

    float scale = first + sqrt(second - third);

    vec2 max = dir * scale;
    vec2 min = max * (innerApoapsisDistance / outerApoapsisDistance);

    float distance1 = distance(max, min);
    float distance2 = distance(max, st);
    float textureCoord = distance2 / distance1;

    vec4 diffuse = texture(discTexture, textureCoord);
    diffuse.a *= opacity;

    Fragment frag;
    frag.color = diffuse;
    frag.depth = vs_position.w;
    return frag;
}
