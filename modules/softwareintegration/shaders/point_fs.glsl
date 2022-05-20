/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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
#include "PowerScaling/powerScaling_fs.hglsl"

flat in float ge_colormapAttributeScalar;
in vec2 coords;
flat in float ge_screenSpaceDepth;
in vec4 ge_positionViewSpace;

uniform vec4 color;
uniform float opacity;

uniform float colormapMin;
uniform float colormapMax;
uniform bool colormapEnabled;
uniform sampler1D colormapTexture;

vec4 attributeScalarToRgb(float attributeData) {
    // Linear interpolation
    float t = (attributeData - colormapMin) / (colormapMax - colormapMin);

    // Sample texture with interpolated value
    return texture(colormapTexture, t);
}

Fragment getFragment() {
    const float radius = 0.5;
    float distance = length(coords - radius);
    if (distance > 0.6) discard;

    // calculate distance from the origin point
    float circle = smoothstep(radius, radius - (radius * 0.2), distance);

    vec4 outputColor = vec4(color.rgb, color.a * opacity); 
    if (colormapEnabled) {
        vec4 colorFromColormap = attributeScalarToRgb(ge_colormapAttributeScalar);
        outputColor = vec4(colorFromColormap.rgb, colorFromColormap.a * opacity);
    }

    Fragment frag;
    frag.color = outputColor * vec4(circle);
    frag.depth = ge_screenSpaceDepth;
    frag.gPosition = ge_positionViewSpace;
    frag.gNormal = vec4(0.0, 0.0, 0.0, 1.0);
    frag.disableLDR2HDR = true;

    return frag;
}
