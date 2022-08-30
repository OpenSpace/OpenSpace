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

#version __CONTEXT__

#include "PowerScaling/powerScalingMath.hglsl"

layout(points) in;
layout(triangle_strip, max_vertices = 4) out;

const int MaxColors = 8;

flat in float vs_component[];
flat in int vs_nColors[];
flat in vec4 vs_colors[][MaxColors];
in dvec4 vs_dposWorld[];

flat out float gs_component;
out float gs_depthClipSpace;
out vec4 gs_positionViewSpace;
flat out int gs_nColors;
flat out vec4 gs_colors[MaxColors];
out vec2 texCoord;

// The factor used for the radius of the ring
out float gs_sizeFactor;

uniform dmat4 modelMatrix;
uniform dmat4 cameraViewProjectionMatrix;
uniform float size; // Pixels
uniform vec2 screenSize; // Pixels
uniform float maxBillboardSize; // Pixels
uniform float minBillboardSize; // Pixels
uniform bool onTop;
uniform bool useFixedRingWidth;


const vec2 corners[4] = vec2[4](
    vec2(-1.0, -1.0),
    vec2(1.0, -1.0),
    vec2(-1.0, 1.0),
    vec2(1.0, 1.0)
);

void main() {
    gs_component = vs_component[0];
    gs_colors = vs_colors[0];
    gs_nColors = vs_nColors[0];

    dvec4 dpos = vs_dposWorld[0]; //modelMatrix * dvec4(pos);
    vec4 dposClip = vec4(cameraViewProjectionMatrix * dpos);

    float scale = size;
    float screenRatio = screenSize.x / screenSize.y;

    // TODO: make billboarding optional
    vec4 scaledRightClip = scale * vec4(1.0, 0.0, 0.0, 0.0);
    vec4 scaledUpClip = scale * screenRatio * vec4(0.0, 1.0, 0.0, 0.0);

    // TODO: make it work in dome

    // Limit size of billboards based on pixel size
    vec2 halfViewSize = screenSize * 0.5;
    vec4 lowerLeft =  z_normalization(dposClip - scaledRightClip - scaledUpClip);
    vec4 upperRight = z_normalization(dposClip + scaledUpClip + scaledRightClip);

    vec2 topRight = upperRight.xy / upperRight.w;
    vec2 bottomLeft = lowerLeft.xy / lowerLeft.w;

    vec2 sizes = abs(halfViewSize * (topRight.xy - bottomLeft.xy));
    float diagonalSize = length(sizes);

    float correctionScale = 1.0;
    if (diagonalSize > maxBillboardSize) {
        correctionScale = maxBillboardSize / diagonalSize;
    }
    else if (diagonalSize < minBillboardSize) {
        correctionScale = minBillboardSize / diagonalSize;
    }
    scaledRightClip *= correctionScale;
    scaledUpClip *= correctionScale;

    // Apply component scaling lastly, to get comparable sizes
    float comp = vs_component[0];

    float sizeFactor = comp;
    if (!useFixedRingWidth) {
        // Same area:
//        sizeFactor = sqrt(2.0 * comp);

        // Ish 90% width of previous ring
        sizeFactor = 1.0;
        for (int i = 1; i < comp; i++) {
            // TODO: create a constant for the 
            sizeFactor += sqrt(pow(0.87, comp)); // This computation is not completely logical.
                                                // But it makes the result look ok. based on
                                                // trying to make each ring about 90% as wide
                                                // as the previous. The sqrt spaces them out quite nicely
        }
        gs_sizeFactor = sizeFactor;
    }

    scaledRightClip *= sizeFactor;
    scaledUpClip *= sizeFactor;


    lowerLeft = dposClip - scaledRightClip - scaledUpClip;
    vec4 lowerRight = dposClip + scaledRightClip - scaledUpClip;
    vec4 upperLeft = dposClip + scaledUpClip - scaledRightClip;
    upperRight = dposClip + scaledUpClip + scaledRightClip;
    gs_depthClipSpace = lowerLeft.w * (1 - int(onTop));

    // Lower left
    texCoord = corners[0];
    gl_Position = z_normalization(lowerLeft);
    EmitVertex();

    // Lower right
    texCoord = corners[1];
    gl_Position = z_normalization(lowerRight);
    EmitVertex();

    // Upper left
    texCoord = corners[2];
    gl_Position = z_normalization(upperLeft);
    EmitVertex();

    // Upper right
    texCoord = corners[3];
    gl_Position = z_normalization(upperRight);
    EmitVertex();

    //gl_PointSize = scale;

    EndPrimitive();
}
