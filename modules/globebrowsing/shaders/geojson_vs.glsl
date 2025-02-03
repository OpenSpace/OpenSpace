/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

layout(location = 0) in vec3 in_position;
layout(location = 1) in vec3 in_normal;
layout(location = 2) in float in_height;

out float vs_depth;
out vec3 vs_normal;
out vec4 vs_positionViewSpace;

uniform dmat4 modelTransform;
uniform dmat4 viewTransform;
uniform dmat4 projectionTransform;
uniform mat3 normalTransform;

uniform float heightOffset;
uniform bool useHeightMapData;

void main() {
    dvec4 modelPos = dvec4(in_position, 1.0);

    // Offset model pos based on height info
    if (length(in_position) > 0) {
        dvec3 outDirection = normalize(dvec3(in_position));
        float height = heightOffset;
        if (useHeightMapData) {
          height += in_height;
        }
        modelPos += dvec4(outDirection * double(height), 0.0);
    }

    vs_positionViewSpace = vec4(viewTransform * modelTransform * modelPos);
    vec4 positionScreenSpace = vec4(projectionTransform * vs_positionViewSpace);
    vs_depth = positionScreenSpace.w;
    vs_normal = normalize(normalTransform * in_normal);
    gl_Position = positionScreenSpace;

    // Set z to 0 to disable near and far plane, unique handling for perspective in space
    gl_Position.z = 0.0;
}
