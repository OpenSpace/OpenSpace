/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

//uniform mat4 ViewProjection;
//uniform mat4 ModelTransform;

uniform mat4 modelViewProjectionTransform;
uniform vec4 objectVelocity;

layout(location = 0) in vec4 in_point_position;

//out vec4 vs_point_position;
out vec4 vs_color;
out vec4 vs_positionScreenSpace;
//out float fade;

uniform uint nVertices;
uniform vec4 shadowColor;
//uniform float lineFade;

#include "PowerScaling/powerScaling_vs.hglsl"

void main() {
    //float id = float(gl_VertexID) / float(nVertices * lineFade);
    //fade = 1.0 - id;

    if(mod(gl_VertexID,2) == 0.f){
        vs_color = shadowColor;
    }else{
        vs_color = vec4(0);
    }
    
    // Transform the damn psc to homogenous coordinate
    vec4 position = vec4(in_point_position.xyz * pow(10, in_point_position.w), 1);
    vec4 positionClipSpace = modelViewProjectionTransform * position;
    vs_positionScreenSpace = z_normalization(positionClipSpace);
    gl_Position = vs_positionScreenSpace;
}