/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#version 400

layout(location = 0) in vec4 in_position;

//#include "PowerScaling/powerScaling_vs.hglsl"

uniform dmat4 inverseSgctProjectionMatrix;
uniform dmat4 objToWorldTransform;
uniform dmat4 worldToObjectTransform;
uniform dmat4 worldToEyeTransform;
uniform dmat4 eyeToWorldTransform;
uniform dmat4 eyeToViewTranform;
uniform dmat4 viewToEyeTranform;

out vec3 viewDirectionVS;
out vec4 vertexPosObjVS;

void main()
{
    //viewDirectionVS = normalize( (completeInverse * vec4((projInverse * in_position).xyz, 0.0)).xyz - cameraPosObj.xyz);

    //viewDirectionVS = normalize( (completeInverse * vec4(projInverse * in_position) ).xyz );

    //viewDirectionVS = (completeInverse * projInverse * in_position).xyz;

    vertexPosObjVS = in_position;
    gl_Position = in_position;
}
