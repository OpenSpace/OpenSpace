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
#include "fragment.glsl"

in vec3 texCoord;
in vec4 positionCameraSpace;
out vec4 outColor;

uniform sampler3D volumeTexture;
uniform sampler1D transferFunction;
uniform vec3 volumeResolution;

Fragment getFragment() {
    // Discard fragments that lie outside the volume bounds
    if (any(lessThan(texCoord, vec3(0.0))) || any(greaterThan(texCoord, vec3(1.0)))) {
        discard;
    }

    // Fixes color artifact at the edges, TODO come up with a better solution that allows
    // lookup of texture coordinates at the extremes (0,1)
    vec3 texelSize = 1.0 / volumeResolution;
    vec3 coords = clamp(texCoord, texelSize, 1.0 - texelSize );

    Fragment frag;
    vec4 value = texture(volumeTexture, coords);
    frag.color = texture(transferFunction, value.r);

    vec4 position = positionCameraSpace;
    frag.depth = -position.z;
    // TODO: ask alex about wether or not to pre multiply alpha values and what
    // that means in terms of the interpretation of the values

    // TODO: Enable this as a property to show/hide background. Must also fix depth values
    // so that the entire background is shown. Right now trails for example are not shown.
    // Also need to fix so that the volume is shown behind the cut plane as well.
    frag.color.rgb *= frag.color.a;
    frag.color.a = 1.0;

    return frag;
}
