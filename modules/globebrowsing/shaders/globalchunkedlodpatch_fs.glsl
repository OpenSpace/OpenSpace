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

#include <${MODULE_GLOBEBROWSING}/shaders/tilefragcolor.hglsl>
#include "fragment.glsl"

Fragment getFragment() {
    Fragment frag;
    frag.color = getTileFragColor();

#if SHOW_CHUNK_EDGES
    frag.color += patchBorderOverlay(fs_uv, vec3(0,1,0), 0.02);
#endif // SHOW_CHUNK_EDGES
    
    // TODO: Change the color for the new deferred system (JCC)
    frag.gOtherData = vec4(waterReflectance, waterReflectance, waterReflectance, 1.0);
    // Normal is written in Camera Rig (OS Eye) Space
    frag.gNormal    = vec4(fs_normal, 1.0); 
    frag.gPosition  = vec4(positionCameraSpace, 1.0); // in Camera Rig Space

    frag.depth = fs_position.w;
    return frag;
}

