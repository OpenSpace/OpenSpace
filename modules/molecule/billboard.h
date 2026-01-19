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

#ifndef __OPENSPACE_MODULE_MOLECULE___BILLBOARD___H__
#define __OPENSPACE_MODULE_MOLECULE___BILLBOARD___H__

#include <ghoul/glm.h>
#include <ghoul/opengl/ghoul_gl.h>

void billboardGlInit();
void billboardGlDeinit();

/**
 * Draw the pre - rendered screen - space texture(colorTex, depthTex) in a circular
 * billboard.
 *
 * \param transform Model-to-screen transform of the billboard rect (model is -0.5, 0.5)
 * \param colorTex Color values to draw inside billboard
 * \param depthTex Depth values to draw inside billboard
 * \param stroke Color of the billboard outline
 * \param width Width of the billboard outline
 * \param depth Depth of the billboard outline
 * \param falloffExp Fall-off exponent of the billboard outline
 */
void billboardDraw(const glm::mat4& transform, GLuint colorTex, GLuint depthTex,
    const glm::vec4& stroke, float width, float depth, float falloffExp = 2.f);

#endif // __OPENSPACE_MODULE_MOLECULE___BILLBOARD___H__
