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

#ifndef __OPENSPACE_CORE___PLANEGEOMETRY___H__
#define __OPENSPACE_CORE___PLANEGEOMETRY___H__

#include <ghoul/glm.h>
#include <ghoul/opengl/ghoul_gl.h>

namespace openspace {

class PlaneGeometry {
public:
    explicit PlaneGeometry(glm::vec2 size);
    explicit PlaneGeometry(float size);

    ~PlaneGeometry() = default;

    void initialize();
    void deinitialize();
    void render() const;

    void updateSize(const glm::vec2& size);
    void updateSize(const float size);

private:
    void updateGeometry();

    GLuint _vaoId = 0;
    GLuint _vBufferId = 0;
    glm::vec2 _size = glm::vec2(0.f);
};

} // namespace openspace

#endif // __OPENSPACE_CORE___PLANEGEOMETRY___H__
