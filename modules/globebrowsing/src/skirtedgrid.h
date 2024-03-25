/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___SKIRTEDGRID___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___SKIRTEDGRID___H__

#include <ghoul/glm.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <vector>

namespace openspace::globebrowsing {

/**
 * This grid is a regular grid with skirts around its edges. The areas covered by the
 * skirts have position coordinates and texture coordinates that are outside of the range
 * [0, 1]. The width of the skirts is half the size of one segment width or a cell.
 */
class SkirtedGrid {
public:
    /**
     * \param xSeg is the number of grid cells in the x direction
     * \param ySeg is the number of grid cells in the y direction
     */
    SkirtedGrid(unsigned int xSeg, unsigned int ySeg);
    ~SkirtedGrid() = default;

    void initializeGL();
    void deinitializeGL();

    /**
     * Calls OpenGL's draw function to draw the triangles defined in the vertex buffers
     * using the current bound program object. The vertex buffer attribute input locations
     * to the shader program comes in the order of positions (0), texture coordinates (1)
     * and normals (2). The input locations in the shader program should be specified to
     * match these locations.
     */
    void drawUsingActiveProgram() const;

    const int xSegments;
    const int ySegments;

private:
    GLuint _vaoID = 0;
    GLuint _vertexBufferID = 0;
    GLuint _elementBufferID = 0;
    const GLsizei _elementSize;
};

} // namespace openspace::globebrowsing

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___SKIRTEDGRID___H__
