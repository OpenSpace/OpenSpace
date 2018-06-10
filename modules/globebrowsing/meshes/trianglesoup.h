/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___TRIANGLESOUP___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___TRIANGLESOUP___H__

#include <ghoul/glm.h>
#include <ghoul/misc/boolean.h>
#include <ghoul/opengl/ghoul_gl.h>

#include <vector>

namespace openspace::globebrowsing {

/**
 * Class to hold vertex data and handling OpenGL interfacing and rendering.
 * A <code>TriangleSoup</code> has all data needed such as position buffer and normal
 * buffer but all data is not necessarily needed for all purpouses so some vertex buffers
 * such as normals can be disabled if not needed.
 */

// TODO : Possibly render triangle strips in this class instead of triangles since
// that is faster
class TriangleSoup {
public:
    BooleanType(Positions);
    BooleanType(TextureCoordinates);
    BooleanType(Normals);

    TriangleSoup(std::vector<unsigned int> elements, // At least elements are required
        Positions usePositions = Positions::No,
        TextureCoordinates useTextures = TextureCoordinates::No,
        Normals useNormals = Normals::No);

    ~TriangleSoup();

    // Setters
    void setVertexPositions(std::vector<glm::vec4> positions);
    void setVertexTextureCoordinates(std::vector<glm::vec2> textures);
    void setVertexNormals(std::vector<glm::vec3> normals);
    void setElements(std::vector<unsigned int> elements);

    /**
    * Calls OpenGL's draw function to draw the triangles defined in the vertex buffers
    * using the current bound program object.
    * The vertex buffer attribute input locations to the shader program comes in the
    * order of positions (0), texture coordinates (1) and normals (2).
    * The input locations in the shader program should be specified to match these
    * locations.
    */
    void drawUsingActiveProgram();

protected:
    // Determines what attribute data is in use
    bool _useVertexPositions;
    bool _useTextureCoordinates;
    bool _useVertexNormals;

    struct Vertex {
        GLfloat position[3];
        GLfloat texture[2];
        GLfloat normal[3];
    };

    // Vertex data
    std::vector<Vertex> _vertexData;
    std::vector<GLuint> _elementData;

private:
    bool updateDataOnGPU();

    // GL handles
    GLuint _vaoID = 0;
    GLuint _vertexBufferID = 0;
    GLuint _elementBufferID = 0;

    bool _gpuDataNeedUpdate = false;
};

} // namespace openspace::globebrowsing

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___TRIANGLESOUP___H__
