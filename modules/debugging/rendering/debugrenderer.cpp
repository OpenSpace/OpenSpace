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

#include <modules/debugging/rendering/debugrenderer.h>

#include <openspace/rendering/renderengine.h>
#include <openspace/engine/openspaceengine.h>

#include <glm/glm.hpp>
#include <memory>
#include <ostream>

#include <ghoul/misc/assert.h>
#include <iostream>

namespace {
    const std::string _loggerCat = "DebugRenderer";
}


namespace openspace {

DebugRenderer::DebugRenderer()
    : _programObject(OsEng.renderEngine().buildRenderProgram(
        "BasicDebugShader",
        "${MODULE_DEBUGGING}/rendering/debugshader_vs.glsl",
        "${MODULE_DEBUGGING}/rendering/debugshader_fs.glsl"
    ))
{
}

std::shared_ptr<DebugRenderer> DebugRenderer::ref() {
    static std::shared_ptr<DebugRenderer> renderer = std::make_shared<DebugRenderer>();
    return renderer;
}

void DebugRenderer::renderVertices(const std::vector<glm::vec4>& clippingSpacePoints, GLenum mode, glm::vec4 rgba) const {
    if (clippingSpacePoints.size() == 0) {
        return;
    }

    GLuint _vaoID;
    glGenVertexArrays(1, &_vaoID);
    ghoul_assert(_vaoID != 0, "Could not generate vertex arrays");

    GLuint _vertexBufferID;
    glGenBuffers(1, &_vertexBufferID);
    ghoul_assert(_vertexBufferID != 0, "Could not create vertex buffer");

    _programObject->activate();
    _programObject->setUniform("color", rgba);


    glBindVertexArray(_vaoID);


    // Vertex buffer
    glBindBuffer(GL_ARRAY_BUFFER, _vertexBufferID);
    glBufferData(
        GL_ARRAY_BUFFER,
        clippingSpacePoints.size() * sizeof(clippingSpacePoints[0]),
        &clippingSpacePoints[0],
        GL_STATIC_DRAW);

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(clippingSpacePoints[0]), 0);

    // uniforms



    glDrawArrays(mode, 0, clippingSpacePoints.size());
    GLenum error = glGetError();
    if (error != GL_NO_ERROR) {
        LERROR(error);
    }
        
    glBindVertexArray(0);

    glDeleteVertexArrays(1, &_vaoID);
    glDeleteBuffers(1, &_vertexBufferID);
    _programObject->deactivate();

}

void DebugRenderer::renderBoxFaces(const std::vector<glm::vec4>& clippingSpaceBoxCorners, glm::vec4 rgba) const {
    const std::vector<glm::vec4>& V = clippingSpaceBoxCorners;
    std::vector<glm::vec4> T;

    // add "sides";

    T.push_back(V[1]); T.push_back(V[0]); T.push_back(V[4]);
    T.push_back(V[4]); T.push_back(V[5]); T.push_back(V[1]);

    T.push_back(V[3]); T.push_back(V[1]); T.push_back(V[5]);
    T.push_back(V[5]); T.push_back(V[7]); T.push_back(V[3]);

    T.push_back(V[6]); T.push_back(V[3]); T.push_back(V[7]);
    T.push_back(V[3]); T.push_back(V[6]); T.push_back(V[2]);

    T.push_back(V[4]); T.push_back(V[2]); T.push_back(V[6]);
    T.push_back(V[2]); T.push_back(V[4]); T.push_back(V[0]);

    // add "top"
    T.push_back(V[5]); T.push_back(V[6]); T.push_back(V[7]);
    T.push_back(V[6]); T.push_back(V[5]); T.push_back(V[4]);

    // add bottom
    T.push_back(V[0]); T.push_back(V[1]); T.push_back(V[2]);
    T.push_back(V[3]); T.push_back(V[2]); T.push_back(V[1]);

    renderVertices(T, GL_TRIANGLES, rgba);
}

void DebugRenderer::renderBoxEdges(const std::vector<glm::vec4>& clippingSpacePoints, glm::vec4 rgba) const {
    const std::vector<glm::vec4>& V = clippingSpacePoints;
    std::vector<glm::vec4> lineVertices;
    for (size_t i = 0; i < 4; i++) {
        lineVertices.push_back(V[2 * i]);
        lineVertices.push_back(V[2 * i + 1]);
        lineVertices.push_back(V[i]);
        lineVertices.push_back(V[i + 4]);
    }
    lineVertices.push_back(V[0]); lineVertices.push_back(V[2]);
    lineVertices.push_back(V[1]); lineVertices.push_back(V[3]);
    lineVertices.push_back(V[4]); lineVertices.push_back(V[6]);
    lineVertices.push_back(V[5]); lineVertices.push_back(V[7]);
    DebugRenderer::ref()->renderVertices(lineVertices, GL_LINES, rgba);
}

    
} // namespace openspace
