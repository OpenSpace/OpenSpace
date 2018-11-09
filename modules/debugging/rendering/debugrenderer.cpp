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

#include <modules/debugging/rendering/debugrenderer.h>

#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>
#include <ghoul/opengl/programobject.h>
//#include <ostream>
//#include <iostream>

namespace {
    constexpr const char* _loggerCat = "DebugRenderer";
} // namespace

namespace openspace {

DebugRenderer* DebugRenderer::_reference = nullptr;

DebugRenderer::DebugRenderer()  {
    _programObject = global::renderEngine.buildRenderProgram(
        "BasicDebugShader",
        absPath("${MODULE_DEBUGGING}/rendering/debugshader_vs.glsl"),
        absPath("${MODULE_DEBUGGING}/rendering/debugshader_fs.glsl")
    );
}

DebugRenderer::DebugRenderer(std::unique_ptr<ghoul::opengl::ProgramObject> programObject)
    : _programObject(std::move(programObject))
{
    // nothing to do
}

DebugRenderer::~DebugRenderer() { }

const DebugRenderer& DebugRenderer::ref() {
    if (!_reference) {
        try {
            _reference = new DebugRenderer();
        }
        catch (const ghoul::opengl::ShaderObject::ShaderCompileError& e) {
            LERROR(e.what());
        }
    }
    return *_reference;
}

void DebugRenderer::renderVertices(const Vertices& clippingSpacePoints, GLenum mode,
                                   const glm::vec4& rgba) const
{
    if (clippingSpacePoints.empty()) {
        return;
    }

    // Generate a vao, vertex array object (keeping track of pointers to vbo)
    GLuint _vaoID;
    glGenVertexArrays(1, &_vaoID);
    ghoul_assert(_vaoID != 0, "Could not generate vertex arrays");

    // Generate a vbo, vertex buffer object (storeing actual data)
    GLuint _vertexBufferID;
    glGenBuffers(1, &_vertexBufferID);
    ghoul_assert(_vertexBufferID != 0, "Could not create vertex buffer");

    // Activate the shader program and set the uniform color within the shader
    _programObject->activate();
    _programObject->setUniform("color", rgba);

    glBindVertexArray(_vaoID);
    glBindBuffer(GL_ARRAY_BUFFER, _vertexBufferID);
    glBufferData(
        GL_ARRAY_BUFFER,
        clippingSpacePoints.size() * sizeof(clippingSpacePoints[0]),
        &clippingSpacePoints[0],
        GL_STATIC_DRAW);


    glEnableVertexAttribArray(0);
    glVertexAttribPointer(
        0,
        4,
        GL_FLOAT,
        GL_FALSE,
        sizeof(clippingSpacePoints[0]),
        nullptr
    );

    // Draw the vertices
    glDrawArrays(mode, 0, static_cast<GLsizei>(clippingSpacePoints.size()));

    // Clean up after the draw call was made
    glBindVertexArray(0);
    glDeleteVertexArrays(1, &_vaoID);
    glDeleteBuffers(1, &_vertexBufferID);
    _programObject->deactivate();
}

void DebugRenderer::renderBoxFaces(const Vertices& clippingSpaceBoxCorners,
                                   const glm::vec4& rgba) const
{
    ghoul_assert(clippingSpaceBoxCorners.size() == 8, "Box must have 8 vertices");

    const Vertices& V = clippingSpaceBoxCorners;

    std::vector<glm::vec4> T;

    // add "sides"
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

void DebugRenderer::renderBoxEdges(const Vertices& clippingSpaceBoxCorners,
                                   const glm::vec4& rgba) const
{
    ghoul_assert(clippingSpaceBoxCorners.size() == 8, "Box must have 8 vertices");

    const Vertices& V = clippingSpaceBoxCorners;

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

    DebugRenderer::ref().renderVertices(lineVertices, GL_LINES, rgba);
}

void DebugRenderer::renderNiceBox(const Vertices& clippingSpaceBoxCorners,
                                  const glm::vec4& rgba) const
{
    renderBoxFaces(clippingSpaceBoxCorners, rgba);

    glLineWidth(4.f);
    DebugRenderer::ref().renderBoxEdges(clippingSpaceBoxCorners, rgba);

    glPointSize(10.f);
    DebugRenderer::ref().renderVertices(clippingSpaceBoxCorners, GL_POINTS, rgba);
}

void DebugRenderer::renderCameraFrustum(const RenderData& data, const Camera& otherCamera,
                                        const glm::vec4& rgba) const
{
    using namespace glm;
//    dmat4 modelTransform = translate(dmat4(1), data.position.dvec3());
    const dmat4 viewTransform = dmat4(data.camera.combinedViewMatrix());
    const dmat4 vp = dmat4(data.camera.projectionMatrix()) * viewTransform;

    const dmat4 inverseSavedV = glm::inverse(otherCamera.combinedViewMatrix());
    const dmat4 inverseSavedP = glm::inverse(otherCamera.projectionMatrix());
    Vertices clippingSpaceFrustumCorners(8);
    // loop through the corners of the saved camera frustum
    for (size_t i = 0; i < 8; i++) {
        const bool cornerIsRight = i % 2 == 0;
        const bool cornerIsUp = i > 3;
        const bool cornerIsFar = (i / 2) % 2 == 1;

        const double x = cornerIsRight ? 1 : -1;
        const double y = cornerIsUp ? 1 : -1;
        const double z = cornerIsFar ? 1 : 0;

        // p represents a corner in the frustum of the saved camera
        const dvec4 pSavedClippingSpace(x, y, z, 1);
        dvec4 pSavedCameraSpace = inverseSavedP * pSavedClippingSpace;
        if (cornerIsFar) {
            pSavedCameraSpace.w *= 1e-7;
        }
        pSavedCameraSpace = glm::abs(1.0 / pSavedCameraSpace.w) * pSavedCameraSpace;

        const dvec4 pWorldSpace = inverseSavedV * pSavedCameraSpace;
        const dvec4 pCurrentClippingSpace = vp * pWorldSpace;
        clippingSpaceFrustumCorners[i] = pCurrentClippingSpace;
    }

    glDisable(GL_CULL_FACE);
    renderNiceBox(clippingSpaceFrustumCorners, rgba);
    glEnable(GL_CULL_FACE);
}

#ifdef OPENSPACE_MODULE_GLOBEBROWSING_ENABLED
void DebugRenderer::renderAABB2(const globebrowsing::AABB2& screenSpaceAABB,
                                const glm::vec4& rgba) const
{
    Vertices vertices = {
        glm::vec4(screenSpaceAABB.min.x, screenSpaceAABB.min.y, 1, 1),
        glm::vec4(screenSpaceAABB.min.x, screenSpaceAABB.max.y, 1, 1),
        glm::vec4(screenSpaceAABB.max.x, screenSpaceAABB.min.y, 1, 1),
        glm::vec4(screenSpaceAABB.max.x, screenSpaceAABB.max.y, 1, 1)
    };

    renderVertices(vertices, GL_LINES, rgba);
}
#endif // OPENSPACE_MODULE_GLOBEBROWSING_ENABLED

#ifdef OPENSPACE_MODULE_GLOBEBROWSING_ENABLED
const DebugRenderer::Vertices DebugRenderer::verticesFor(
                                        const globebrowsing::AABB3& screenSpaceAABB) const
{
    Vertices vertices(8);
    for (size_t i = 0; i < 8; i++) {
        const bool cornerIsRight = i % 2 == 0;
        const bool cornerIsUp = i > 3;
        const bool cornerIsFar = (i / 2) % 2 == 1;

        const double x = cornerIsRight ? screenSpaceAABB.max.x : screenSpaceAABB.min.x;
        const double y = cornerIsUp ? screenSpaceAABB.max.y : screenSpaceAABB.min.y;
        const double z = cornerIsFar ? screenSpaceAABB.max.z : screenSpaceAABB.min.z;

        vertices[i] = glm::vec4(x, y, z, 1);
    }
    return vertices;
}
#endif // OPENSPACE_MODULE_GLOBEBROWSING_ENABLED

} // namespace openspace
