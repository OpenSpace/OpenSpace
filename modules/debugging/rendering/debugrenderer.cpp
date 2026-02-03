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

#include <modules/debugging/rendering/debugrenderer.h>

#include <openspace/camera/camera.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/shaderobject.h>
#include <string_view>
#include <utility>

namespace {
    constexpr std::string_view _loggerCat = "DebugRenderer";
} // namespace

namespace openspace {

DebugRenderer* DebugRenderer::_reference = nullptr;

DebugRenderer::DebugRenderer()  {
    _programObject = global::renderEngine->buildRenderProgram(
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
                                   const glm::vec4& color) const
{
    if (clippingSpacePoints.empty()) {
        return;
    }

    GLuint vbo = 0;
    glCreateBuffers(1, &vbo);
    glNamedBufferStorage(
        vbo,
        clippingSpacePoints.size() * sizeof(Vertices::value_type),
        clippingSpacePoints.data(),
        GL_NONE_BIT
    );

    GLuint vao = 0;
    glCreateVertexArrays(1, &vao);
    glVertexArrayVertexBuffer(vao, 0, vbo, 0, sizeof(glm::vec4));

    glEnableVertexArrayAttrib(vao, 0);
    glVertexArrayAttribFormat(vao, 0, 4, GL_FLOAT, GL_FALSE, 0);
    glVertexArrayAttribBinding(vao, 0, 0);

    // Activate the shader program and set the uniform color within the shader
    _programObject->activate();
    _programObject->setUniform("color", color);

    // Draw the vertices
    glBindVertexArray(vao);
    glDrawArrays(mode, 0, static_cast<GLsizei>(clippingSpacePoints.size()));
    glBindVertexArray(0);

    glDeleteVertexArrays(1, &vao);
    glDeleteBuffers(1, &vbo);
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

} // namespace openspace
