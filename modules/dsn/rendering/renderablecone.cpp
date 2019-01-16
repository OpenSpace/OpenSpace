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

#include <modules/dsn/rendering/renderablecone.h>

#include <modules/base/basemodule.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/opengl/programobject.h>

namespace {
    constexpr const char* ProgramName = "ConesProgram";
    constexpr const char* _loggerCat = "RenderableCone";

    constexpr const std::array <const char*, openspace::RenderableCone::uniformCacheSize> UniformNames = {
    "modelView", "projectionTransform"};
} // namespace

namespace openspace {
RenderableCone::RenderableCone(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
{

}
void RenderableCone::initializeGL() {
    _programObject = BaseModule::ProgramObjectManager.request(
        ProgramName,
        []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
            return global::renderEngine.buildRenderProgram(
                ProgramName,
                absPath("${MODULE_DSN}/shaders/renderablecone_vs.glsl"),
                absPath("${MODULE_DSN}/shaders/renderablecone_fs.glsl")
            );
        }
    );

    ghoul::opengl::updateUniformLocations(*_programObject, _uniformCache, UniformNames);
    setRenderBin(Renderable::RenderBin::Overlay);

    // We don't need an index buffer, so we keep it at the default value of 0
    glGenVertexArrays(1, &_lateralSurfaceInfo._vaoID);
    glGenBuffers(1, &_lateralSurfaceInfo._vBufferID);

    glGenVertexArrays(1, &_baseInfo._vaoID);
    glGenBuffers(1, &_baseInfo._vBufferID);

    updateVertexAttributes();
}

void RenderableCone::deinitializeGL() {
    glDeleteVertexArrays(1, &_lateralSurfaceInfo._vaoID);
    glDeleteBuffers(1, &_lateralSurfaceInfo._vBufferID);

    glDeleteVertexArrays(1, &_baseInfo._vaoID);
    glDeleteBuffers(1, &_baseInfo._vBufferID);

    BaseModule::ProgramObjectManager.release(
        ProgramName,
        [](ghoul::opengl::ProgramObject* p) {
            global::renderEngine.removeRenderProgram(p);
        }
    );
    _programObject = nullptr;
}

bool RenderableCone::isReady() const {
    return _programObject != nullptr;
}

// Unbind buffers and arrays
inline void unbindGL() {
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);
}

void RenderableCone::updateVertexAttributes() {
    // position attributes
    glVertexAttribPointer(_vaLocVer, _sizeThreeVal, GL_FLOAT, GL_FALSE, 
                        sizeof(ColorVBOLayout) + sizeof(PositionVBOLayout),
                        (void*)0);
    glEnableVertexAttribArray(_vaLocVer);
    // color attributes
    glVertexAttribPointer(_vaLocCol, _sizeFourVal, GL_FLOAT, GL_FALSE,
                        sizeof(ColorVBOLayout) + sizeof(PositionVBOLayout),
                        (void*)(sizeof(PositionVBOLayout)));
    glEnableVertexAttribArray(_vaLocCol);
};

void RenderableCone::render(const RenderData& data, RendererTasks&) {
    _programObject->activate();
    updateUniforms(data);

    const bool usingFramebufferRenderer =
        global::renderEngine.rendererImplementation() ==
        RenderEngine::RendererImplementation::Framebuffer;

    if (usingFramebufferRenderer) {
        glDepthMask(false);
        //glBlendFunc(GL_SRC_ALPHA, GL_ONE);
    }
    //Lateral surface of the cone
    glBindVertexArray(_lateralSurfaceInfo._vaoID);
    glBindBuffer(GL_ARRAY_BUFFER, _lateralSurfaceInfo._vBufferID);
    glBufferData(
        GL_ARRAY_BUFFER,
        _vertexLateralSurfaceArray.size() * sizeof(float),
        _vertexLateralSurfaceArray.data(),
        GL_STATIC_DRAW
    );
    updateVertexAttributes();
    glFrontFace(GL_CW);
    glDrawArrays(
        GL_TRIANGLE_FAN,
        0,
        _lateralSurfaceInfo.count
    );
    glFrontFace(GL_CCW);

    //Base part of the cone
    glBindVertexArray(_baseInfo._vaoID);
    glBindBuffer(GL_ARRAY_BUFFER, _baseInfo._vBufferID);
    glBufferData(
        GL_ARRAY_BUFFER,
        _vertexBaseArray.size() * sizeof(float),
        _vertexBaseArray.data(),
        GL_STATIC_DRAW
    );
    updateVertexAttributes();
    glDrawArrays(
        GL_TRIANGLE_FAN,
        0,
        _baseInfo.count
    );

    unbindGL();

    if (usingFramebufferRenderer) {
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        glDepthMask(true);
    }
    _programObject->deactivate();
}

void RenderableCone::update(const UpdateData& data) {
    _vertexLateralSurfaceArray.clear();
    _vertexBaseArray.clear();

    glm::dvec3 earthPos = global::renderEngine.scene()->sceneGraphNode("Earth")->worldPosition();
    glm::dvec3 apex = global::renderEngine.scene()->sceneGraphNode("DSS14")->worldPosition();
    glm::dvec3 d = glm::normalize(apex - earthPos);

    double h = 20000000;
    double radius = h/2;
    int n = 8;

    std::vector<glm::dvec3> points;
    glm::dvec3 baseCenter = apex + (d * h);

    glm::dvec3 e0 = glm::cross(d, glm::dvec3(1.0, 0.0, 0.0));
    glm::dvec3 e1 = glm::cross(d, e0);

    float angleIncrement = glm::radians(360.0/n);

    for (int i = 0; i < n; ++i) {
        double rad = angleIncrement * i;
        glm::dvec3 p = baseCenter + (((e0 * glm::cos(rad)) + (e1 * glm::sin(rad))) * radius);
        points.push_back(p);
    }
    
    fillVertexArray(_vertexBaseArray, baseCenter, points);
    fillVertexArray(_vertexLateralSurfaceArray, apex, points);

    // Update the number of lines to render
    _lateralSurfaceInfo.count = static_cast<GLsizei>(_vertexLateralSurfaceArray.size() / (_sizeThreeVal + _sizeFourVal));
    _baseInfo.count = static_cast<GLsizei>(_vertexBaseArray.size() / (_sizeThreeVal + _sizeFourVal));

    unbindGL();
}

void RenderableCone::updateUniforms(const RenderData& data) {
    _programObject->setUniform(_uniformCache.modelView,
        data.camera.combinedViewMatrix() * _lateralSurfaceInfo._localTransform);
    _programObject->setUniform(_uniformCache.projection, data.camera.sgctInternal.projectionMatrix());
}

void RenderableCone::fillVertexArray(std::vector<float> &vertexArray, glm::dvec3 centerPoint, std::vector<glm::dvec3> points) {
    glm::vec4 color = { 0.0f, 0.0f, 1.0f, 0.2f };
    
    addVertexToVertexArray(vertexArray, centerPoint, color);
    for (int i = 0; i < points.size(); ++i) {
        addVertexToVertexArray(vertexArray,points[i], color);
    }
    addVertexToVertexArray(vertexArray,points[0], color);
}

void RenderableCone::addVertexToVertexArray(std::vector<float> &vertexArray,glm::dvec3 position, glm::vec4 color)
{
    vertexArray.push_back(position.x);
    vertexArray.push_back(position.y);
    vertexArray.push_back(position.z);
    vertexArray.push_back(color.r);
    vertexArray.push_back(color.g);
    vertexArray.push_back(color.b);
    vertexArray.push_back(color.a);
}

} // namespace openspace
