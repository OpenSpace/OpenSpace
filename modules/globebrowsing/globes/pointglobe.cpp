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

#include <modules/globebrowsing/globes/pointglobe.h>

#include <modules/globebrowsing/globes/renderableglobe.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opengl/programobject.h>

namespace {
    constexpr const std::array<const char*, 3> UniformNames = {
        "lightIntensityClamped", "modelViewTransform", "projectionTransform"
    };

    constexpr openspace::properties::Property::PropertyInfo IntensityClampInfo = {
        "IntensityClamp",
        "Intensity clamp",
        ""
    };

    constexpr openspace::properties::Property::PropertyInfo LightIntensityInfo = {
        "LightIntensity",
        "Light intensity",
        "" // @TODO Missing documentation
    };
} // namespace

namespace openspace::globebrowsing {

PointGlobe::PointGlobe(const RenderableGlobe& owner)
    : Renderable({ { "Identifier", owner.identifier() },  { "Name", owner.guiName() } })
    , _owner(owner)
    , _intensityClamp(IntensityClampInfo, 1.f, 0.f, 1.f)
    , _lightIntensity(LightIntensityInfo, 1.f, 0.f, 50.f)
{
    addProperty(_intensityClamp);
    addProperty(_lightIntensity);
}

PointGlobe::~PointGlobe() {
    glDeleteBuffers(1, &_vertexBufferID);
    glDeleteVertexArrays(1, &_vaoID);
}

void PointGlobe::initialize() {
    _programObject = global::renderEngine.buildRenderProgram(
        "PointGlobe",
        absPath("${MODULE_GLOBEBROWSING}/shaders/pointglobe_vs.glsl"),
        absPath("${MODULE_GLOBEBROWSING}/shaders/pointglobe_fs.glsl")
    );

    ghoul::opengl::updateUniformLocations(*_programObject, _uniformCache, UniformNames);

    glGenVertexArrays(1, &_vaoID);
    glGenBuffers(1, &_vertexBufferID);

    glBindVertexArray(_vaoID);

    std::array<float, 2 * 6> quadVertexData = {
        -1.f, -1.f,
         1.f, -1.f,
        -1.f,  1.f,
        -1.f,  1.f,
         1.f, -1.f,
         1.f,  1.f
    };

    // Vertex buffer
    glBindBuffer(GL_ARRAY_BUFFER, _vertexBufferID);
    glBufferData(
        GL_ARRAY_BUFFER,
        sizeof(float) * quadVertexData.size(),
        quadVertexData.data(),
        GL_STATIC_DRAW
    );

    // Position at location 0
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 2 * sizeof(float), nullptr);

    glBindVertexArray(0);
}

void PointGlobe::deinitialize() {
    glDeleteVertexArrays(1, &_vaoID);
    glDeleteBuffers(1, &_vertexBufferID);
}

bool PointGlobe::isReady() const {
    return (_vaoID != 0) && (_vertexBufferID != 0);
}

void PointGlobe::render(const RenderData& data, RendererTasks&) {
    _programObject->activate();

    // Calculate variables to be used as uniform variables in shader
    const glm::dvec3 bodyPosition = data.modelTransform.translation;

    const glm::dmat4 rotationTransform = glm::lookAt(
        glm::dvec3(0.0f),
        data.camera.positionVec3() - bodyPosition,
        glm::normalize(glm::dvec3(1000000.0f) - bodyPosition)
    );

    const glm::dvec3 camToBody = bodyPosition - data.camera.positionVec3();
    const float distanceToBody = static_cast<float>(glm::length(camToBody));

    const float avgRadius = static_cast<float>(_owner.ellipsoid().averageRadius());
    const float lightIntensity = static_cast<float>(
        _lightIntensity.value() * data.modelTransform.scale * avgRadius / distanceToBody
    );
    const float lightIntensityClamped = glm::min(lightIntensity, _intensityClamp.value());
    //float lightOverflow = glm::max(lightIntensity - lightIntensityClamped, 0.0f);

    const float billboardRadius = lightIntensityClamped * distanceToBody;
    const glm::dmat4 scaleTransform = glm::scale(
        glm::dmat4(1.0), glm::dvec3(billboardRadius)
    );

    setBoundingSphere(billboardRadius);

    // Model transform and view transform needs to be in double precision
    const glm::dmat4 modelTransform = glm::translate(glm::dmat4(1.0), bodyPosition) *
                                      glm::inverse(rotationTransform) *
                                      scaleTransform; // Scale
    const glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() *
                                          modelTransform;


    _programObject->setUniform(
        _uniformCache.lightIntensityClamped,
        lightIntensityClamped
    );
    //_program->setUniform("lightOverflow", lightOverflow);
    //_program->setUniform("directionToSunViewSpace", directionToSunViewSpace);
    _programObject->setUniform(
        _uniformCache.modelView,
        glm::mat4(modelViewTransform)
    );
    _programObject->setUniform(
        _uniformCache.projection,
        data.camera.sgctInternal.projectionMatrix()
    );

    glBlendFunc(GL_SRC_ALPHA, GL_ONE);

    glDisable(GL_CULL_FACE);
    glDisable(GL_DEPTH_TEST);

    glBindVertexArray(_vaoID);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _vertexBufferID);
    glDrawArrays(GL_TRIANGLES, 0, 6);
    glBindVertexArray(0);

    glEnable(GL_DEPTH_TEST);
    glEnable(GL_CULL_FACE);

    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    _programObject->deactivate();
}

} // namespace openspace::globebrowsing
