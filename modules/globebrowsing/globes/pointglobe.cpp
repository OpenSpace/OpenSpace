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

#include <modules/globebrowsing/globes/pointglobe.h>

#include <modules/globebrowsing/globes/renderableglobe.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/wrapper/windowwrapper.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/updatestructures.h>

#include <ghoul/opengl/programobject.h>

namespace openspace::globebrowsing {

PointGlobe::PointGlobe(const RenderableGlobe& owner)
    : _owner(owner)
    , _intensityClamp(
        "intensityClamp",
        "Intensity clamp",
        1, 0, 1
    )
    , _lightIntensity(
        "lightIntensity",
        "Light intensity",
        1, 0, 50
    )
{
    addProperty(_intensityClamp);
    addProperty(_lightIntensity);
}

PointGlobe::~PointGlobe() {
    glDeleteBuffers(1, &_vertexBufferID);
    glDeleteVertexArrays(1, &_vaoID);
}

bool PointGlobe::initialize() {
    _programObject = OsEng.renderEngine().buildRenderProgram(
        "PointGlobe",
        "${MODULE_GLOBEBROWSING}/shaders/pointglobe_vs.glsl",
        "${MODULE_GLOBEBROWSING}/shaders/pointglobe_fs.glsl");

    glGenVertexArrays(1, &_vaoID);
    glGenBuffers(1, &_vertexBufferID);

    glBindVertexArray(_vaoID);

    std::array<glm::vec2, 6> quadVertexData = {{
      glm::vec2(-1.0f, -1.0f),
      glm::vec2(1.0f, -1.0f),
      glm::vec2(-1.0f, 1.0f),
      glm::vec2(-1.0f, 1.0f),
      glm::vec2(1.0f, -1.0f),
      glm::vec2(1.0f, 1.0f)
    }};
    
    // Vertex buffer
    glBindBuffer(GL_ARRAY_BUFFER, _vertexBufferID);
    glBufferData(
        GL_ARRAY_BUFFER,
        sizeof(glm::vec2) * quadVertexData.size(),
        quadVertexData.data(),
        GL_STATIC_DRAW
    );

    // Position at location 0
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, sizeof(glm::vec2), 0);

    glBindVertexArray(0);

    return isReady();
}

bool PointGlobe::deinitialize() {
    return true;
}

bool PointGlobe::isReady() const {
    return (_vaoID != 0) && (_vertexBufferID != 0);
}
    
void PointGlobe::render(const RenderData& data, RendererTasks&) {
    _programObject->activate();

    // Calculate variables to be used as uniform variables in shader
    glm::dvec3 bodyPosition = data.modelTransform.translation;

    glm::dmat4 rotationTransform = glm::lookAt(
        glm::dvec3(0.0f),
        data.camera.positionVec3() - bodyPosition,
        glm::normalize(glm::dvec3(1000000.0f) - bodyPosition));
  
    glm::dvec3 camToBody = bodyPosition - data.camera.positionVec3();
    float distanceToBody = static_cast<float>(glm::length(camToBody));

    float avgRadius = static_cast<float>(_owner.ellipsoid().averageRadius());
    float lightIntensity = static_cast<float>(
        _lightIntensity.value() * data.modelTransform.scale * avgRadius / distanceToBody
    );
    float lightIntensityClamped = glm::min(lightIntensity, _intensityClamp.value());
    //float lightOverflow = glm::max(lightIntensity - lightIntensityClamped, 0.0f);

    float billboardRadius = lightIntensityClamped * distanceToBody;
    glm::dmat4 scaleTransform = glm::scale(glm::dmat4(1.0), glm::dvec3(billboardRadius));
  
    setBoundingSphere(billboardRadius);

    // Model transform and view transform needs to be in double precision
    glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), bodyPosition) * // Translation
        glm::inverse(rotationTransform) * 
        scaleTransform; // Scale
    glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() * modelTransform;
    //glm::vec3 directionToSun = glm::normalize(glm::vec3(0) - glm::vec3(bodyPosition));
    //glm::vec3 directionToSunViewSpace = glm::mat3(data.camera.combinedViewMatrix()) * directionToSun;
        
  
    _programObject->setUniform("lightIntensityClamped", lightIntensityClamped);
    //_programObject->setUniform("lightOverflow", lightOverflow);
    //_programObject->setUniform("directionToSunViewSpace", directionToSunViewSpace);
    _programObject->setUniform("modelViewTransform", glm::mat4(modelViewTransform));
    _programObject->setUniform("projectionTransform", data.camera.sgctInternal.projectionMatrix());
  
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
