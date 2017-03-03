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

//#include <openspace/engine/configurationmanager.h>
#include <modules/base/rendering/renderablespacecraftcameraplane.h>
#include <openspace/engine/openspaceengine.h>
//#include <openspace/util/powerscaledcoordinate.h>

#include <openspace/scene/scenegraphnode.h>
#include <openspace/rendering/renderengine.h>
//#include <modules/newhorizons/rendering/renderableplanetprojection.h>

//#include <ghoul/filesystem/filesystem>
//#include <ghoul/io/texture/texturereader.h>
//#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>


// namespace {
//     static const std::string _loggerCat = "RenderableSpacecraftCameraPlane";
// }

namespace openspace {

RenderableSpacecraftCameraPlane::RenderableSpacecraftCameraPlane(const ghoul::Dictionary& dictionary)
    : RenderablePlane(dictionary)
    , _moveFactor("movefactor", "Move Factor" , 0.5, 0.0, 1.0)
    , _target("target", "Target", "Sun")
{
    // if (dictionary.hasKey("MoveFactor")) {
    //     float moveFactor = 0.5f;
    //     if (dictionary.getValue("MoveFactor", moveFactor)) {
    //         _moveFactor = moveFactor;
    //     }
    // }
    std::string target;
    if ( dictionary.getValue("Target", target)){
        _target = target;
    }
    addProperty(_target);
    addProperty(_moveFactor);
}

void RenderableSpacecraftCameraPlane::render(const RenderData& data) {
    glm::mat4 scaleTransform = glm::mat4(1.0);

    // Activate shader
    _shader->activate();

    // Model transform and view transform needs to be in double precision
    glm::dmat4 rotationTransform;
    glm::dvec3 translationTransform;
    rotationTransform = glm::inverse(glm::dmat4(data.camera.viewRotationMatrix()));
    // Sun's barycenter
    SceneGraphNode* p = OsEng.renderEngine().scene()->sceneGraphNode(_target);
    glm::dmat4 rotationTransformTangentTrajectory = glm::lookAt(glm::normalize(data.modelTransform.translation),
                                         glm::dvec3(p->worldPosition()), data.modelTransform.rotation * glm::dvec3(0.0, 0.0, 1.0));
    rotationTransform = glm::dmat4(glm::inverse(rotationTransformTangentTrajectory));

    // Scale vector to sun barycenter to get translation distance
    translationTransform = (p->worldPosition() - data.modelTransform.translation) * _moveFactor.value();
    // Stereos internal ref frame. pass in as property
    //staticRotationTransform = glm::dmat4({0.0, 0.0, 1.0, 0.0, 1.0, 0.0, -1.0, 0.0, 0.0});

    glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation + translationTransform) *
        rotationTransform *
        glm::dmat4(glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale))) *
        glm::dmat4(scaleTransform);
    glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() * modelTransform;

    _shader->setUniform("modelViewProjectionTransform",
        data.camera.projectionMatrix() * glm::mat4(modelViewTransform));

    //_shader->setUniform("ViewProjection", data.camera.viewProjectionMatrix());
    //_shader->setUniform("ModelTransform", transform);
    //setPscUniforms(*_shader.get(), data.camera, data.position);

    ghoul::opengl::TextureUnit unit;
    unit.activate();
    _texture->bind();
    _shader->setUniform("texture1", unit);

    bool usingFramebufferRenderer =
        OsEng.renderEngine().rendererImplementation() == RenderEngine::RendererImplementation::Framebuffer;

    bool usingABufferRenderer =
        OsEng.renderEngine().rendererImplementation() == RenderEngine::RendererImplementation::ABuffer;

    if (usingABufferRenderer) {
        _shader->setUniform("additiveBlending", _blendMode == BlendMode::Additive);
    }

    bool additiveBlending = _blendMode == BlendMode::Additive && usingFramebufferRenderer;
    if (additiveBlending) {
        glDepthMask(false);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE);
    }

    glBindVertexArray(_quad);
    glDrawArrays(GL_TRIANGLES, 0, 6);

    if (additiveBlending) {
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        glDepthMask(true);
    }

    _shader->deactivate();
}

} // namespace openspace
