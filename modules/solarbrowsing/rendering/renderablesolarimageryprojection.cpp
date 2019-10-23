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

#include <modules/solarbrowsing/rendering/renderablesolarimageryprojection.h>

#include <modules/solarbrowsing/rendering/renderablesolarimagery.h>
#include <modules/solarbrowsing/rendering/spacecraftcameraplane.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/transferfunction.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/logging/logmanager.h>
#include <fstream>
#include <limits>
#include <memory>

namespace {
    constexpr const char* _loggerCat = "RendearbleSpacecraftCameraSphere";

    constexpr const char* keyRadius = "Radius";
    constexpr const char* KeyDependentNodes = "DependentNodes";

    // This number MUST match the constant specified in the shader, otherwise UB / MN
    constexpr const int MaxSpacecraftObservatories = 7;
} // namespace

namespace openspace {

RenderableSolarImageryProjection::RenderableSolarImageryProjection(
                                                      const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _sphere(6.96701E8f, 100)
{
    ghoul::Dictionary nodes = dictionary.value<ghoul::Dictionary>(KeyDependentNodes);
    for (int i = 1; i <= nodes.size(); ++i) {
        std::string n = nodes.value<std::string>(std::to_string(i));
        _dependentNodes.push_back(std::move(n));
    }
}

void RenderableSolarImageryProjection::initialize() {
    for (const std::string& n : _dependentNodes) {
        SceneGraphNode* depNode = global::renderEngine.scene()->sceneGraphNode(n);
        if (!depNode) {
            LWARNING(fmt::format(
                "Specified dependent node '{}' did not exist", n
            ));
            continue;
        }
        Renderable* depR = depNode->renderable();
        RenderableSolarImagery* siR = dynamic_cast<RenderableSolarImagery*>(depR);
        if (!siR) {
            LWARNING(fmt::format(
                "Specified dependent node '{}' that was not a RenderableSolarImagery", n
            ));
            continue;
        }
        _solarImageryDependencies.push_back(depNode);
    }
}

void RenderableSolarImageryProjection::initializeGL() {
    if (!_shader) {
        _shader = global::renderEngine.buildRenderProgram("SpacecraftImageSphereProgram",
            absPath("${MODULE_SOLARBROWSING}/shaders/spacecraftimageprojection_vs.glsl"),
            absPath("${MODULE_SOLARBROWSING}/shaders/spacecraftimageprojection_fs.glsl")
        );
    }

    _sphere.initialize();
}

void RenderableSolarImageryProjection::deinitializeGL() {
    if (_shader) {
        global::renderEngine.removeRenderProgram(_shader.get());
        _shader = nullptr;
    }
}

bool RenderableSolarImageryProjection::isReady() const {
    return _shader != nullptr;
}

void RenderableSolarImageryProjection::update(const UpdateData& data) {
    if (_shader->isDirty()) {
        _shader->rebuildFromFile();
    }
}

void RenderableSolarImageryProjection::render(const RenderData& data, RendererTasks& rendererTask) {
    glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
        glm::dmat4(data.modelTransform.rotation) *
        glm::dmat4(glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale)));
    glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() * modelTransform;

    _shader->activate();

    _shader->setUniform(
        "modelViewProjectionTransform",
        data.camera.projectionMatrix() * glm::mat4(modelViewTransform)
    );

    const int numPlanes = _solarImageryDependencies.size();
    int solarImageryCount = 0;

    ghoul::opengl::TextureUnit txUnits[MaxSpacecraftObservatories];
    ghoul::opengl::TextureUnit tfUnits[MaxSpacecraftObservatories];

    for (int i = 0; i < numPlanes; ++i) {
        RenderableSolarImagery* solarImagery = static_cast<RenderableSolarImagery*>(
            _solarImageryDependencies[i]->renderable()
        );

        bool isCoronaGraph = solarImagery->isCoronaGraph();
        bool enabled = solarImagery->isEnabled();

        const SpacecraftCameraPlane& plane = solarImagery->getCameraPlane();
        const glm::dvec3 planePos = plane.worldPosition();
        const glm::dmat4 planeRot = plane.worldRotation();

        _shader->setUniform("isCoronaGraph[" + std::to_string(i) + "]", isCoronaGraph);
        _shader->setUniform("isEnabled[" + std::to_string(i) + "]", enabled);
        _shader->setUniform("sunToSpacecraftReferenceFrame[" + std::to_string(i) + "]",
                        planeRot * glm::dmat4(data.modelTransform.rotation));
        _shader->setUniform("planePositionSpacecraft[" + std::to_string(i) + "]",
                            glm::dvec3(planeRot * glm::dvec4(planePos, 1.0)));
        _shader->setUniform("gammaValue[" + std::to_string(i) + "]", solarImagery->getGammaValue());
        _shader->setUniform("multiplierValue[" + std::to_string(i) + "]", solarImagery->getMultiplierValue());
        _shader->setUniform("contrastValue[" + std::to_string(i) + "]", solarImagery->getContrastValue());
        _shader->setUniform("scale[" + std::to_string(i) + "]", solarImagery->getScale());
        _shader->setUniform("centerPixel[" + std::to_string(i) + "]", solarImagery->getCenterPixel());

        // Imagery texture
        txUnits[i].activate();
        solarImagery->getImageryTexture()->bind();
        _shader->setUniform("imageryTexture[" + std::to_string(i) + "]", txUnits[i]);
        tfUnits[i].activate();

        TransferFunction* lut = solarImagery->getTransferFunction();
        if (lut && solarImagery->isEnabled()) {
            lut->bind();
            _shader->setUniform("hasLut[" + std::to_string(i) + "]", true);
        } else {
            _shader->setUniform("hasLut[" + std::to_string(i) + "]", false);
        }
        // Must bind all sampler2D, otherwise undefined behaviour
        _shader->setUniform("lut[" + std::to_string(i) + "]", tfUnits[i]);
        solarImageryCount++;
    }

    // Set the rest of the texture units for well defined behaviour
    for (int i = solarImageryCount; i < MaxSpacecraftObservatories; ++i) {
        txUnits[i].activate();
        _shader->setUniform("imageryTexture[" + std::to_string(i) + "]", txUnits[i]);
        tfUnits[i].activate();
        _shader->setUniform("lut[" + std::to_string(i) + "]", tfUnits[i]);
    }

    _shader->setUniform("numSpacecraftCameraPlanes", numPlanes);
    _sphere.render();
    _shader->deactivate();
}

} // namespace openspace
