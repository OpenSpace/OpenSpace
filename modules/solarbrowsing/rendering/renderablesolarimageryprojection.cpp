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

#include <modules/solarbrowsing/rendering/renderablesolarimageryprojection.h>

#include <modules/solarbrowsing/rendering/renderablesolarimagery.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/transferfunction.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/logging/logmanager.h>
#include <format>
#include <fstream>
#include <limits>
#include <memory>

namespace {
    constexpr char* _loggerCat = "RendearbleSpacecraftCameraSphere";

    // This number MUST match the constant specified in the shader, otherwise UB / MN
    constexpr int MaxSpacecraftObservatories = 7;

    struct [[codegen::Dictionary(RenderableSolarImageryProjection)]] Parameters {
        // List of spacecraft identifiers that will be projected on the sphere (Sun)
        // surface.
        std::vector<std::string> dependentNodes;

        //std::optional<float> radius;
    };

#include "renderablesolarimageryprojection_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderableSolarImageryProjection::Documentation() {
    return codegen::doc<Parameters>("renderablesolarimageryprojection");
}

RenderableSolarImageryProjection::RenderableSolarImageryProjection(
                                                      const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _sphere(6.96701E8f, 100)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    for (const std::string& nodeName : p.dependentNodes) {
        SceneGraphNode* dependentNode =
            global::renderEngine->scene()->sceneGraphNode(nodeName);

        if (!dependentNode) {
            LWARNING(std::format(
                "Specified dependent node '{}' did not exist", nodeName
            ));
            continue;
        }
        Renderable* dependentRenderable = dependentNode->renderable();
        RenderableSolarImagery* renderableSolarImagery =
            dynamic_cast<RenderableSolarImagery*>(dependentRenderable);
        if (!renderableSolarImagery) {
            LWARNING(std::format(
                "Specified dependent node '{}' that was not a RenderableSolarImagery",
                nodeName
            ));
            continue;
        }
        _solarImageryDependencies.push_back(dependentNode);
    }
}

void RenderableSolarImageryProjection::initialize() {
    for (SceneGraphNode* node : _solarImageryDependencies) {
        parent()->addDependency(*node);
    }
}

void RenderableSolarImageryProjection::initializeGL() {
    if (!_shader) {
        _shader = global::renderEngine->buildRenderProgram("SpacecraftImageSphereProgram",
            absPath("${MODULE_SOLARBROWSING}/shaders/spacecraftimageprojection_vs.glsl"),
            absPath("${MODULE_SOLARBROWSING}/shaders/spacecraftimageprojection_fs.glsl")
        );
    }

    _sphere.initialize();
}

void RenderableSolarImageryProjection::deinitializeGL() {
    if (_shader) {
        global::renderEngine->removeRenderProgram(_shader.get());
        _shader = nullptr;
    }
}

bool RenderableSolarImageryProjection::isReady() const {
    return _shader != nullptr;
}

void RenderableSolarImageryProjection::update(const UpdateData&) {
    if (_shader->isDirty()) {
        _shader->rebuildFromFile();
    }
}

void RenderableSolarImageryProjection::render(const RenderData& data, RendererTasks&) {
    glm::dmat4 modelTransform = calcModelTransform(data);
    glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() * modelTransform;

    _shader->activate();

    _shader->setUniform(
        "modelViewProjectionTransform",
        data.camera.projectionMatrix() * glm::mat4(modelViewTransform)
    );

    const int numPlanes = static_cast<int>(_solarImageryDependencies.size());
    int solarImageryCount = 0;

    ghoul::opengl::TextureUnit textureImageryUnits[MaxSpacecraftObservatories];
    ghoul::opengl::TextureUnit transferFunctionUnits[MaxSpacecraftObservatories];

    for (int i = 0; i < numPlanes && i < MaxSpacecraftObservatories; ++i) {
        RenderableSolarImagery* solarImagery = static_cast<RenderableSolarImagery*>(
            _solarImageryDependencies[i]->renderable()
        );

        const bool isCoronaGraph = solarImagery->isCoronaGraph();
        const bool solarImageryEnabled = solarImagery->isEnabled();

        const glm::dvec3 planePos = solarImagery->planeWorldPosition();
        const glm::dmat4 planeRot = glm::inverse(solarImagery->planeWorldRotation());
        const glm::dvec3 planePosSpacecraft = glm::dvec3(
            planeRot * glm::dvec4(planePos - data.modelTransform.translation, 1.0)
        );

        _shader->setUniform("isCoronaGraph[" + std::to_string(i) + "]", isCoronaGraph);
        _shader->setUniform("isEnabled[" + std::to_string(i) + "]", solarImageryEnabled);
        _shader->setUniform("scale[" + std::to_string(i) + "]", solarImagery->scale());
        _shader->setUniform(
            "sunToSpacecraftReferenceFrame[" + std::to_string(i) + "]",
            planeRot * glm::dmat4(data.modelTransform.rotation)
        );
        _shader->setUniform(
            "planePositionSpacecraft[" + std::to_string(i) + "]",
            planePosSpacecraft
        );
        _shader->setUniform(
            "gammaValue[" + std::to_string(i) + "]",
            solarImagery->gammaValue()
        );
        _shader->setUniform(
            "contrastValue[" + std::to_string(i) + "]",
            solarImagery->contrastValue()
        );
        _shader->setUniform(
            "centerPixel[" + std::to_string(i) + "]",
            solarImagery->getCenterPixel()
        );

        // Imagery texture
        textureImageryUnits[i].activate();
        solarImagery->imageryTexture()->bind();
        _shader->setUniform(
            "imageryTexture[" + std::to_string(i) + "]",
            textureImageryUnits[i]
        );
        // Transfer function texture
        transferFunctionUnits[i].activate();
        TransferFunction* transferFunction = solarImagery->transferFunction();
        if (transferFunction && solarImageryEnabled) {
            transferFunction->bind();
            _shader->setUniform("hasLut[" + std::to_string(i) + "]", true);
        } else {
            _shader->setUniform("hasLut[" + std::to_string(i) + "]", false);
        }
        // Must bind all sampler2D, otherwise undefined behaviour
        _shader->setUniform("lut[" + std::to_string(i) + "]", transferFunctionUnits[i]);

        solarImageryCount++;
    }

    // Set the rest of the texture units for well defined behaviour
    for (int i = solarImageryCount; i < MaxSpacecraftObservatories; ++i) {
        textureImageryUnits[i].activate();
        _shader->setUniform(
            "imageryTexture[" + std::to_string(i) + "]",
            textureImageryUnits[i]
        );
        transferFunctionUnits[i].activate();
        _shader->setUniform("lut[" + std::to_string(i) + "]", transferFunctionUnits[i]);
    }

    _shader->setUniform("numSpacecraftCameraPlanes", numPlanes);
    _sphere.render();
    _shader->deactivate();
}

} // namespace openspace
