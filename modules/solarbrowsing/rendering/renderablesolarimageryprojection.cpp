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

#include <modules/solarbrowsing/rendering/renderablesolarimageryprojection.h>
#include <modules/solarbrowsing/rendering/renderablesolarimagery.h>
//#include <modules/solarbrowsing/rendering/renderablesolarvideo.h>
#include <modules/space/rendering/planetgeometry.h>
#include <openspace/util/time.h>
#include <openspace/scene/scenegraphnode.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/engine/openspaceengine.h>
#include <modules/fitsfilereader/include/fitsfilereader.h>
#include <modules/solarbrowsing/rendering/spacecraftcameraplane.h>
#include <openspace/util/timemanager.h>

#include <memory>
#include <fstream>
#include <limits>

using namespace ghoul::opengl;

namespace {
    static const std::string _loggerCat = "RendearbleSpacecraftCameraSphere";
    const char* keyRadius = "Radius";
    // const std::vector<std::pair<int, std::pair<std::string, std::string>>> loopTimes
    //       = {
    //          {3600, {"2012-07-12T15:00:00.00", "2012-07-12T18:00:00.00"}},
    //          {3600, {"2012-07-12T15:00:00.00", "2012-07-13T03:00:00.00"}},
    //          {21600, {"2012-07-08T00:00:00.00", "2012-07-13T00:00:00.00"}},
    //          {7200, {"2012-07-17T12:45:00.00", "2012-07-19T17:00:00.00"}},
    //          {7200, {"2012-07-17T06:00:00.00", "2012-07-19T17:00:00.00"}},
    //          {7200, {"2012-07-23T00:00:00.00", "2012-07-23T16:00:00.00"}}
    //         };
}

namespace openspace {
RenderableSolarImageryProjection::RenderableSolarImageryProjection(
      const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _shader(nullptr)
    //, _loopId("loopId", "Loop Id", 0, 0, 5)
    //, _activateLooping("activateLooping", "Activate Looping", false)
    , _sphere(nullptr)
{
    if (!dictionary.getValue("Name", _nodeName)) {
        throw ghoul::RuntimeError("Nodename has to be specified");
    }
    //std::string path;
    // if (!dictionary.getValue("hmipath", path)) {
    //     throw ghoul::RuntimeError("HMIPath has to be specified");
    // }

    // _activateLooping.onChange([this]() {
    //     if (_activateLooping) {
    //         auto& time = OsEng.timeManager().time();
    //         const auto& timePair = loopTimes[_loopId.value()];
    //         time.setTime(timePair.second.first);
    //         time.setDeltaTime(timePair.first);
    //     }
    // });

    // _startLoop1.onChange([this]() {
    //     if (_startLoop1) {
    //         auto& time = OsEng.timeManager().time();
    //         time.setTime("2012-07-12T15:00:00.00");
    //         time.setDeltaTime(3600);
    //     }
    // });

    // _startLoop2.onChange([this]() {
    //     if (_startLoop2) {
    //         auto& time = OsEng.timeManager().time();
    //         time.setTime("2012-07-12T15:00:00.00");
    //         time.setDeltaTime(3600);
    //     }
    // });

    //addProperty(_activateLooping);
    //a/ddProperty(_loopId);
    //addProperty(_startLoop1);
    //addProperty(_startLoop2);
}

bool RenderableSolarImageryProjection::initialize() {
    const std::vector<SceneGraphNode*>& allNodes
          = OsEng.renderEngine().scene()->allSceneGraphNodes();
    for (auto node : allNodes) {
        if (dynamic_cast<RenderableSolarImagery*>(node->renderable())) {
           // SceneGraphNode* tmp2 = const_cast<SceneGraphNode*>(node);
            auto thisNode = OsEng.renderEngine().scene()->sceneGraphNode(_nodeName);
            thisNode->addDependency(*node);
        }

    }

    _solarImageryDependencies
         = OsEng.renderEngine().scene()->sceneGraphNode(_nodeName)->dependencies();

    //FitsFileReader fts(false);
    //std::shared_ptr<ImageData<float>> imageData = fts.readImage<float>(path);
    float* data = nullptr;
    //if (imageData) {
    //    const std::valarray<float>& imageContents = imageData->contents;
    //    data = new float[imageContents.size()];
    //    std::memmove(data, &imageContents[0], imageContents.size() * sizeof(float));
   // }

    //const glm::size3_t imageSize(sizeX, sizeY, 1);
    _magnetogramTexture = std::make_unique<Texture>(
        data,
        glm::size3_t(3600, 1440, 1),
        ghoul::opengl::Texture::Red,
        GL_R32F,
        GL_FLOAT,
        Texture::FilterMode::Linear,
        Texture::WrappingMode::ClampToEdge
    );
    _magnetogramTexture->uploadTexture();

    if (!_shader) {
        RenderEngine& renderEngine = OsEng.renderEngine();
        _shader = renderEngine.buildRenderProgram("SpacecraftImageSphereProgram",
            "${MODULE_SOLARBROWSING}/shaders/spacecraftimagesphere_vs.glsl",
            "${MODULE_SOLARBROWSING}/shaders/spacecraftimagesphere_fs.glsl"
            );
        if (!_shader) {
            return false;
        }
    }

    //PowerScaledScalar planetSize(glm::vec2(696701000.f, 0.f));
    PowerScaledScalar planetSize(glm::vec2(6.96701f, 8.f));
    _sphere = std::make_unique<PowerScaledSphere>(PowerScaledSphere(planetSize, 100));
    _sphere->initialize();

    return isReady();
}

bool RenderableSolarImageryProjection::deinitialize() {
    RenderEngine& renderEngine = OsEng.renderEngine();
    if (_shader) {
        renderEngine.removeRenderProgram(_shader);
        _shader = nullptr;
    }
    return true;
}

bool RenderableSolarImageryProjection::isReady() const {
    return _shader && _sphere && _magnetogramTexture;
}

void RenderableSolarImageryProjection::update(const UpdateData& data) {
    if (_shader->isDirty()) {
        _shader->rebuildFromFile();
    }

    // if (_activateLooping) {
    //     auto& time = OsEng.timeManager().time();
    //     const auto& timePair = loopTimes[_loopId.value()];
    //     auto endTime = time.convertTime(timePair.second.second);
    //     if (time.j2000Seconds() > endTime) {
    //         time.setTime(timePair.second.first);
    //         for (int i = 0; i < _solarImageryDependencies.size(); ++i) {
    //             auto* solarImagery = static_cast<RenderableSolarImagery*>(
    //                   _solarImageryDependencies[i]->renderable());
    //             solarImagery->clearBuffer();
    //         }
    //     }
    // }


    // if (_startLoop1) {
    //     auto endTime = time.convertTime("2012-07-12T18:00:00.00");
    //     if (time.j2000Seconds() > endTime) {
    //         time.setTime("2012-07-12T15:00:00.00");
    //         for (int i = 0; i < _solarImageryDependencies.size(); ++i) {
    //             auto* solarImagery = static_cast<RenderableSolarImagery*>(
    //                   _solarImageryDependencies[i]->renderable());
    //             solarImagery->clearBuffer();
    //         }

    //     }
    // }

    // else if (_startLoop2) {
    //     auto endTime = time.convertTime("2012-07-13T03:00:00.00");
    //     if (time.j2000Seconds() > endTime) {
    //         time.setTime("2012-07-12T15:00:00.00");
    //         for (int i = 0; i < _solarImageryDependencies.size(); ++i) {
    //             auto* solarImagery = static_cast<RenderableSolarImagery*>(
    //                   _solarImageryDependencies[i]->renderable());
    //             solarImagery->clearBuffer();
    //         }

    //     }
    // }
}

void RenderableSolarImageryProjection::render(const RenderData& data) {
    glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) * // Translation
        glm::dmat4(data.modelTransform.rotation) *  // Spice rotation
        glm::dmat4(glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale)));
    glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() * modelTransform;

    _shader->activate();
    // _shader->setUniform(
    //     "modelTransform",
    //     modelTransform
    // );

    // _shader->setUniform(
    //     "viewTransform",
    //     data.camera.combinedViewMatrix()
    // );

    //  _shader->setUniform(
    //     "projectionTransform",
    //      data.camera.projectionMatrix()
    // );

    _shader->setUniform(
        "modelViewProjectionTransform",
        data.camera.projectionMatrix() * glm::mat4(modelViewTransform)
    );

    const int numPlanes = _solarImageryDependencies.size();
    const int MAX_SPACECRAFT_OBSERVATORY = 12;
    int solarImageryCount = 0;

    ghoul::opengl::TextureUnit txUnits[MAX_SPACECRAFT_OBSERVATORY];
    ghoul::opengl::TextureUnit tfUnits[MAX_SPACECRAFT_OBSERVATORY];

    for (int i = 0; i < numPlanes; ++i) {
        auto* solarImagery = static_cast<RenderableSolarImagery*>(
              _solarImageryDependencies[i]->renderable());

      //  if (!solarImagery->isEnabled()) continue;

        bool isCoronaGraph = solarImagery->_isCoronaGraph;
        bool enabled = solarImagery->isEnabled();

        const SpacecraftCameraPlane& plane = solarImagery->getCameraPlane();
        const glm::dvec3 planePos = plane.worldPosition();
        const glm::dmat4 planeRot = plane.worldRotation();

        // Camera looking away, fake corona graph same bool same logic, TODO: change name
        // if (!solarImagery->_shouldRenderPlane) {
        //     isCoronaGraph = true;
        // }

        _shader->setUniform("isCoronaGraph[" + std::to_string(i) + "]", isCoronaGraph);
        _shader->setUniform("isEnabled[" + std::to_string(i) + "]", enabled);

        //_shader->setUniform("magicPlaneFactor[" + std::to_string(i) + "]", solarImagery->_magicPlaneFactor);

        _shader->setUniform("sunToSpacecraftReferenceFrame[" + std::to_string(i) + "]",
                        planeRot * glm::dmat4(data.modelTransform.rotation));
        _shader->setUniform("planePositionSpacecraft[" + std::to_string(i) + "]",
                            glm::dvec3(planeRot * glm::dvec4(planePos, 1.0)));

        //_shader->setUniform("imageSize[" + std::to_string(i) + "]" , solarImagery->_imageSize);
        //_shader->setUniform("sharpenValue[" + std::to_string(i) + "]", solarImagery->_sharpenValue);
        _shader->setUniform("gammaValue[" + std::to_string(i) + "]", solarImagery->_gammaValue);
        _shader->setUniform("contrastValue[" + std::to_string(i) + "]", solarImagery->_contrastValue);

        // Offset and scale
        _shader->setUniform("scale[" + std::to_string(i) + "]", solarImagery->_currentScale);
        _shader->setUniform("centerPixel[" + std::to_string(i) + "]", solarImagery->_currentCenterPixel);

        // Imagery texture
        txUnits[i].activate();
        solarImagery->getImageryTexture()->bind();
        _shader->setUniform("imageryTexture[" + std::to_string(i) + "]", txUnits[i]);
        tfUnits[i].activate();

        auto lut = solarImagery->getTransferFunction();
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
    for (int i = solarImageryCount; i < MAX_SPACECRAFT_OBSERVATORY; ++i) {
        txUnits[i].activate();
        _shader->setUniform("imageryTexture[" + std::to_string(i) + "]", txUnits[i]);
        tfUnits[i].activate();
        _shader->setUniform("lut[" + std::to_string(i) + "]", tfUnits[i]);
    }

    ghoul::opengl::TextureUnit imageUnit;
    imageUnit.activate();
    _magnetogramTexture->bind();
    _shader->setUniform("magnetogram", imageUnit);

    _shader->setUniform("numSpacecraftCameraPlanes", numPlanes);

    _sphere->render();;
    _shader->deactivate();
}

}
