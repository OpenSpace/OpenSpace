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

#include <modules/solarbrowsing/rendering/renderablesolarimagery.h>


#include <modules/solarbrowsing/solarbrowsingmodule.h>
#include <modules/solarbrowsing/util/j2kcodec.h>
#include <modules/solarbrowsing/rendering/spacecraftcameraplane.h>
#include <modules/solarbrowsing/util/pixelbufferobject.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/query/query.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/timemanager.h>
#include <ghoul/glm.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/defer.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <chrono>

namespace {
    constexpr char* _loggerCat = "RenderableSolarImagery";

    constexpr unsigned int DefaultTextureSize = 32;

    constexpr openspace::properties::Property::PropertyInfo ActiveInstrumentsInfo = {
        "ActiveInstrument",
        "Active instrument",
        "The active instrument of the current spacecraft imagery"
    };
    constexpr openspace::properties::Property::PropertyInfo ContrastValueInfo = {
        "ContrastValue",
        "Contrast",
        "Contrast of the current spacecraft imagery"
    };
    constexpr openspace::properties::Property::PropertyInfo DownsamplingLevelInfo = {
        "DownsamplingLevel",
        "Downsampling Level",
        "How much to downsample the original data. 0 is original resolution."
    };
    constexpr openspace::properties::Property::PropertyInfo EnableBorderInfo = {
        "EnableBorder",
        "Enable Border",
        "Enables border around the current spacecraft imagery"
    };
    constexpr openspace::properties::Property::PropertyInfo EnableFrustumInfo = {
        "EnableFrustum",
        "Enable frustum",
        "Enables frustum around the current spacecraft imagery"
    };
    constexpr openspace::properties::Property::PropertyInfo GammaValueInfo = {
        "GammaValue",
        "Gamma",
        "Gamma of the current spacecraft imagery"
    };
    constexpr openspace::properties::Property::PropertyInfo MoveFactorInfo = {
        "MoveFactor",
        "Move Factor",
        "How close to the sun to render the imagery"
    };
    constexpr openspace::properties::Property::PropertyInfo PlaneOpacityInfo = {
        "PlaneOpacity",
        "Plane Opacity",
        "Opacity of the image plane"
    };
    constexpr openspace::properties::Property::PropertyInfo VerboseModeInfo = {
        "VerboseMode",
        "Verbose Mode",
        "Output information about image decoding etc"
    };

    struct [[codegen::Dictionary(RenderableSolarImagery)]] Parameters {
        std::filesystem::path rootDir [[codegen::directory()]];

        std::filesystem::path transferfunctionDir [[codegen::directory()]];

        std::optional<std::string> startInstrument;

        // [[codegen::verbatim(EnableBorderInfo.description)]]
        std::optional<bool> enableBorder;

        // [[codegen::verbatim(EnableFrustumInfo.description)]]
        std::optional<bool> enableFrustum;
    };
#include "renderablesolarimagery_codegen.cpp"
}

namespace openspace {

documentation::Documentation RenderableSolarImagery::Documentation() {
    return codegen::doc<Parameters>("renderablesolarimegary");
}

RenderableSolarImagery::RenderableSolarImagery(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _activeInstruments(ActiveInstrumentsInfo)
    , _contrastValue(ContrastValueInfo, 0.f, -15.f, 15.f)
    , _enableBorder(EnableBorderInfo, false)
    , _enableFrustum(EnableFrustumInfo, false)
    , _gammaValue(GammaValueInfo, 0.9f, 0.1f, 10.f)
    , _moveFactor(MoveFactorInfo, 1.0, 0.0, 1.0)
    , _planeOpacity(PlaneOpacityInfo, 1.f, 0.f, 1.f)
    , _downsamplingLevel(DownsamplingLevelInfo, 2, 0, 5)
    , _verboseMode(VerboseModeInfo, false)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    SolarBrowsingModule* solarbrowsingModule =
        global::moduleEngine->module<SolarBrowsingModule>();

    SpacecraftImageryManager& spacecraftImageryManager =
        solarbrowsingModule->spacecraftImageryManager();

    spacecraftImageryManager.loadTransferFunctions(p.transferfunctionDir, _tfMap);

    spacecraftImageryManager.loadImageMetadata(p.rootDir, _imageMetadataMap);

    // Add GUI names
    unsigned int guiNameCount = 0;
    using K = std::string;
    using V = Timeline<ImageMetadata>;
    for (const std::pair<K, V>& el : _imageMetadataMap) {
        _activeInstruments.addOption(guiNameCount++, el.first);
    }

    if (p.startInstrument.has_value()) {
        _currentActiveInstrument = p.startInstrument.value();
    }
    else {
        _currentActiveInstrument = _activeInstruments.getDescriptionByValue(
            _activeInstruments
        );
    }
    // Some sanity checks
    if (_imageMetadataMap.empty()) {
        LERROR("Images map is empty! Check your path");
    }

    _enableBorder = p.enableBorder.value_or(_enableBorder);
    _enableFrustum = p.enableFrustum.value_or(_enableFrustum);

    addProperty(_planeOpacity);
    addProperty(_enableBorder);
    addProperty(_enableFrustum);
    addProperty(_activeInstruments);
    addProperty(_gammaValue);
    addProperty(_contrastValue);
    addProperty(_downsamplingLevel);
    addProperty(_moveFactor);
    addProperty(_verboseMode);

    _enableFrustum.onChange([this]() {
        _enableBorder.setValue(_enableFrustum.value());
    });

    _activeInstruments.onChange([this]() {
        _currentActiveInstrument = _activeInstruments.getDescriptionByValue(
            _activeInstruments
        );
        _currentImage = nullptr;
    });

    _downsamplingLevel.onChange([this]() {
        _currentImage = nullptr;
    });

    _moveFactor.onChange([this]() {
        _spacecraftCameraPlane->createPlaneAndFrustum(_moveFactor);
    });
}



void RenderableSolarImagery::initializeGL() {
    _spacecraftCameraPlane = std::make_unique<SpacecraftCameraPlane>(_moveFactor);
    _texture = std::make_unique<ghoul::opengl::Texture>(
        glm::uvec3(DefaultTextureSize, DefaultTextureSize, 1),
        GL_TEXTURE_2D,
        ghoul::opengl::Texture::Format::Red, // Format of the pixeldata
        GL_R8, // INTERNAL format:
        // More preferable to give explicit precision here,
        // otherwise up to the driver to decide
        GL_UNSIGNED_BYTE, // Type of data
        ghoul::opengl::Texture::FilterMode::Linear,
        ghoul::opengl::Texture::WrappingMode::ClampToEdge,
        ghoul::opengl::Texture::AllocateData::Yes,
        ghoul::opengl::Texture::TakeOwnership::No
        );

    updateTextureGPU();
}

void RenderableSolarImagery::deinitializeGL() {
    _spacecraftCameraPlane->destroy();
}

TransferFunction* RenderableSolarImagery::getTransferFunction() {
    return _lut;
}

const std::unique_ptr<ghoul::opengl::Texture>& RenderableSolarImagery::getImageryTexture()
{
    return _texture;
}

const SpacecraftCameraPlane& RenderableSolarImagery::getCameraPlane() {
    return *_spacecraftCameraPlane;
}

float RenderableSolarImagery::getContrastValue() {
    return _contrastValue;
}

float RenderableSolarImagery::getGammaValue() {
    return _gammaValue;
}

unsigned int RenderableSolarImagery::getImageResolutionFactor() {
    return _imageSize;
}

glm::vec2 RenderableSolarImagery::getCenterPixel() {
    return _currentCenterPixel;
}

float RenderableSolarImagery::getScale() {
    return _currentScale;
}

bool RenderableSolarImagery::isCoronaGraph() {
    return _isCoronaGraph;
}

bool RenderableSolarImagery::isReady() const {
    return _spacecraftCameraPlane &&
        _spacecraftCameraPlane->isReady();
}

void RenderableSolarImagery::updateTextureGPU(bool asyncUpload, bool resChanged) {
    Keyframe<ImageMetadata>* keyframe =
        _imageMetadataMap[_currentActiveInstrument].lastKeyframeBefore(
            global::timeManager->time().j2000Seconds(),
            true
        );

    if (keyframe) {
        if (_currentImage == &(keyframe->data)) {
            // This keyframe is already uploaded to the GPU.
            return;
        }
        _imageSize = static_cast<unsigned int>(
            keyframe->data.fullResolution /
            std::pow(2, static_cast<int>(_downsamplingLevel))
        );
        _isCoronaGraph = keyframe->data.isCoronaGraph;
        _currentScale = keyframe->data.scale;
        _currentCenterPixel = keyframe->data.centerPixel;
        _currentImage = &(keyframe->data);

        _decodeBuffer.resize(_imageSize * _imageSize * sizeof(IMG_PRECISION));
        decode(_decodeBuffer.data(), keyframe->data.filePath.string());
    }
    else {
        if (_currentImage == nullptr) {
            // No need to re-upload an empty image.
            return;
        }
        _isCoronaGraph = false;
        _imageSize = DefaultTextureSize;
        _currentScale = 0;
        _currentCenterPixel = glm::vec2(2.f);
        _currentImage = nullptr;
    }

    _texture->setDimensions(glm::uvec3(_imageSize, _imageSize, 1));
    _texture->setPixelData(
        _decodeBuffer.data(),
        ghoul::opengl::Texture::TakeOwnership::No
    );
    _texture->uploadTexture();
}

void RenderableSolarImagery::decode(unsigned char* buffer, const std::string& filename) {
    J2kCodec j2c(_verboseMode);
    j2c.decodeIntoBuffer(filename, buffer, _downsamplingLevel);
}

bool RenderableSolarImagery::checkBoundaries(const RenderData& data) {
    const glm::dvec3& normal = _spacecraftCameraPlane->normal();
    const glm::dvec3& cameraPosition = data.camera.positionVec3();
    const glm::dvec3& planePosition = _spacecraftCameraPlane->worldPosition();

    const glm::dvec3 toCamera = glm::normalize(cameraPosition - planePosition);
    if (glm::dot(toCamera, normal) < 0) {
        return false;
    }
    return true;
}

void RenderableSolarImagery::update(const UpdateData& data) {
    // Update lookup table, TODO: No need to do this every update
    _lut = _tfMap[_currentActiveInstrument].get();
    _spacecraftCameraPlane->update();
}

void RenderableSolarImagery::render(const RenderData& data, RendererTasks&) {
    // Update texture
    //if (checkBoundaries(data)) {
    // TODO: The checkBoundaries logic was temporarily disabled since it causes
    // a bug that prevents this renderablesolarimageryprojection component to be updated
    // as soon as the view frustum is more than 90 degrees off.
    updateTextureGPU();
    //}
    const glm::dvec3& sunPositionWorld = sceneGraphNode("Sun")->worldPosition();
    _spacecraftCameraPlane->render(
        data,
        *_texture,
        _lut,
        sunPositionWorld,
        _planeOpacity,
        _contrastValue,
        _gammaValue,
        _enableBorder,
        _enableFrustum,
        _currentCenterPixel,
        _currentScale,
        _imagePlaneOffset,
        _isCoronaGraph
    );
}

} // namespace openspace
