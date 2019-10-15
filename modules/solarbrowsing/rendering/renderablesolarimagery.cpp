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
    constexpr const char* _loggerCat = "RenderableSolarImagery";
    constexpr const char* KeyStartInstrument = "StartInstrument";

    constexpr const unsigned int DefaultTextureSize = 32;
    constexpr const unsigned int MaxImageResolution = 4096;

    static const openspace::properties::Property::PropertyInfo ActiveInstrumentsInfo = {
        "activeInstrument",
        "Active instrument",
        "The active instrument of the current spacecraft imagery"
    };
    static const openspace::properties::Property::PropertyInfo ContrastValueInfo = {
        "contrastValue",
        "Contrast",
        "Contrast of the current spacecraft imagery"
    };
    static const openspace::properties::Property::PropertyInfo EnableBorderInfo = {
        "enableBorder",
        "Enable Border",
        "Enables border around the current spacecraft imagery"
    };
    static const openspace::properties::Property::PropertyInfo EnableFrustumInfo = {
        "enableFrustum",
        "Enable frustum",
        "Enables frustum around the current spacecraft imagery"
    };
    static const openspace::properties::Property::PropertyInfo GammaValueInfo = {
        "gammaValue",
        "Gamma",
        "Gamma of the current spacecraft imagery"
    };
    static const openspace::properties::Property::PropertyInfo MoveFactorInfo = {
        "moveFactor",
        "Move Factor",
        "How close to the sun to render the imagery"
    };
    static const openspace::properties::Property::PropertyInfo PlaneOpacityInfo = {
        "planeOpacity",
        "Plane Opacity",
        "Opacity of the image plane"
    };
    static const openspace::properties::Property::PropertyInfo DownsamplingLevelInfo = {
        "downsamplingLevel",
        "Downsampling Level",
        "How much to downsample the original data. 0 is original resolution."
    };
    static const openspace::properties::Property::PropertyInfo VerboseModeInfo = {
        "verboseMode",
        "Verbose Mode",
        "Output information about image decoding etc"
    };
}

namespace openspace {

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
    std::string rootPath;
    if (!dictionary.getValue("RootPath", rootPath)) {
        throw ghoul::RuntimeError("RootPath has to be specified");
    }

    SolarBrowsingModule* solarbrowsingModule =
        global::moduleEngine.module<SolarBrowsingModule>();

    SpacecraftImageryManager& spacecraftImageryManager =
        solarbrowsingModule->spacecraftImageryManager();
    
    std::string transferfunctionPath;
    if (dictionary.getValue("TransferfunctionPath", transferfunctionPath)) {
        spacecraftImageryManager.loadTransferFunctions(transferfunctionPath, _tfMap);
    }

    spacecraftImageryManager.loadImageMetadata(rootPath, _imageMetadataMap);

    // Add GUI names
    unsigned int guiNameCount = 0;
    using K = std::string;
    using V = Timeline<ImageMetadata>;
    for (const std::pair<K, V>& el : _imageMetadataMap) {
        _activeInstruments.addOption(guiNameCount++, el.first);
    }

    if (dictionary.hasKey(KeyStartInstrument)) {
        _currentActiveInstrument = dictionary.value<std::string>(KeyStartInstrument);
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
        nullptr,
        glm::uvec3(DefaultTextureSize, DefaultTextureSize, 1),
        ghoul::opengl::Texture::Format::Red, // Format of the pixeldata
        GL_R8, // INTERNAL format:
        // More preferable to give explicit precision here,
        // otherwise up to the driver to decide
        GL_UNSIGNED_BYTE, // Type of data
        ghoul::opengl::Texture::FilterMode::Linear,
        ghoul::opengl::Texture::WrappingMode::ClampToEdge
        );

    _texture->setDataOwnership(ghoul::opengl::Texture::TakeOwnership::No);
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

float RenderableSolarImagery::getImageResolutionFactor() {
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
            global::timeManager.time().j2000Seconds(),
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
        decode(_decodeBuffer.data(), keyframe->data.filename);
    }
    else {
        if (_currentImage == nullptr) {
            // No need to re-upload an empty image.
            return;
        }
        _isCoronaGraph = false;
        _imageSize = 32;
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
