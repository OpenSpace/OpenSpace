/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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
#include <modules/base/rendering/renderableplanetimevaryingimage.h>
#include <modules/base/basemodule.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>

// Test debugging tools more then logmanager
#include <ghoul/logging/logmanager.h>
//#include <ghoul/logging/consolelog.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/util/updatestructures.h>
#include <optional>

namespace {
    constexpr const char* KeyLazyLoading = "LazyLoading";
    constexpr const char* _loggerCat = "RenderablePlaneTimeVaryingImage";

    constexpr openspace::properties::Property::PropertyInfo TextureInfo = {
        "Texture",
        "Texture",
        "This value specifies an image that is loaded from disk and is used as a texture "
        "that is applied to this plane. This image has to be square."
    };

    constexpr openspace::properties::Property::PropertyInfo RenderTypeInfo = {
       "RenderType",
       "RenderType",
       "This value specifies if the plane should be rendered in the Background,"
       "Opaque, Transparent, or Overlay rendering step."
    };

    struct [[codegen::Dictionary(RenderablePlaneTimeVaryingImage)]] Parameters {

        // [[codegen::verbatim(TextureInfo.description)]]
        std::string texture;

        enum class RenderType {
            Background,
            Opaque,
            PreDeferredTransparency,
            PostDeferredTransparency,
            Overlay
        };

        // [[codegen::verbatim(RenderTypeInfo.description)]]
        std::optional<RenderType> renderType [[codegen::key("RenderType")]];

        //KeyLazyLoading,   // TODO, might be useful for potential realtime streaming
        //    new BoolVerifier,
        //    Optional::Yes,
        //    "If this value is set to 'true', the image for this plane will not"
        //    "be loaded at startup but rather when image is shown for the first "
        //    "time. Additionally, if the plane is hidden, the image will "
        //    "automatically be unloaded"

    };
#include "renderableplanetimevaryingimage_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderablePlaneTimeVaryingImage::Documentation() {
    documentation::Documentation doc = codegen::doc<Parameters>();
    doc.id = "base_renderable_plane_time_varying_image";
    return doc;

    //return codegen::doc<Parameters>("base_renderable_plane_time_varying_image");
}

RenderablePlaneTimeVaryingImage::RenderablePlaneTimeVaryingImage(
    const ghoul::Dictionary& dictionary)
    : RenderablePlane(dictionary)

    , _texturePath(TextureInfo)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);
    
    addProperty(_blendMode);
    
    _texturePath = absPath(p.texture).string();
    _textureFile = std::make_unique<ghoul::filesystem::File>(_texturePath.value());

    addProperty(_texturePath);
    _texturePath.onChange([this]() {loadTexture(); });
    _textureFile->setCallback(
        [this]() { _textureIsDirty = true; }
    );

    if (p.renderType.has_value()) {
        switch (*p.renderType) {
        case Parameters::RenderType::Background:
            setRenderBin(Renderable::RenderBin::Background);
            break;
        case Parameters::RenderType::Opaque:
            setRenderBin(Renderable::RenderBin::Opaque);
            break;
        case Parameters::RenderType::PreDeferredTransparency:
            setRenderBin(Renderable::RenderBin::PreDeferredTransparent);
            break;
        case Parameters::RenderType::PostDeferredTransparency:
            setRenderBin(Renderable::RenderBin::PostDeferredTransparent);
            break;
        case Parameters::RenderType::Overlay:
            setRenderBin(Renderable::RenderBin::Overlay);
            break;
        }
    }
    else {
        setRenderBin(Renderable::RenderBin::Opaque);
    }

    if (dictionary.hasKey(KeyLazyLoading)) {
        _isLoadingLazily = dictionary.value<bool>(KeyLazyLoading);

        if (_isLoadingLazily) {
            _enabled.onChange([this]() {
                if (_enabled) {
                    _textureIsDirty = true;
                }
                else {
                    BaseModule::TextureManager.release(_texture);
                    _texture = nullptr;
                }
            });
        }
    }
}

bool RenderablePlaneTimeVaryingImage::isReady() const {
    return RenderablePlane::isReady();
}

void RenderablePlaneTimeVaryingImage::initialize() {

    LDEBUG("sourcefiles size:" + std::to_string(_sourceFiles.size()));
       
    if (!extractMandatoryInfoFromDictionary()) {
        return;
    }
    extractTriggerTimesFromFileNames();
    computeSequenceEndTime();
        
    _textureFiles.resize(_sourceFiles.size());
      
    if (!_isLoadingLazily) {
        loadTexture();
    }
}

void RenderablePlaneTimeVaryingImage::initializeGL() {
    RenderablePlane::initializeGL();

    for (int i = 0; i < _sourceFiles.size(); ++i) {

        _textureFiles[i] = ghoul::io::TextureReader::ref().loadTexture(
                                                       absPath(_sourceFiles[i]).string());
        _textureFiles[i]->setInternalFormat(GL_COMPRESSED_RGBA);
        _textureFiles[i]->uploadTexture();
        _textureFiles[i]->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
        _textureFiles[i]->purgeFromRAM();
    }

}

bool RenderablePlaneTimeVaryingImage::extractMandatoryInfoFromDictionary()
{
    // Ensure that the source folder exists and then extract
    // the files with the same extension as <inputFileTypeString>
    namespace fs = std::filesystem;
    fs::path sourceFolder = _texturePath.value();
    if (std::filesystem::is_directory(sourceFolder)) {
        // Extract all file paths from the provided folder
        _sourceFiles.clear();
        namespace fs = std::filesystem;
        for (const fs::directory_entry& e : fs::directory_iterator(sourceFolder)) {
            if (e.is_regular_file()) {
                _sourceFiles.push_back(e.path().string());
            }
        }
        std::sort(_sourceFiles.begin(), _sourceFiles.end());
        // Ensure that there are available and valid source files left
        if (_sourceFiles.empty()) {
            LERROR(fmt::format(
                "{}: {} contains no {} files",
                _identifier, _texturePath, "extension"
            ));
            return false;
        }
    }
    else {
        LERROR(fmt::format(
            "{}: Plane sequence filepath {} is not a valid directory",
            _identifier,
            _texturePath
        ));
        return false;
    }
    _nStates = _sourceFiles.size();
    LDEBUG("returning true");
    return true;
}
void RenderablePlaneTimeVaryingImage::deinitializeGL() {
    _textureFile = nullptr;

    BaseModule::TextureManager.release(_texture);

    _textureFiles.clear();
    RenderablePlane::deinitializeGL();
}

void RenderablePlaneTimeVaryingImage::bindTexture() {
    _texture->bind();
}

void RenderablePlaneTimeVaryingImage::update(const UpdateData& data) {
    ZoneScoped
    RenderablePlane::update(data);
        
    if (!this->_enabled) {
        return;
    }

    const double currentTime = data.time.j2000Seconds();
    const bool isInInterval = (currentTime >= _startTimes[0]) &&
        (currentTime < _sequenceEndTime);
    //const bool isInInterval = true;
    if (isInInterval) {
        const size_t nextIdx = _activeTriggerTimeIndex + 1;
        if (
            // true => Previous frame was not within the sequence interval
            //_activeTriggerTimeIndex < 0 ||
            // true => We stepped back to a time represented by another state
            currentTime < _startTimes[_activeTriggerTimeIndex] ||
            // true => We stepped forward to a time represented by another state
            (nextIdx < _nStates && currentTime >= _startTimes[nextIdx]))
        {
            updateActiveTriggerTimeIndex(currentTime);
            _needsUpdate = true;

        } // else we're still in same state as previous frame (no changes needed)
    }
    else {
        //not in interval => set everything to false
        _activeTriggerTimeIndex = 0;
        _needsUpdate = false;
    }

    if ((_needsUpdate || _textureIsDirty) && !_isLoadingTexture) {
        _isLoadingTexture = true;
        loadTexture();
        _textureIsDirty = false;
    }

}

// Requires time to be formated as such: 'YYYY-MM-DDTHH-MM-SS-XXX'
void RenderablePlaneTimeVaryingImage::extractTriggerTimesFromFileNames() {
    constexpr const int FilenameSize = 23;
    // size(.png or .jpg)
    constexpr const int ExtSize = 4;

    for (const std::string& filePath : _sourceFiles) {
        LDEBUG("filepath " + filePath);
        const size_t strLength = filePath.size();
        // Extract the filename from the path (without extension)
        std::string timeString = filePath.substr(
            strLength - FilenameSize - ExtSize,
            FilenameSize - 1
        );
        // Ensure the separators are correct
        timeString.replace(4, 1, "-");
        timeString.replace(7, 1, "-");
        timeString.replace(13, 1, ":");
        timeString.replace(16, 1, ":");
        timeString.replace(19, 1, ".");
        const double triggerTime = Time::convertTime(timeString);
        LDEBUG("timestring " + timeString);
        _startTimes.push_back(triggerTime);
    }
}

void RenderablePlaneTimeVaryingImage::updateActiveTriggerTimeIndex
(double currentTime) {
    auto iter = std::upper_bound(_startTimes.begin(), _startTimes.end(), currentTime);
    if (iter != _startTimes.end()) {
        if (iter != _startTimes.begin()) {
            _activeTriggerTimeIndex = static_cast<int>(
                std::distance(_startTimes.begin(), iter)
                ) - 1;
        }
        else {
            _activeTriggerTimeIndex = 0;
        }
    }
    else {
        _activeTriggerTimeIndex = static_cast<int>(_nStates) - 1;
    }
}
void RenderablePlaneTimeVaryingImage::computeSequenceEndTime() {
    if (_nStates > 1) {
        const double lastTriggerTime = _startTimes[_nStates - 1];
        const double sequenceDuration = lastTriggerTime - _startTimes[0];
        const double averageStateDuration = sequenceDuration /
            (static_cast<double>(_nStates) - 1.0);
        _sequenceEndTime = lastTriggerTime + averageStateDuration;
    }
    else {
        // If there's just one state it should never disappear!
        _sequenceEndTime = DBL_MAX;
    }
}
void RenderablePlaneTimeVaryingImage::loadTexture() {
    if (_activeTriggerTimeIndex != -1) {
        _texture = _textureFiles[_activeTriggerTimeIndex].get();
        _isLoadingTexture = false;
    }
}

} // namespace openspace
