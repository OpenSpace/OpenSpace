/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <optional>

namespace {
    constexpr std::string_view KeyLazyLoading = "LazyLoading";
    constexpr std::string_view _loggerCat = "RenderablePlaneTimeVaryingImage";

    constexpr openspace::properties::Property::PropertyInfo SourceFolderInfo = {
        "SourceFolder",
        "Source Folder",
        "This value specifies the image directory that is loaded from disk and "
        "is used as a texture that is applied to this plane",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo RenderTypeInfo = {
       "RenderType",
       "Render Type",
       "This value specifies if the plane should be rendered in the Background, "
       "Opaque, Transparent, or Overlay rendering step",
        // @VISIBILITY(3.67)
        openspace::properties::Property::Visibility::AdvancedUser
    };

    struct [[codegen::Dictionary(RenderablePlaneTimeVaryingImage)]] Parameters {
        // [[codegen::verbatim(SourceFolderInfo.description)]]
        std::string sourceFolder;

        enum class [[codegen::map(openspace::Renderable::RenderBin)]] RenderType {
            Background,
            Opaque,
            PreDeferredTransparent [[codegen::key("PreDeferredTransparency")]],
            PostDeferredTransparent [[codegen::key("PostDeferredTransparency")]],
            Overlay
        };

        // [[codegen::verbatim(RenderTypeInfo.description)]]
        std::optional<RenderType> renderType;
    };
#include "renderableplanetimevaryingimage_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderablePlaneTimeVaryingImage::Documentation() {
    return codegen::doc<Parameters>(
        "base_renderable_plane_time_varying_image",
        RenderablePlane::Documentation()
    );
}

RenderablePlaneTimeVaryingImage::RenderablePlaneTimeVaryingImage(
                                                      const ghoul::Dictionary& dictionary)
    : RenderablePlane(dictionary)
    , _sourceFolder(SourceFolderInfo)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    addProperty(_blendMode);

    _sourceFolder = p.sourceFolder;
    if (!std::filesystem::is_directory(absPath(_sourceFolder))) {
        LERROR(std::format(
            "Time varying image, '{}' is not a valid directory",
            _sourceFolder.value()
        ));
    }

    addProperty(_sourceFolder);
    _sourceFolder.onChange([this]() { _texture = loadTexture(); });

    if (p.renderType.has_value()) {
        setRenderBin(codegen::map<Renderable::RenderBin>(*p.renderType));
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

void RenderablePlaneTimeVaryingImage::initialize() {
    RenderablePlane::initialize();
    const bool success = extractMandatoryInfoFromDictionary();
    if (!success) {
        return;
    }
    extractTriggerTimesFromFileNames();
    computeSequenceEndTime();
}

void RenderablePlaneTimeVaryingImage::initializeGL() {
    RenderablePlane::initializeGL();

    _textureFiles.resize(_sourceFiles.size());
    for (size_t i = 0; i < _sourceFiles.size(); i++) {
        _textureFiles[i] = ghoul::io::TextureReader::ref().loadTexture(
            absPath(_sourceFiles[i]),
            2
        );
        _textureFiles[i]->setInternalFormat(GL_COMPRESSED_RGBA);
        _textureFiles[i]->uploadTexture();
        _textureFiles[i]->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
        _textureFiles[i]->purgeFromRAM();
    }
    if (!_isLoadingLazily) {
        _texture = loadTexture();
    }
}

bool RenderablePlaneTimeVaryingImage::extractMandatoryInfoFromDictionary() {
    // Ensure that the source folder exists and then extract
    // the files with the same extension as <inputFileTypeString>
    namespace fs = std::filesystem;
    const fs::path sourceFolder = absPath(_sourceFolder);
    // Extract all file paths from the provided folder
    _sourceFiles.clear();
    namespace fs = std::filesystem;
    for (const fs::directory_entry& e : fs::directory_iterator(sourceFolder)) {
        if (e.is_regular_file()) {
            _sourceFiles.push_back(e.path());
        }
    }
    std::sort(_sourceFiles.begin(), _sourceFiles.end());
    // Ensure that there are available and valid source files left
    if (_sourceFiles.empty()) {
        LERROR(std::format(
            "{}: Plane sequence filepath '{}' was empty",
            _identifier, _sourceFolder.value()
        ));
        return false;
    }

    return true;
}

void RenderablePlaneTimeVaryingImage::deinitializeGL() {
    BaseModule::TextureManager.release(_texture);

    _textureFiles.clear();
    RenderablePlane::deinitializeGL();
}

void RenderablePlaneTimeVaryingImage::bindTexture() {
    if (_texture && !_textureIsDirty) {
        _texture->bind();
    }
}

void RenderablePlaneTimeVaryingImage::update(const UpdateData& data) {
    ZoneScoped;
    RenderablePlane::update(data);

    if (!_enabled || _startTimes.empty()) {
        return;
    }
    bool needsUpdate = false;
    const double currentTime = data.time.j2000Seconds();
    const bool isInInterval = (currentTime >= _startTimes[0]) &&
                              (currentTime < _sequenceEndTime);
    if (isInInterval) {
        const size_t nextIdx = _activeTriggerTimeIndex + 1;
        if (
            // true => we were not in an interval the previous frame but now we are
            _activeTriggerTimeIndex == -1 ||
            // true => We stepped back to a time represented by another state
            currentTime < _startTimes[_activeTriggerTimeIndex] ||
            // true => We stepped forward to a time represented by another state
            (nextIdx < _sourceFiles.size() && currentTime >= _startTimes[nextIdx]))
        {
            _activeTriggerTimeIndex = updateActiveTriggerTimeIndex(currentTime);
            needsUpdate = true;
        } // else we're still in same state as previous frame (no changes needed)
    }
    else {
        // not in interval => set everything to false
        _activeTriggerTimeIndex = -1;
        needsUpdate = false;
    }

    if (needsUpdate || _textureIsDirty) {
        _texture = loadTexture();
        _textureIsDirty = false;
    }
}

void RenderablePlaneTimeVaryingImage::render(const RenderData& data, RendererTasks& t) {
    if (!_startTimes.empty() &&
        data.time.j2000Seconds() < _sequenceEndTime &&
        data.time.j2000Seconds() > _startTimes[0])
    {
        glDisable(GL_CULL_FACE);
        RenderablePlane::render(data, t);
    }
}

// Requires time to be formated as such: 'YYYY-MM-DDTHH-MM-SS-XXX'
void RenderablePlaneTimeVaryingImage::extractTriggerTimesFromFileNames() {
    for (const std::filesystem::path& filePath : _sourceFiles) {
        // Extract the filename from the path (without extension)
        std::string timeString = filePath.stem().string();

        // Ensure the separators are correct
        timeString.replace(4, 1, "-");
        timeString.replace(7, 1, "-");
        timeString.replace(13, 1, ":");
        timeString.replace(16, 1, ":");
        timeString.replace(19, 1, ".");
        const double triggerTime = Time::convertTime(timeString);
        _startTimes.push_back(triggerTime);
    }
}

int RenderablePlaneTimeVaryingImage::updateActiveTriggerTimeIndex(
                                                                 double currentTime) const
{
    int activeIndex = 0;
    auto iter = std::upper_bound(_startTimes.begin(), _startTimes.end(), currentTime);
    if (iter != _startTimes.end()) {
        if (iter != _startTimes.begin()) {
            const std::ptrdiff_t idx = std::distance(_startTimes.begin(), iter);
            activeIndex = static_cast<int>(idx) - 1;
        }
        else {
            activeIndex = 0;
        }
    }
    else {
        activeIndex = static_cast<int>(_sourceFiles.size() - 1);
    }
    return activeIndex;
}

void RenderablePlaneTimeVaryingImage::computeSequenceEndTime() {
    if (_sourceFiles.size() > 1) {
        const double lastTriggerTime = _startTimes[_sourceFiles.size() - 1];
        const double sequenceDuration = lastTriggerTime - _startTimes[0];
        const double averageStateDuration = sequenceDuration /
            static_cast<double>(_sourceFiles.size() - 1);
        _sequenceEndTime = lastTriggerTime + averageStateDuration;
    }
}

ghoul::opengl::Texture* RenderablePlaneTimeVaryingImage::loadTexture() const {
    ghoul::opengl::Texture* texture = nullptr;
    if (_activeTriggerTimeIndex != -1) {
        texture = _textureFiles[_activeTriggerTimeIndex].get();
    }
    return texture;
}
} // namespace openspace
