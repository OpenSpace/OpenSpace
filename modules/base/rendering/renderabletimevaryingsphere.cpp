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

#include <modules/base/rendering/renderabletimevaryingsphere.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/util/sphere.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/misc/crc32.h>
#include <ghoul/opengl/texture.h>

namespace {
    // Extract J2000 time from file names
    // Requires files to be named as such: 'YYYY-MM-DDTHH-MM-SS-XXX.png'
    double extractTriggerTimeFromFileName(const std::filesystem::path& filePath) {
        // Extract the filename from the path (without extension)
        std::string timeString = filePath.stem().string();

        // Ensure the separators are correct
        timeString.replace(4, 1, "-");
        timeString.replace(7, 1, "-");
        timeString.replace(13, 1, ":");
        timeString.replace(16, 1, ":");
        timeString.replace(19, 1, ".");

        return openspace::Time::convertTime(timeString);
    }

    constexpr openspace::properties::Property::PropertyInfo TextureSourceInfo = {
        "TextureSource",
        "Texture Source",
        "A directory containing images that are loaded from disk and used for texturing "
        "the sphere. The images are expected to be equirectangular projections.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    struct [[codegen::Dictionary(RenderableTimeVaryingSphere)]] Parameters {
        // [[codegen::verbatim(TextureSourceInfo.description)]]
        std::filesystem::path textureSource [[codegen::directory()]];
    };
#include "renderabletimevaryingsphere_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderableTimeVaryingSphere::Documentation() {
    return codegen::doc<Parameters>("base_renderable_time_varying_sphere");
}

RenderableTimeVaryingSphere::RenderableTimeVaryingSphere(
                                                      const ghoul::Dictionary& dictionary)
    : RenderableSphere(dictionary)
    , _textureSourcePath(TextureSourceInfo)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _textureSourcePath = p.textureSource.string();
}

bool RenderableTimeVaryingSphere::isReady() const {
    return RenderableSphere::isReady() && _texture;
}

void RenderableTimeVaryingSphere::initializeGL() {
    RenderableSphere::initializeGL();

    extractMandatoryInfoFromSourceFolder();
    computeSequenceEndTime();
    loadTexture();
}

void RenderableTimeVaryingSphere::deinitializeGL() {
    _texture = nullptr;
    _files.clear();

    RenderableSphere::deinitializeGL();
}

void RenderableTimeVaryingSphere::extractMandatoryInfoFromSourceFolder() {
    // Ensure that the source folder exists and then extract
    // the files with the same extension as <inputFileTypeString>
    namespace fs = std::filesystem;
    const fs::path sourceFolder = absPath(_textureSourcePath);
    if (!std::filesystem::is_directory(sourceFolder)) {
        throw ghoul::RuntimeError(
            "Source folder for RenderableTimeVaryingSphere is not a valid directory"
        );
    }
    // Extract all file paths from the provided folder
    _files.clear();
    namespace fs = std::filesystem;
    for (const fs::directory_entry& e : fs::directory_iterator(sourceFolder)) {
        if (!e.is_regular_file()) {
            continue;
        }
        std::filesystem::path filePath = e.path();
        const double time = extractTriggerTimeFromFileName(filePath);
        std::unique_ptr<ghoul::opengl::Texture> t =
            ghoul::io::TextureReader::ref().loadTexture(filePath, 2);

        t->setInternalFormat(GL_COMPRESSED_RGBA);
        t->uploadTexture();
        t->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
        t->purgeFromRAM();

        _files.push_back({ std::move(filePath), time, std::move(t) });
    }

    std::sort(
        _files.begin(),
        _files.end(),
        [](const FileData& a, const FileData& b) {
            return a.time < b.time;
        }
    );
    // Ensure that there are available and valid source files left
    if (_files.empty()) {
        throw ghoul::RuntimeError(
            "Source folder for RenderableTimeVaryingSphere contains no files"
        );
    }
}

void RenderableTimeVaryingSphere::update(const UpdateData& data) {
    RenderableSphere::update(data);

    const double currentTime = data.time.j2000Seconds();
    const bool isInInterval = (currentTime >= _files[0].time) &&
        (currentTime < _sequenceEndTime);
    if (isInInterval) {
        const size_t nextIdx = _activeTriggerTimeIndex + 1;
        if (
            // true => We stepped back to a time represented by another state
            currentTime < _files[_activeTriggerTimeIndex].time ||
            // true => We stepped forward to a time represented by another state
            (nextIdx < _files.size() && currentTime >= _files[nextIdx].time))
        {
            updateActiveTriggerTimeIndex(currentTime);
            _textureIsDirty = true;
        } // else {we're still in same state as previous frame (no changes needed)}
    }
    else {
        // not in interval => set everything to false
        _activeTriggerTimeIndex = 0;
    }
    if (_textureIsDirty) {
        loadTexture();
        _textureIsDirty = false;
    }
}

void RenderableTimeVaryingSphere::bindTexture() {
    if (_texture) {
        _texture->bind();
    }
    else {
        unbindTexture();
    }
}

void RenderableTimeVaryingSphere::updateActiveTriggerTimeIndex(double currentTime) {
    auto iter = std::upper_bound(
        _files.begin(),
        _files.end(),
        currentTime,
        [](double value, const FileData& f) {
            return value < f.time;
        }
    );
    if (iter != _files.end()) {
        if (iter != _files.begin()) {
            const ptrdiff_t idx = std::distance(_files.begin(), iter);
            _activeTriggerTimeIndex = static_cast<int>(idx - 1);
        }
        else {
            _activeTriggerTimeIndex = 0;
        }
    }
    else {
        _activeTriggerTimeIndex = static_cast<int>(_files.size()) - 1;
    }
}

void RenderableTimeVaryingSphere::computeSequenceEndTime() {
    if (_files.size() > 1) {
        const double lastTriggerTime = _files[_files.size() - 1].time;
        const double sequenceDuration = lastTriggerTime - _files[0].time;
        const double averageStateDuration =
            sequenceDuration / (static_cast<double>(_files.size()) - 1.0);
        _sequenceEndTime = lastTriggerTime + averageStateDuration;
    }
}

void RenderableTimeVaryingSphere::loadTexture() {
    if (_activeTriggerTimeIndex != -1) {
        _texture = _files[_activeTriggerTimeIndex].texture.get();
    }
}

} // namespace openspace
