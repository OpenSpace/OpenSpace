/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include <modules/base/rendering/screenspacetimevaryingimageonline.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/json.h>
#include <openspace/util/timemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/texture.h>
#include <fstream>

namespace {
    constexpr std::string_view _loggerCat = "ScreenSpaceTimeVaryingImageOnline";

    constexpr openspace::properties::Property::PropertyInfo FileInfo = {
        "FilePath",
        "File Path",
        "The file path to the data containing information about when to display which "
        "image.",
        openspace::properties::Property::Visibility::User
    };

    // This `ScreenSpaceRenderable` displays an image based on the current in-game
    // simulation time. The image shown is selected from a JSON file containing
    // timestamp-URL pairs. The image with the closest timestamp before or equal to the
    // current time is displayed.
    //
    // Example JSON format:
    // {
    //   "files": [
    //     {"timestamp": "2024-05-10 15:00:00.0","url": "https://example.com/image1.png"},
    //     {"timestamp": "2024-05-10 15:02:00.0","url": "https://example.com/image2.png"}
    //   ]
    // }
    struct [[codegen::Dictionary(ScreenSpaceTimeVaryingImageOnline)]] Parameters {
        // [[codegen::verbatim(FileInfo.description)]]
        std::filesystem::path filePath;
    };
#include "screenspacetimevaryingimageonline_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation ScreenSpaceTimeVaryingImageOnline::Documentation() {
    return codegen::doc<Parameters>("base_screenspace_time_varying_image_online");
}

ScreenSpaceTimeVaryingImageOnline::ScreenSpaceTimeVaryingImageOnline(
                                                      const ghoul::Dictionary& dictionary)
    : ScreenSpaceRenderable(dictionary)
    , _jsonFilePath(FileInfo, "")
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _jsonFilePath = p.filePath.string();
    _jsonFilePath.onChange([this]() {
        loadJsonData(_jsonFilePath.value());
    });
    addProperty(_jsonFilePath);
}

void ScreenSpaceTimeVaryingImageOnline::initialize() {
    ScreenSpaceRenderable::initialize();

    loadJsonData(_jsonFilePath.value());
}

void ScreenSpaceTimeVaryingImageOnline::deinitializeGL() {
    _texture = nullptr;

    ScreenSpaceRenderable::deinitializeGL();
}

void ScreenSpaceTimeVaryingImageOnline::loadJsonData(const std::filesystem::path& path) {
    std::ifstream file = std::ifstream(path);
    if (!file.is_open()) {
        throw ghoul::RuntimeError(std::format("Could not open JSON file at '{}'", path));
    }

    nlohmann::json json;
    file >> json;

    if (json.find("files") == json.end()) {
        throw ghoul::RuntimeError(std::format(
            "Error loading JSON file. No 'files' was found in '{}'", path
        ));
    }

    _timestamps.clear();
    _urls.clear();

    for (const nlohmann::json& entry : json["files"]) {
        const std::string& timestamp = entry["timestamp"].get<std::string>();
        double j2000 = Time::convertTime(timestamp);
        _timestamps.push_back(j2000);
        _urls[j2000] = entry["url"].get<std::string>();
    }

    std::sort(_timestamps.begin(), _timestamps.end());
    computeSequenceEndTime();
}

void ScreenSpaceTimeVaryingImageOnline::computeSequenceEndTime() {
    if (_timestamps.size() <= 1) {
        return;
    }

    double first = _timestamps.front();
    double last = _timestamps.back();
    double avg = (last - first) / static_cast<double>(_timestamps.size() - 1);
    // Extend end time so the last value remains visible for one more interval
    _sequenceEndTime = last + avg;
}

void ScreenSpaceTimeVaryingImageOnline::update() {
    if (_timestamps.empty()) {
        return;
    }

    const double current = global::timeManager->time().j2000Seconds();

    if (current < _timestamps.front() || current >= _sequenceEndTime) {
        _activeIndex = -1;
        _texture = nullptr;
        _currentUrl.clear();
        return;
    }

    if (current >= _timestamps.front() && current < _sequenceEndTime) {
        if (int idx = activeIndex(current);  idx != _activeIndex) {
            _activeIndex = idx;
            std::string url = _urls[_timestamps[_activeIndex]];
            if (_currentUrl != url) {
                _currentUrl = url;
                loadImage(url);
            }
        }
    }
    else {
        _activeIndex = -1;
    }

    if (_imageFuture.valid() && DownloadManager::futureReady(_imageFuture)) {
        const DownloadManager::MemoryFile imageFile = _imageFuture.get();
        _imageFuture = std::future<DownloadManager::MemoryFile>();
        if (imageFile.corrupted) {
            LERROR(std::format("Error loading image from URL '{}'", _currentUrl));
            return;
        }

        try {
            std::unique_ptr<ghoul::opengl::Texture> texture =
                ghoul::io::TextureReader::ref().loadTexture(
                    reinterpret_cast<void*>(imageFile.buffer),
                    imageFile.size,
                    2,
                    imageFile.format
                );

            if (texture) {
                glPixelStorei(GL_UNPACK_ALIGNMENT, 1);

                if (texture->format() == ghoul::opengl::Texture::Format::Red) {
                    texture->setSwizzleMask({ GL_RED, GL_RED, GL_RED, GL_ONE });
                }

                texture->uploadTexture();
                texture->setFilter(ghoul::opengl::Texture::FilterMode::LinearMipMap);
                texture->purgeFromRAM();

                _texture = std::move(texture);
                _objectSize = _texture->dimensions();
            }
        }
        catch (const ghoul::io::TextureReader::InvalidLoadException& e) {
            LERRORC(e.component, e.message);
        }
    }
}

int ScreenSpaceTimeVaryingImageOnline::activeIndex(double currentTime) const {
    if (_timestamps.empty()) {
        return -1;
    }

    auto it = std::upper_bound(_timestamps.begin(), _timestamps.end(), currentTime);
    if (it == _timestamps.begin()) {
        return 0;
    }
    else if (it != _timestamps.end()) {
        return static_cast<int>(std::distance(_timestamps.begin(), it)) - 1;
    }
    else {
        return static_cast<int>(_timestamps.size()) - 1;
    }
}

void ScreenSpaceTimeVaryingImageOnline::loadImage(const std::string& imageUrl) {
    if (_imageFuture.valid()) {
        return;
    }

    _imageFuture = global::downloadManager->fetchFile(
        imageUrl,
        [](const DownloadManager::MemoryFile&) {},
        [](const std::string& e) {
            LERROR(std::format("Download failed: {}", e));
        }
    );
}

void ScreenSpaceTimeVaryingImageOnline::bindTexture() {
    if (_texture) [[likely]] {
        _texture->bind();
    }
}

} // namespace openspace
