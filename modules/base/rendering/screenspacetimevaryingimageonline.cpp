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

    constexpr openspace::properties::Property::PropertyInfo JsonFileInfo = {
        "JsonFilePath",
        "JSON File Path",
        "The file path to the JSON data.",
        openspace::properties::Property::Visibility::User
    };

    // This `ScreenSpaceRenderable` displays an image based on the current in-game
    // simulation time. The image shown is selected from a JSON file containing
    // timestamp-URL pairs. The image with the closest timestamp before or equal to the
    // current time is displayed.
    // Example JSON format:
    // {
    //   "files": [
    //     {"timestamp": "2024-05-10 15:00:17.0","url": "https://example.com/image1.png"},
    //     {"timestamp": "2024-05-10 15:02:17.0","url": "https://example.com/image2.png"}
    //   ]
    // }
    struct [[codegen::Dictionary(ScreenSpaceTimeVaryingImageOnline)]] Parameters {
        // [[codegen::verbatim(JsonFileInfo.description)]]
        std::string jsonFilePath;
    };
#include "screenspacetimevaryingimageonline_codegen.cpp"
} // namespace

namespace openspace {

ScreenSpaceTimeVaryingImageOnline::ScreenSpaceTimeVaryingImageOnline(
                                                      const ghoul::Dictionary& dictionary)
    : ScreenSpaceRenderable(dictionary)
    , _jsonFilePath(JsonFileInfo, "")
{
    const Parameters p = codegen::bake<Parameters>(dictionary);


    _jsonFilePath = absPath(p.jsonFilePath).string();
    addProperty(_jsonFilePath);

    _jsonFilePath.onChange([this]() {
        loadJsonData(_jsonFilePath);
        });

    loadJsonData(_jsonFilePath);
}

documentation::Documentation ScreenSpaceTimeVaryingImageOnline::Documentation() {
    return codegen::doc<Parameters>("base_screenspace_time_varying_image_online");
}

bool ScreenSpaceTimeVaryingImageOnline::deinitializeGL() {
    _texture = nullptr;
    return ScreenSpaceRenderable::deinitializeGL();
}

void ScreenSpaceTimeVaryingImageOnline::loadJsonData(const std::string& path) {
    std::ifstream file(path);
    if (!file.is_open()) {
        throw ghoul::RuntimeError(std::format("Could not open JSON file at '{}'", path));
        return;
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

    for (const auto& entry : json["files"]) {
        const std::string& timestamp = entry["timestamp"].get<std::string>();
        double j2000 = Time::convertTime(timestamp);
        _timestamps.push_back(j2000);
        _urls[j2000] = entry["url"].get<std::string>();
    }

    std::sort(_timestamps.begin(), _timestamps.end());
    computeSequenceEndTime();
}

void ScreenSpaceTimeVaryingImageOnline::computeSequenceEndTime() {
    if (_timestamps.size() > 1) {
        double first = _timestamps.front();
        double last = _timestamps.back();
        double avg = (last - first) / static_cast<double>(_timestamps.size() - 1);
        // Extend end time so the last value remains visible for one more interval
        _sequenceEndTime = last + avg;
    }
}

void ScreenSpaceTimeVaryingImageOnline::update() {
    if (_timestamps.empty()) {
        return;
    }

    double current = global::timeManager->time().j2000Seconds();

    if (current >= _timestamps.front() && current < _sequenceEndTime) {
        int newIdx = activeIndex(current);
        if (newIdx != _activeIndex) {
            _activeIndex = newIdx;
            std::string url = _urls[_timestamps[_activeIndex]];
            loadImage(url);
        }
    }
    else {
        _activeIndex = -1;
    }
}

int ScreenSpaceTimeVaryingImageOnline::activeIndex(double currentTime) const {
    auto it = std::upper_bound(_timestamps.begin(), _timestamps.end(), currentTime);
    if (it != _timestamps.end()) {
        if (it != _timestamps.begin()) {
            return static_cast<int>(std::distance(_timestamps.begin(), it)) - 1;
        }
        return 0;
    }
    return static_cast<int>(_timestamps.size()) - 1;
}

void ScreenSpaceTimeVaryingImageOnline::loadImage(const std::string& imageUrl) {
    if (!_imageFuture.valid()) {
        _imageFuture = global::downloadManager->fetchFile(
            imageUrl,
            [](const DownloadManager::MemoryFile&) {},
            [](const std::string& e) {
                LERROR(std::format("Download failed: {}", e));
            }
        );
    }

    if (_imageFuture.valid() && DownloadManager::futureReady(_imageFuture)) {
        const DownloadManager::MemoryFile imageFile = _imageFuture.get();
        if (imageFile.corrupted) {
            LERROR(std::format("Corrupted image: {}", imageUrl));
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

void ScreenSpaceTimeVaryingImageOnline::bindTexture() {
    if (_texture) [[likely]] {
        _texture->bind();
    }
}

} // namespace openspace
