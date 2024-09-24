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

#include <modules/base/rendering/screenspaceimageonline.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/util/timemanager.h>
#include <openspace/engine/downloadmanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/programobject.h>
#include <json/json.hpp>
#include <optional>
#include <iostream>
#include <iomanip>
#include <sstream>
#include <string_view>
#include <fstream>

namespace {
    constexpr std::string_view _loggerCat = "ScreenSpaceImageOnline";

    constexpr openspace::properties::Property::PropertyInfo TextureInfo = {
        "URL",
        "Image URL",
        "The URL of the webpage containing the JSON data with image URLs."
        "The image with the closest timestamp to the current time will be displayed.",
        openspace::properties::Property::Visibility::User
    };

    struct [[codegen::Dictionary(ScreenSpaceImageOnline)]] Parameters {
        std::optional<std::string> url [[codegen::key("URL")]];
    };
#include "screenspaceimageonline_codegen.cpp"
} // namespace

namespace openspace {

    documentation::Documentation ScreenSpaceImageOnline::Documentation() {
        return codegen::doc<Parameters>("base_screenspace_image_online");
    }

    ScreenSpaceImageOnline::ScreenSpaceImageOnline(const ghoul::Dictionary& dictionary)
        : ScreenSpaceRenderable(dictionary)
        , _textureIsDirty(false)
        , _jsonDownloaded(false)  // Initialize the JSON download flag
        , _texturePath(TextureInfo)
    {
        const Parameters p = codegen::bake<Parameters>(dictionary);

        std::string identifier;
        if (dictionary.hasValue<std::string>(KeyIdentifier)) {
            identifier = dictionary.value<std::string>(KeyIdentifier);
        }
        else {
            identifier = "ScreenSpaceImageOnline";
        }
        identifier = makeUniqueIdentifier(identifier);
        setIdentifier(std::move(identifier));

        _texturePath.onChange([this]() { _textureIsDirty = true; });
        _texturePath = p.url.value_or(_texturePath);
        addProperty(_texturePath);

    }

    ScreenSpaceImageOnline::~ScreenSpaceImageOnline() {}

    bool ScreenSpaceImageOnline::deinitializeGL() {
        _texture = nullptr;
        return ScreenSpaceRenderable::deinitializeGL();
    }

    //void ScreenSpaceImageOnline::update() {
    //    std::cout << "Update called. Texture is dirty: " << _textureIsDirty << ", JSON downloaded: " << _jsonDownloaded << std::endl;

    //    if (_textureIsDirty && !_jsonDownloaded) {
    //        std::cout << "Initiating JSON download..." << std::endl;

    //        if (!_jsonFuture.valid()) {
    //            std::future<DownloadManager::MemoryFile> future = downloadJsonToMemory(_texturePath);
    //            if (future.valid()) {
    //                _jsonFuture = std::move(future);
    //            }
    //        }

    //        if (_jsonFuture.valid() && DownloadManager::futureReady(_jsonFuture)) {
    //            const DownloadManager::MemoryFile jsonFile = _jsonFuture.get();

    //            if (jsonFile.corrupted) {
    //                LERROR(std::format("Error loading JSON from URL '{}'", _texturePath.value()));
    //                return;
    //            }

    //            try {
    //                std::cout << "Parsing JSON..." << std::endl;
    //                auto jsonData = nlohmann::json::parse(std::string(reinterpret_cast<char*>(jsonFile.buffer), jsonFile.size));
    //                std::cout << "JSON parsed successfully!" << std::endl;
    //                std::cout << jsonData.dump(4) << std::endl; // Print formatted JSON for debugging

    //                // Find the image URL with the closest timestamp
    //                std::string bestImageUrl;
    //                std::string currentTimestamp = getCurrentTimestamp();
    //                std::string closestTimestamp;
    //                for (const auto& file : jsonData["files"]) {
    //                    std::string timestamp = file["timestamp"];
    //                    // Compare timestamps and find the closest one
    //                    if (closestTimestamp.empty() || isCloser(currentTimestamp, timestamp, closestTimestamp)) {
    //                        closestTimestamp = timestamp;
    //                        bestImageUrl = file["url"];
    //                    }
    //                }

    //                if (!bestImageUrl.empty()) {
    //                    std::cout << "Image to download URL: " << bestImageUrl << std::endl;
    //                    downloadImageToMemory(bestImageUrl);
    //                }
    //            }
    //            catch (const std::exception& e) {
    //                LERRORC("JSON parsing error", e.what());
    //            }

    //            // Mark JSON as downloaded
    //            _jsonDownloaded = true;  // Set the flag to true after successful download
    //        }
    //    }
    //}


    //void ScreenSpaceImageOnline::update() {
    //    if (_textureIsDirty) {
    //        // Read the JSON from the local file system
    //        std::string jsonFilePath = _texturePath.value();
    //        std::ifstream file(jsonFilePath);

    //        if (!file.is_open()) {
    //            LERROR("Could not open JSON file at " + jsonFilePath);
    //            return;
    //        }

    //        // Parse the JSON data
    //        std::stringstream buffer;
    //        buffer << file.rdbuf();
    //        std::string jsonContent = buffer.str();

    //        // Find the closest image URL based on timestamp
    //        std::string closestUrl = findClosestTimestampUrl(jsonContent);

    //        if (!closestUrl.empty()) {
    //            // Now load the image from the closest URL
    //            loadImage(closestUrl);
    //        }
    //    }
    //}


    void ScreenSpaceImageOnline::update() {
        auto currentTime = std::chrono::system_clock::now();

        // Check if one minute has passed since the last check
        if (std::chrono::duration_cast<std::chrono::minutes>(currentTime - _lastCheckedTime).count() >= 1) {
            _lastCheckedTime = std::chrono::system_clock::now();  // Update the last checked time

            // Read the JSON from the local file system
            std::string jsonFilePath = _texturePath.value();
            std::ifstream file(jsonFilePath);

            if (!file.is_open()) {
                LERROR("Could not open JSON file at " + jsonFilePath);
                return;
            }

            // Parse the JSON data
            std::stringstream buffer;
            buffer << file.rdbuf();
            std::string jsonContent = buffer.str();
            std::string currentTimestamp = getCurrentTimestamp();

            // Find the closest image URL based on the current timestamp
            std::string closestUrl = findClosestTimestampUrl(jsonContent, currentTimestamp);

            if (!closestUrl.empty() && closestUrl != _currentTextureUrl) {
                // Update the currently used texture URL and load the new image
                _currentTextureUrl = closestUrl;
                _textureIsDirty = true;  // Mark texture as dirty to reload
            }
        }

        // Load the image if the texture is dirty
        if (_textureIsDirty && !_currentTextureUrl.empty()) {
            loadImage(_currentTextureUrl); // Load the image based on the current URL
            _textureIsDirty = false;        // Reset the dirty flag
        }
    }

    /*std::future<DownloadManager::MemoryFile> ScreenSpaceImageOnline::downloadJsonToMemory(const std::string& url) {
        if (url.empty()) {
            LERROR("Attempted to download JSON with an empty URL.");
            return std::future<DownloadManager::MemoryFile>();
        }

        return global::downloadManager->fetchFile(
            url,
            [url](const DownloadManager::MemoryFile&) {
                LDEBUG("Download to memory finished for JSON data");
            },
            [url](const std::string& err) {
                LERROR(std::format("Download to memory failed for JSON data from URL '{}': {}", url, err));
            }
        );
    }*/


std::future<DownloadManager::MemoryFile> ScreenSpaceImageOnline::downloadImageToMemory(
                                                                   const std::string& url)
{
    return global::downloadManager->fetchFile(
        url,
        [url](const DownloadManager::MemoryFile&) {
            LDEBUG("Download to memory finished for screen space image");
        },
        [url](const std::string& err) {
            LDEBUG(std::format(
                "Download to memory failed for screen space image: {}", err
            ));
        }
    );
}


std::string ScreenSpaceImageOnline::findClosestTimestampUrl(const std::string& jsonContent, const std::string& currentTimestamp) {
    auto json = nlohmann::json::parse(jsonContent);

    std::string closestUrl;
    std::string closestTimestamp;
    bool foundFuture = false; // Flag to indicate if we've found a future timestamp

    for (const auto& file : json["files"]) {
        if (file.contains("timestamp") && file.contains("url")) {
            std::string timestamp = file["timestamp"];
            std::string url = file["url"];
            //TODO: fix this
            // Check if the timestamp is greater than or equal to the current timestamp
            if (timestamp <= currentTimestamp) {
                // We found a future timestamp
                if (!foundFuture || timestamp < closestTimestamp) {
                    closestTimestamp = timestamp;
                    closestUrl = url;
                    foundFuture = true;
                }
            }
            else {
                // If no future timestamps found yet, track the latest past timestamp
                if (closestTimestamp.empty() || timestamp > closestTimestamp) {
                    closestTimestamp = timestamp;
                    closestUrl = url;
                }
            }
        }
    }

    if (closestUrl.empty()) {
        LERROR("No valid URL found for the given timestamps.");
    }

    return closestUrl;
}






void ScreenSpaceImageOnline::loadImage(const std::string& imageUrl) {
    // Download or load the image from the local disk or the web


    if (!_imageFuture.valid()) {
        std::future<DownloadManager::MemoryFile> future = downloadImageToMemory(
            imageUrl
        );
        if (future.valid()) {
            _imageFuture = std::move(future);
        }
    }

    if (_imageFuture.valid() && DownloadManager::futureReady(_imageFuture)) {
        const DownloadManager::MemoryFile imageFile = _imageFuture.get();

        if (imageFile.corrupted) {
            LERROR(std::format(
                "Error loading image from URL '{}'", imageUrl
            ));
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
                // Images don't need to start on 4-byte boundaries, for example if the
                // image is only RGB
                glPixelStorei(GL_UNPACK_ALIGNMENT, 1);

                if (texture->format() == ghoul::opengl::Texture::Format::Red) {
                    texture->setSwizzleMask({ GL_RED, GL_RED, GL_RED, GL_ONE });
                }

                texture->uploadTexture();
                texture->setFilter(ghoul::opengl::Texture::FilterMode::LinearMipMap);
                texture->purgeFromRAM();

                _texture = std::move(texture);
                _objectSize = _texture->dimensions();
                _textureIsDirty = false;
            }
        }
        catch (const ghoul::io::TextureReader::InvalidLoadException& e) {
            _textureIsDirty = false;
            LERRORC(e.component, e.message);
        }
    }
}



//bool ScreenSpaceImageOnline::isCloser(const std::string& current, const std::string& newTimestamp, const std::string& closest) {
//    // Convert timestamp strings to std::chrono::time_point
//    auto parseTimestamp = [](const std::string& timestamp) {
//        std::tm tm = {};
//        std::istringstream ss(timestamp);
//        ss >> std::get_time(&tm, "%Y-%m-%d %H:%M:%S");
//        return std::chrono::system_clock::from_time_t(std::mktime(&tm));
//        };
//
//    auto currentTimePoint = parseTimestamp(current);
//    auto newTimePoint = parseTimestamp(newTimestamp);
//    auto closestTimePoint = parseTimestamp(closest);
//
//    // Calculate the absolute differences
//    auto newDiff = std::abs(std::chrono::duration_cast<std::chrono::seconds>(newTimePoint - currentTimePoint).count());
//    auto closestDiff = std::abs(std::chrono::duration_cast<std::chrono::seconds>(closestTimePoint - currentTimePoint).count());
//
//    return newDiff < closestDiff; // Return true if newTimestamp is closer
//}


bool ScreenSpaceImageOnline::isCloser(const std::string& current, const std::string& newTimestamp, const std::string& closest) {
    if (closest.empty()) {
        return true;  // If there's no closest yet, the new one is automatically closer
    }

    // Convert timestamps to a comparable format (e.g., std::chrono::time_point)
    auto currentTime = parseTimestamp(current);
    auto newTime = parseTimestamp(newTimestamp);
    auto closestTime = parseTimestamp(closest);

    // Compare the differences between the current time and the timestamps
    return std::abs((newTime - currentTime).count()) < std::abs((closestTime - currentTime).count());
}

std::chrono::system_clock::time_point ScreenSpaceImageOnline::parseTimestamp(const std::string& timestamp) {
    std::tm t = {};
    std::stringstream ss(timestamp);
    ss >> std::get_time(&t, "%Y-%m-%d %H:%M:%S");
    return std::chrono::system_clock::from_time_t(std::mktime(&t));
}

std::string ScreenSpaceImageOnline::formatTimeForData(std::string_view timeStr) {
    std::string formattedTime(timeStr);

    // Convert to the format YYYY-MM-DDTHH:MM:00Z
    std::replace(formattedTime.begin(), formattedTime.end(), 'T', ' ');
    //std::replace(formattedTime.begin(), formattedTime.end(), '.', ' '); // Remove milliseconds

    std::tm tm = {};
    std::istringstream ss(formattedTime);
    ss >> std::get_time(&tm, "%Y %b %d %H:%M:%S");

    std::ostringstream oss;
    oss << std::put_time(&tm, "%Y-%m-%d %H:%M:00.0");
    return oss.str();
}


std::string ScreenSpaceImageOnline::getCurrentTimestamp() {
    // Get the current time as a string
    std::string_view currentTimeStr = global::timeManager->time().UTC();
    std::string formattedTime(currentTimeStr);

    // Parse the formatted time
    std::tm tm = {};
    std::istringstream ss(formattedTime);
    ss >> std::get_time(&tm, "%Y %b %d %H:%M:%S");

    // Create a new formatted string in JSON timestamp format
    std::ostringstream jsonTimestamp;
    jsonTimestamp << std::put_time(&tm, "%Y-%m-%d %H:%M:%S") << ".0"; // Adding .0 for milliseconds

    std::string formattedTime2 = formatTimeForData(currentTimeStr);

    std::cout << "Formatted time: " << formattedTime2 << std::endl;

    return formattedTime2;
    //return jsonTimestamp.str();
}



//std::string ScreenSpaceImageOnline::getCurrentTimestamp() {
//    // Get the current time
//    std::string_view currentTimeStr = global::timeManager->time().UTC();
//    std::string formattedTime(currentTimeStr);
//
//    // Parse the formatted time
//    std::tm tm = {};
//    std::istringstream ss(formattedTime);
//    ss >> std::get_time(&tm, "%Y %b %d %H:%M:%S");
//
//    // Convert to time_point
//    auto timePoint = std::chrono::system_clock::from_time_t(std::mktime(&tm));
//
//    // Create a new formatted string in JSON timestamp format
//    std::ostringstream jsonTimestamp;
//    jsonTimestamp << std::put_time(&tm, "%Y-%m-%d %H:%M:%S") << ".0"; // Adding .0 for milliseconds
//
//    return jsonTimestamp.str();
//}

void ScreenSpaceImageOnline::bindTexture() {
    if (_texture) {
        _texture->bind();
    }
}

} // namespace openspace
