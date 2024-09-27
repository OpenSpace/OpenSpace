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
#include <ctime>

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

        _lastCheckedSoftwareTimestamp = getCurrentTimestamp();

    }

    ScreenSpaceImageOnline::~ScreenSpaceImageOnline() {}

    bool ScreenSpaceImageOnline::deinitializeGL() {
        _texture = nullptr;
        return ScreenSpaceRenderable::deinitializeGL();
    }

    //void ScreenSpaceImageOnline::update() {
    //    auto currentTime = std::chrono::system_clock::now();

    //    // Check if one minute has passed since the last check
    //    if (std::chrono::duration_cast<std::chrono::minutes>(currentTime - _lastCheckedTime).count() >= 1) {
    //        _lastCheckedTime = std::chrono::system_clock::now();  // Update the last checked time

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
    //        std::string currentTimestamp = getCurrentTimestamp();

    //        // Find the closest image URL based on the current timestamp
    //        std::string closestUrl = findClosestTimestampUrl(jsonContent, currentTimestamp);

    //        if (!closestUrl.empty()) {
    //            // Update the currently used texture URL and load the new image
    //            _currentTextureUrl = closestUrl;
    //            _textureIsDirty = true;  // Mark texture as dirty to reload
    //        }
    //    }

    //    // Load the image if the texture is dirty
    //    if (_textureIsDirty && !_currentTextureUrl.empty()) {
    //        loadImage(_currentTextureUrl); // Load the image based on the current URL
    //        _textureIsDirty = false;        // Reset the dirty flag
    //    }
    //}




    void ScreenSpaceImageOnline::update() {
        std::string currentTimestamp = getCurrentTimestamp();  // Get the current software timestamp

        // Check if one minute has passed since the last check
        if (currentTimestamp != _lastCheckedSoftwareTimestamp) {

            std::time_t lastCheckedTime = parseTimestamp(_lastCheckedSoftwareTimestamp);
            std::time_t currentTime = parseTimestamp(currentTimestamp);

            // Calculate the time difference in minutes
            double timeDiff = std::abs(std::difftime(currentTime, lastCheckedTime)) / 60.0;

            if (timeDiff >= 15) {  // If 15 minutes has passed
                _lastCheckedSoftwareTimestamp = currentTimestamp;  // Update the last checked timestamp

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

                // Find the closest image URL based on the current timestamp
                std::string closestUrl = findClosestTimestampUrl(jsonContent, currentTimestamp);

                if (!closestUrl.empty()) {
                    // Update the currently used texture URL and load the new image
                    _currentTextureUrl = closestUrl;
                    _textureIsDirty = true;  // Mark texture as dirty to reload
                }
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

    std::cout << "JSON file: " << std::endl << json << std::endl;

    std::string closestUrl;
    double closestTimeDiff = std::numeric_limits<double>::max();

    // Parse the current timestamp
    std::time_t currentTime = parseTimestamp(currentTimestamp);

    for (const auto& fileEntry : json["files"]) {
        std::string timestamp = fileEntry["timestamp"].get<std::string>();
        std::time_t fileTime = parseTimestamp(timestamp);

        // Calculate time difference
        double timeDiff = std::abs(difftime(fileTime, currentTime));

        // Check for the closest timestamp
        if (timeDiff < closestTimeDiff) {
            closestTimeDiff = timeDiff;
            closestUrl = fileEntry["url"].get<std::string>();
            std::cout << "Closest URL: " << closestUrl << " with time difference: " << closestTimeDiff << std::endl;
        }
    }

    if (closestUrl.empty()) {
        std::cerr << "No valid URL found for the given timestamps." << std::endl;
    }

    return closestUrl;
}



std::time_t ScreenSpaceImageOnline::parseTimestamp(const std::string& timestamp) {
    std::tm tm = {};
    std::istringstream ss(timestamp);
    ss >> std::get_time(&tm, "%Y-%m-%d %H:%M:%S");
    return std::mktime(&tm);
}

std::string ScreenSpaceImageOnline::getClosestUrl(const std::string& jsonFilePath, const std::string& currentTime)
{
    std::ifstream file(jsonFilePath);
    nlohmann::json root;
    file >> root;

    std::string closestUrl = "";
    double closestTimeDiff = std::numeric_limits<double>::max();

    for (const auto& fileEntry : root["files"]) {
        std::string timestamp = fileEntry["timestamp"].get<std::string>();
        double timeDiff = std::abs(std::difftime(std::stoi(timestamp.substr(0, 10)), std::stoi(currentTime.substr(0, 10))));

        if (timeDiff < closestTimeDiff) {
            closestTimeDiff = timeDiff;
            closestUrl = fileEntry["url"].get<std::string>();
        }
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
