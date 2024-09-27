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

#ifndef __OPENSPACE_MODULE_BASE___SCREENSPACEIMAGEONLINE___H__
#define __OPENSPACE_MODULE_BASE___SCREENSPACEIMAGEONLINE___H__

#include <openspace/rendering/screenspacerenderable.h>
#include <openspace/engine/downloadmanager.h>
#include <openspace/properties/stringproperty.h>
#include <future>  // Added for std::future
#include <string>  // Added for std::string

namespace ghoul::opengl { class Texture; }

namespace openspace {

    namespace documentation { struct Documentation; }

    class ScreenSpaceImageOnline : public ScreenSpaceRenderable {
    public:
        ScreenSpaceImageOnline(const ghoul::Dictionary& dictionary);
        virtual ~ScreenSpaceImageOnline() override;

        bool deinitializeGL() override;

        void update() override;

        static documentation::Documentation Documentation();

    protected:
        bool _jsonDownloaded = false; // New flag for JSON download status
        bool _textureIsDirty;
        std::future<DownloadManager::MemoryFile> _imageFuture;
        std::future<DownloadManager::MemoryFile> _jsonFuture;
        properties::StringProperty _texturePath;
        std::string _currentTextureUrl;
        std::chrono::system_clock::time_point _lastCheckedTime;
        std::string _lastCheckedSoftwareTimestamp;


    private:
        void bindTexture() override;

        std::future<DownloadManager::MemoryFile> downloadJsonToMemory(
            const std::string& url);  // Method for downloading JSON

        std::future<DownloadManager::MemoryFile> downloadImageToMemory(
            const std::string& url);  // Remains for image download

        std::string findClosestTimestampUrl(const std::string& jsonContent, const std::string& currentTimestamp);
        std::string getClosestUrl(const std::string& jsonFilePath, const std::string& currentTime);

        std::unique_ptr<ghoul::opengl::Texture> _texture;

        std::string getCurrentTimestamp();
        void loadImage(const std::string& imageUrl);
        // Method for getting current timestamp
        bool isCloser(const std::string& current, const std::string& newTimestamp, const std::string& closest); // Method for timestamp comparison
        std::time_t parseTimestamp(const std::string& timestamp);
        std::string formatTimeForData(std::string_view timeStr);
    };

} // namespace openspace

#endif // __OPENSPACE_MODULE_BASE___SCREENSPACEIMAGEONLINE___H__
