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

#ifndef __OPENSPACE_MODULE_BASE___SCREENSPACETIMEVARYINGIMAGEONLINE___H__
#define __OPENSPACE_MODULE_BASE___SCREENSPACETIMEVARYINGIMAGEONLINE___H__

#include <openspace/rendering/screenspacerenderable.h>

#include <openspace/engine/downloadmanager.h>
#include <openspace/properties/misc/stringproperty.h>
#include <filesystem>

namespace ghoul::opengl { class Texture; }

namespace openspace {

namespace documentation { struct Documentation; }

class ScreenSpaceTimeVaryingImageOnline : public ScreenSpaceRenderable {
public:
    explicit ScreenSpaceTimeVaryingImageOnline(const ghoul::Dictionary& dictionary);

    void initialize() override;
    void deinitializeGL() override;
    void update() override;

    static documentation::Documentation Documentation();

private:
    void bindTexture() override;
    void loadJsonData(const std::filesystem::path& path);
    void computeSequenceEndTime();
    void loadImage(const std::string& imageUrl);
    int activeIndex(double currentTime) const;

    properties::StringProperty _jsonFilePath;

    std::future<DownloadManager::MemoryFile> _imageFuture;
    std::map<double, std::string> _urls;
    std::string _currentUrl;
    std::unique_ptr<ghoul::opengl::Texture> _texture;
    std::vector<double> _timestamps;

    int _activeIndex = -1;
    double _sequenceEndTime = 0.0;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_BASE___SCREENSPACETIMEVARYINGIMAGEONLINE___H__
