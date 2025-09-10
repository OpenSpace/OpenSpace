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

#ifndef __OPENSPACE_MODULE_FITSFILEREADER___RENDERABLETIMEVARYINGFITSSPHERE___H__
#define __OPENSPACE_MODULE_FITSFILEREADER___RENDERABLETIMEVARYINGFITSSPHERE___H__

#include <modules/base/rendering/renderablesphere.h>

#include <openspace/properties/misc/optionproperty.h>
#include <openspace/util/dynamicfilesequencedownloader.h>

namespace openspace {

struct RenderData;
struct UpdateData;
namespace documentation { struct Documentation; }

class RenderableTimeVaryingFitsSphere : public RenderableSphere {
public:
    enum class LoadingType {
        StaticLoading,
        DynamicDownloading
    };

    explicit RenderableTimeVaryingFitsSphere(const ghoul::Dictionary& dictionary);

    void deinitializeGL() override;

    void update(const UpdateData& data) override;
    void render(const RenderData& data, RendererTasks& rendererTask) override;

    static documentation::Documentation Documentation();

protected:
    void bindTexture() override;

private:
    struct File {
        enum class FileStatus {
            Downloaded,
            Loaded
        };
        FileStatus status = FileStatus::Downloaded;
        std::filesystem::path path;
        double time = 0.0;
        std::unique_ptr<ghoul::opengl::Texture> texture;
        glm::vec2 dataMinMax = { 0.f, 1.f };
        bool operator<(const File& other) const noexcept {
            return time < other.time;
        }
    };
    void loadTexture();
    void trackOldest(File& file);
    void showCorrectFileName();
    void extractMandatoryInfoFromSourceFolder();
    void readFileFromFits(std::filesystem::path path);
    glm::vec2 minMaxTextureDataValues(std::unique_ptr<ghoul::opengl::Texture>& t);
    void updateActiveTriggerTimeIndex(double currenttime);
    void computeSequenceEndTime();
    void updateDynamicDownloading(double currentTime, double deltaTime);

    properties::OptionProperty _fitsLayerName;
    // An option to keep or delete the downloads from dynamic downloader on shutdown
    // Deletes on default
    properties::BoolProperty _saveDownloadsOnShutdown;
    properties::OptionProperty _textureFilterProperty;
    properties::StringProperty _textureSource;

    // If there's just one state it should never disappear!
    double _sequenceEndTime = std::numeric_limits<double>::max();
    // Static Loading on default / if not specified
    LoadingType _loadingType = LoadingType::StaticLoading;
    // A data ID that corresponds to what dataset to use if using DynamicDownloading
    int _dataID = -1;
    // Number of files to queue up at a time
    int _nFilesToQueue = 10;
    // To keep track of oldest file
    std::deque<File*> _loadedFiles;
    // Max number of files loaded at once
    size_t _maxLoadedFiles = 100;
    std::string _infoUrl;
    std::string _dataUrl;
    std::map<int, std::string> _layerNames;
    std::map<int, std::pair<float, float>> _layerMinMaxCaps;

    int _fitsLayerTemp = -1;
    // If there's just one state it should never disappear
    bool _renderForever = true;
    bool _inInterval = false;

    // DynamicFileSequenceDownloader downloads and updates the renderable with
    // data downloaded from the web
    std::unique_ptr<DynamicFileSequenceDownloader> _dynamicFileDownloader;
    std::deque<File> _files;
    int _activeTriggerTimeIndex = 0;

    bool _firstUpdate = true;
    bool _layerOptionsAdded = false;
    ghoul::opengl::Texture* _texture = nullptr;
    bool _textureIsDirty = true;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_FITSFILEREADER___RENDERABLETIMEVARYINGFITSSPHERE___H__
