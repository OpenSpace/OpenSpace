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

#ifndef __OPENSPACE_MODULE_BASE___RENDERABLETIMEVARYINGSPHERE___H__
#define __OPENSPACE_MODULE_BASE___RENDERABLETIMEVARYINGSPHERE___H__

#include <modules/base/rendering/renderablesphere.h>

#include <openspace/properties/optionproperty.h>
#include <openspace/util/dynamicfilesequencedownloader.h>

//namespace ghoul::opengl { class Texture; }

namespace openspace {

struct RenderData;
struct UpdateData;

namespace documentation { struct Documentation; }

class RenderableTimeVaryingSphere : public RenderableSphere {
public:
    //0: static loading and static downloading
    //1: dynamic loading and static downloading
    //2: dynamic loading and dynamic downloading
    enum class LoadingType {
        StaticLoading = 0,
        DynamicLoading = 1,
        DynamicDownloading = 2
    };
    enum class TextureFilter {
        NearestNeighbor = 0,
        Linear = 1,
        Unspecified =2
    };

    RenderableTimeVaryingSphere(const ghoul::Dictionary& dictionary);

    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;

    void update(const UpdateData& data) override;
    void render(const RenderData& data, RendererTasks& rendererTask) override;

    static documentation::Documentation Documentation();
    struct File {
        enum class FileStatus {
            Downloaded,
            Loaded
        };
        FileStatus status;
        std::filesystem::path path;
        double time;
        std::unique_ptr<ghoul::opengl::Texture> texture;
        glm::vec2 dataMinMax;

        bool operator< (const File& other) const {
            return time < other.time;
        }
    };
protected:
    void bindTexture() override;

private:
    void loadTexture();
    void showCorrectFileName();
    void extractMandatoryInfoFromSourceFolder();
    void readFileFromImage(std::filesystem::path path);
    void readFileFromFits(std::filesystem::path path);
    void setMinMaxValues(std::unique_ptr<ghoul::opengl::Texture>& t, File& file);
    void updateActiveTriggerTimeIndex(double currenttime);
    void computeSequenceEndTime();
    void setupDynamicDownloading(const std::optional<int>& dataID,
        const std::optional<int>& numberOfFiles,
        const std::optional<std::string>& infoURL,
        const std::optional<std::string>& dataURL);
    void updateDynamicDownloading(const double currentTime, const double deltaTime);
    void definePropertyCallbackFunctions();

    // If there's just one state it should never disappear!
    double _sequenceEndTime = std::numeric_limits<double>::max();
    // Static Loading on default / if not specified
    LoadingType _loadingType = LoadingType::StaticLoading;
    // dataID that corresponds to what dataset to use if using DynamicDownloading
    int _dataID;
    // number of files to queue up at a time
    int _nOfFilesToQueue = 10;
    std::string _infoURL = "";
    std::string _dataURL = "";
    properties::OptionProperty _fitsLayer;
    int _fitsLayerTemp;
    // An option to keep or delete the downloads from dynamic downloader on shutdown
    // Deletes on default
    properties::BoolProperty _deleteDownloadsOnShutdown;
    bool _isLoadingStateFromDisk = false;
    //  DynamicFileSequenceDownloader downloads and updates the renderable with
    //  data downloaded from the web.
    std::unique_ptr<DynamicFileSequenceDownloader> _dynamicFileDownloader;
    properties::OptionProperty _textureFilterProperty;
    TextureFilter _textureFilter;

    std::vector<File> _files;
    int _activeTriggerTimeIndex = 0;

    properties::StringProperty _textureSourcePath;
    bool _isFitsFormat = false;
    bool _firstUpdate = true;
    bool _layerOptionsAdded = false;
    ghoul::opengl::Texture* _texture = nullptr;
    bool _textureIsDirty = false;

};

} // namespace openspace

#endif // __OPENSPACE_MODULE_BASE___RENDERABLETIMEVARYINGSPHERE___H__
