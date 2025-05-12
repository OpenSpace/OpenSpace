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

#include <modules/base/rendering/renderabletimevaryingsphere.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/properties/property.h>
#include <openspace/util/sphere.h>
#include <openspace/util/timemanager.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/misc/crc32.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <modules/fitsfilereader/include/wsafitshelper.h>

#include <string>


namespace {
    constexpr std::string_view _loggerCat = "RenderableTimeVaryingSphere";

    // Extract J2000 time from file names
    // Requires files to be named as such: 'YYYY-MM-DDTHH-MM-SS-XXX.png'
    double extractTriggerTimeFromISO8601FileName(const std::filesystem::path& filePath) {
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
    // Extract J2000 time from file names
    // Requires file to be named as example : 'wsa_202209291400R011_agong.fits'
    // Looks for timestamp after first '_'
    double extractTriggerTimeFromFitsFileName(const std::filesystem::path& filePath) {
        // Extract the filename from the path (without extension)
        std::string fileName = filePath.stem().string();

        std::string digits;
        bool foundDigits = false;

        // Iterate over the characters in the file name
        for (char c : fileName) {
            if (std::isdigit(c)) {
                // If current character is a digit, append it to digits string
                digits += c;
                foundDigits = true;
            }
            else {
                // If current character is not a digit, reset digits string
                digits.clear();
                foundDigits = false;
            }

            // If we have found at least 12 consecutive digits, break the loop
            if (digits.size() >= 12) {
                break;
            }
        }
        // If no digits found, return an empty string
        if (!foundDigits || digits.size() < 12) {
            return -1;
        }

        // Extract digits from the substring and construct ISO 8601 formatted string
        std::ostringstream oss;
        oss << digits.substr(0, 4) << "-" // Year
            << digits.substr(4, 2) << "-" // Month
            << digits.substr(6, 2) << "T" // Day
            << digits.substr(8, 2) << ":" // Hour
            << digits.substr(10, 2) << ":" // Minute
            << "00"
            << digits.substr(12, 2) << "." // Second
            << "000";
            //<< digits.substr(14); // Milliseconds

        return openspace::Time::convertTime(oss.str());
    }

    constexpr openspace::properties::Property::PropertyInfo TextureSourceInfo = {
        "TextureSource",
        "Texture Source",
        "This value specifies a directory or file names from where files loaded from "
        "disk and are used as a texture that is applied to this sphere. Images are "
        "expected to be an equirectangular projection.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo FitsLayerInfo = {
        "FitsLayer",
        "Surface Layer",
        "This value specifies which index in the fits file to use as texture.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo FitsLayerNameInfo = {
        "FitsLayerName",
        "Surface Layer Options",
        "This value specifies which name of the fits layer to use as texture.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo TextureFilterInfo = {
        "TextureFilter",
        "Texture Filter",
        "Option to choose nearest neighbor or linear filtering for the texture.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo SaveDownloadsOnShutdown = {
        "SaveDownloadsOnShutdown",
        "Save Downloads On Shutdown",
        "This is an option for if dynamically downloaded should be saved between runs "
        "or not.",
        openspace::properties::Property::Visibility::User
    };

    struct [[codegen::Dictionary(RenderableTimeVaryingSphere)]] Parameters {
        // [[codegen::verbatim(TextureSourceInfo.description)]]
        std::optional<std::string> textureSource;

        enum class [[codegen::map(openspace::RenderableTimeVaryingSphere::LoadingType)]] LoadingType {
            StaticLoading,
            DynamicDownloading
        };
        std::optional<LoadingType> loadingType;
        // dataID that corresponds to what dataset to use if using dynamicWebContent
        std::optional<int> dataID;
        // number Of Files To Queue is a max value of the amount of files to queue up
        // so that not to big of a data set is downloaded nessesarily.
        std::optional<int> numberOfFilesToQueue;
        std::optional<std::string> infoURL;
        std::optional<std::string> dataURL;
        // An index specifying which layer in the fits file to display
        std::optional<int> fitsLayer;
        std::optional<std::vector<std::string>> layerNames;
        // A positive number to be used to cap where the color range will lie.
        // Values higher than this number, and values lower than the negative of
        // this value will be overexposed.
        std::optional<float> fitsDataCapValue;
        // This is set to false by default and will delete all the downloaded content when
        // OpenSpace is shut down. Set to true to save all the downloaded files.
        std::optional<bool> cacheData;
        // Set if first/last file should render outside of the sequence interval
        std::optional<bool> showPastFirstAndLastImage;
        enum class [[codegen::map(openspace::RenderableTimeVaryingSphere::TextureFilter)]] TextureFilter {
            NearestNeighbor,
            Linear,
            Unspecified
        };
        std::optional<TextureFilter> textureFilter;
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
    , _fitsLayer(FitsLayerInfo)
    , _fitsLayerName(FitsLayerNameInfo)
    , _saveDownloadsOnShutdown(SaveDownloadsOnShutdown, false)
    , _textureFilterProperty(TextureFilterInfo)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _textureSourcePath = p.textureSource.value_or(_textureSourcePath);
    _textureSourcePath.setReadOnly(true);
    addProperty(_textureSourcePath); // Added only to show in GUI which file is active
    if (p.loadingType.has_value()) {
        _loadingType = codegen::map<LoadingType>(*p.loadingType);
    }
    else {
        _loadingType = LoadingType::StaticLoading;
    }
    if (p.fitsLayer.has_value()) {
        _fitsLayerTemp = *p.fitsLayer;
    }

    _textureFilterProperty.addOptions({
        {static_cast<int>(TextureFilter::NearestNeighbor), "No Filter"},
        {static_cast<int>(TextureFilter::Linear), "Linear smoothing"}
    });
    if (p.textureFilter.has_value()) {
        _textureFilterProperty =
            static_cast<int>(codegen::map<TextureFilter>(*p.textureFilter));
    }
    else {
        _textureFilterProperty = static_cast<int>(TextureFilter::NearestNeighbor);
    }
    _renderForever = p.showPastFirstAndLastImage.value_or(_renderForever);

    _textureFilterProperty.onChange([this]() {
        switch (_textureFilterProperty) {
        case(0):
            for (auto file = _files.begin(); file != _files.end(); ++file) {
                file->texture->setFilter(ghoul::opengl::Texture::FilterMode::Nearest);
            }
            break;
        case(1):
            for (auto file = _files.begin(); file != _files.end(); ++file) {
                file->texture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
            }
            break;
        }
        });
    addProperty(_textureFilterProperty);

    _fitsLayer.onChange([this]() {
        if (_loadingType == LoadingType::StaticLoading) {
            extractMandatoryInfoFromSourceFolder();
        }
        else {
            if (_isFitsFormat) {
                for (auto file = _files.begin(); file != _files.end(); ++file) {
                    file->texture =
                        loadTextureFromFits(file->path, _fitsLayer, _fitsDataCapValue);
                    file->texture->uploadTexture();
                    file->texture->setFilter(ghoul::opengl::Texture::FilterMode::Nearest);
                }
                loadTexture();
            }
        }
     });

    _fitsLayerName.onChange([this]() {
        if (_loadingType == LoadingType::StaticLoading) {
            extractMandatoryInfoFromSourceFolder();
        }
        else {
            if (_isFitsFormat) {
                for (auto file = _files.begin(); file != _files.end(); ++file) {
                    file->texture =
                        loadTextureFromFits(file->path, _fitsLayerName, _fitsDataCapValue);
                    file->texture->uploadTexture();
                    file->texture->setFilter(ghoul::opengl::Texture::FilterMode::Nearest);
                }
                loadTexture();
            }
        }
    });

    if (_loadingType == LoadingType::StaticLoading) {
        extractMandatoryInfoFromSourceFolder();
        computeSequenceEndTime();
        loadTexture();
    }
    if (_loadingType == LoadingType::DynamicDownloading) {
        _dataID = p.dataID.value_or(_dataID);
        if (!_dataID) {
            throw ghoul::RuntimeError(
                "If running with dynamic downloading, dataID needs to be specified"
            );
        }
        _nOfFilesToQueue = p.numberOfFilesToQueue.value_or(_nOfFilesToQueue);
        _infoURL = p.infoURL.value();
        if (_infoURL.empty()) {
            throw ghoul::RuntimeError("InfoURL has to be provided");
        }
        _dataURL = p.dataURL.value();
        if (_dataURL.empty()) {
            throw ghoul::RuntimeError("DataURL has to be provided");
        }
        _dynamicFileDownloader = std::make_unique<DynamicFileSequenceDownloader>(
            _dataID, _infoURL, _dataURL, _nOfFilesToQueue
        );

        _fitsDataCapValue = p.fitsDataCapValue.value_or(_fitsDataCapValue);
        if (p.layerNames.has_value()) {
            const std::vector<std::string> nameList = *p.layerNames;
            for (int i = 0; i < nameList.size(); ++i) {
                _layerNames.emplace( i, nameList[i] );
            }
            if (!p.fitsLayer.has_value()) {
                LWARNING(std::format(
                    "Specify 'FitsLayer' for scene graph node with DataID: {}.",
                    "Assuming first layer",
                    _dataID
                ));
                _fitsLayerTemp = 0;
            }
            _hasLayerNames = true;
            addProperty(_fitsLayerName);
        }
        else {
            _hasLayerNames = false;
            addProperty(_fitsLayer);
        }
        _saveDownloadsOnShutdown = p.cacheData.value_or(_saveDownloadsOnShutdown);
        addProperty(_saveDownloadsOnShutdown);
    }
}

bool RenderableTimeVaryingSphere::isReady() const {
    return RenderableSphere::isReady();
}

void RenderableTimeVaryingSphere::initializeGL() {
    RenderableSphere::initializeGL();
}

void RenderableTimeVaryingSphere::deinitializeGL() {
    _texture = nullptr;
    _files.clear();

    if (_loadingType == LoadingType::DynamicDownloading && _dynamicFileDownloader) {
        _dynamicFileDownloader->deinitialize(_saveDownloadsOnShutdown);
    }
    RenderableSphere::deinitializeGL();
}

void RenderableTimeVaryingSphere::readFileFromFits(std::filesystem::path path) {
    if (!_layerOptionsAdded) {
        if (_hasLayerNames) {
            for (int i = 0; i < nLayers(path); ++i) {
                auto it = _layerNames.find(i);
                if (it != _layerNames.end()) {
                    _fitsLayerName.addOption(it->first, it->second);
                }
                else {
                    LERROR(std::format(
                        "Could not add fits layer name"
                    ));
                }
            }
            _fitsLayerName = _fitsLayerTemp;
        }
        else {
            for (int i = 0; i < nLayers(path); ++i) {
                _fitsLayer.addOption(i, std::to_string(i + 1));
            }
            _fitsLayer = _fitsLayerTemp;
        }
        _layerOptionsAdded = true;
    }
    size_t layer = _hasLayerNames ? _fitsLayerName : _fitsLayer;
    std::unique_ptr<ghoul::opengl::Texture> t =
        loadTextureFromFits(path, layer, _fitsDataCapValue);
    if (t == nullptr) {
        return;
    }
    if (_textureFilterProperty == static_cast<int>(TextureFilter::NearestNeighbor) ||
        _textureFilterProperty == static_cast<int>(TextureFilter::Unspecified))
    {
        t->setFilter(ghoul::opengl::Texture::FilterMode::Nearest);
    }
    else if(_textureFilterProperty == static_cast<int>(TextureFilter::Linear)) {
        t->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
    }
    glm::vec2 minMaxDataValues = minMaxTextureDataValues(t);

    File newFile = {
        .status = File::FileStatus::Loaded,
        .path = path,
        .time = extractTriggerTimeFromFitsFileName(path),
        .texture = std::move(t),
        .dataMinMax = minMaxDataValues
    };

    const std::vector<File>::const_iterator iter = std::upper_bound(
        _files.begin(), _files.end(),
        newFile.time,
        [](double timeRef, const File& fileRef) {
            return timeRef < fileRef.time;
        }
    );
    _files.insert(iter, std::move(newFile));
}

void RenderableTimeVaryingSphere::readFileFromImage(std::filesystem::path path) {
    std::unique_ptr<ghoul::opengl::Texture> t =
        ghoul::io::TextureReader::ref().loadTexture(path.string(), 2);
    t->setInternalFormat(GL_COMPRESSED_RGBA);
    t->uploadTexture();
    if (_textureFilterProperty == static_cast<int>(TextureFilter::Linear) ||
        _textureFilterProperty == static_cast<int>(TextureFilter::Unspecified))
    {
        t->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
    }
    else if (_textureFilterProperty == static_cast<int>(TextureFilter::NearestNeighbor)) {
        t->setFilter(ghoul::opengl::Texture::FilterMode::Nearest);
    }
    t->purgeFromRAM();
    glm::vec2 minMaxDataValues = minMaxTextureDataValues(t);

    File newFile = {
        .status = File::FileStatus::Loaded,
        .path = path,
        .time = extractTriggerTimeFromISO8601FileName(path),
        .texture = std::move(t),
        .dataMinMax = minMaxDataValues
    };

    const std::vector<File>::const_iterator iter = std::upper_bound(
        _files.begin(), _files.end(),
        newFile.time,
        [](double timeRef, const File& fileRef) {
            return timeRef < fileRef.time;
        }
    );
    _files.insert(iter, std::move(newFile));
}

glm::vec2 RenderableTimeVaryingSphere::minMaxTextureDataValues(
                                               std::unique_ptr<ghoul::opengl::Texture>& t)
{
    const void* rawData = t->pixelData();
    const float* pixelData = static_cast<const float*>(rawData);
    size_t dataSize = t->dimensions().x * t->dimensions().y;
    float min = *std::min_element(pixelData, pixelData + dataSize);
    float max = *std::max_element(pixelData, pixelData + dataSize);
    return glm::vec2(min, max);
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
        std::string fileExtention = e.path().extension().string();
        if (fileExtention == ".fits") {
            _isFitsFormat = true;
            readFileFromFits(e.path());
        }
        else {
            readFileFromImage(e.path());
        }
    }
    // Ensure that there are available and valid source files left
    if (_files.empty()) {
        throw ghoul::RuntimeError(
            "Source folder for RenderableTimeVaryingSphere contains no files"
        );
    }
    else {
        computeSequenceEndTime();
    }
}

void RenderableTimeVaryingSphere::update(const UpdateData& data) {
    RenderableSphere::update(data);

    const double currentTime = data.time.j2000Seconds();
    const double deltaTime = global::timeManager->deltaTime();

    if (_loadingType == LoadingType::DynamicDownloading) {
        updateDynamicDownloading(currentTime, deltaTime);
    }

    _inInterval = _files.size() > 0 &&
        currentTime >= _files[0].time &&
        currentTime < _sequenceEndTime;

    if (_inInterval) {
        const size_t nextIdx = _activeTriggerTimeIndex + 1;
        if (
            // true => We stepped back to a time represented by another state
            currentTime < _files[_activeTriggerTimeIndex].time ||
            // true => We stepped forward to a time represented by another state
            (nextIdx < _files.size() && currentTime >= _files[nextIdx].time))
        {
            int previousIndex = _activeTriggerTimeIndex;
            updateActiveTriggerTimeIndex(currentTime);
            File& file = _files[_activeTriggerTimeIndex];
            if (file.status == File::FileStatus::Downloaded) {
                file.texture =
                    loadTextureFromFits(file.path, _fitsLayer, _fitsDataCapValue);
                file.status = File::FileStatus::Loaded;
            }
            if (previousIndex != _activeTriggerTimeIndex) {
                trackOldest(file);
                loadTexture();
            }
        }
        // The case when we jumped passed last file. where nextIdx is not < file.size()
        else if (currentTime >= _files[_activeTriggerTimeIndex].time &&
            _texture == nullptr)
        {
            loadTexture();
        }
    }
    if (!_firstUpdate && _isUsingColorMap) {
        _dataMinMaxValues = _files[_activeTriggerTimeIndex].dataMinMax;
    }
    if (_textureIsDirty) [[unlikely]] {
        loadTexture();
        _textureIsDirty = false;
    }
}

void RenderableTimeVaryingSphere::render(const RenderData& data, RendererTasks& task) {
    if (_files.empty()) {
        return;
    }
    if (!_inInterval && !_renderForever) {
        return;
    }
    RenderableSphere::render(data, task);
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
        _files.cbegin(),
        _files.cend(),
        currentTime,
        [](double value, const File& f) { return value < f.time; }
    );
    if (iter != _files.cend()) {
        if (iter != _files.cbegin()) {
            const ptrdiff_t idx = std::distance(_files.cbegin(), iter);
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

void RenderableTimeVaryingSphere::updateDynamicDownloading(const double currentTime,
                                                                   const double deltaTime)
{
    _dynamicFileDownloader->update(currentTime, deltaTime);
    const std::vector<std::filesystem::path>& filesToRead =
        _dynamicFileDownloader->downloadedFiles();
    for (std::filesystem::path filePath : filesToRead) {
        std::string fileExtention = filePath.extension().string();
        if (fileExtention == ".fits") {
            _isFitsFormat = true;
            readFileFromFits(filePath);
        }
        else {
            readFileFromImage(filePath);
        }
    }
    if (!filesToRead.empty()) {
        computeSequenceEndTime();
        updateActiveTriggerTimeIndex(currentTime);
    }
    if (_firstUpdate) {
        const bool isInInterval = _files.size() > 0 &&
            currentTime >= _files[0].time &&
            currentTime < _sequenceEndTime;
        if (isInInterval &&
            _activeTriggerTimeIndex != -1 &&
            _activeTriggerTimeIndex<_files.size())
        {
            _firstUpdate = false;
            loadTexture();
        }
    }
    // if all files are moved into _sourceFiles then we can
    // empty the DynamicFileSequenceDownloader _downloadedFiles;
    _dynamicFileDownloader->clearDownloaded();
}

void RenderableTimeVaryingSphere::computeSequenceEndTime() {
    if (_files.size() == 0) {
        _sequenceEndTime = 0.f;
    }
    else if (_files.size() == 1) {
        _sequenceEndTime = _files[0].time + 7200.f;
        if (_loadingType == LoadingType::StaticLoading && !_renderForever) {
            //TODO: Alternativly check at construction and throw exeption.
            LWARNING("Only one file in data set, but ShowAtAllTimes set to false. "
                "Using arbitrary duration to visualize data file instead");
        }
    }
    else if (_files.size() > 1) {
        const double lastTriggerTime = _files[_files.size() - 1].time;
        const double sequenceDuration = lastTriggerTime - _files[0].time;
        const double averageCadence=
            sequenceDuration / (static_cast<double>(_files.size()) - 1.0);
        // A multiplier of 3 to the average cadence is added at the end as a buffer
        // 3 because if you start it just before new data came in, you might just be
        // outside the sequence end time otherwise
        _sequenceEndTime = lastTriggerTime + 3 * averageCadence;
    }
}

void RenderableTimeVaryingSphere::loadTexture() {
    if (_activeTriggerTimeIndex != -1 &&
        static_cast<size_t>(_activeTriggerTimeIndex) < _files.size())
    {
        _texture = _files[_activeTriggerTimeIndex].texture.get();
        showCorrectFileName();
    }
}

void RenderableTimeVaryingSphere::trackOldest(File& file) {
    if (file.status == File::FileStatus::Loaded) {
        _loadedFiles.push(&file);
    }
    // Repopulate the queue if new File makes the queue full
    if (!_loadedFiles.empty() &&
        _loadingType != LoadingType::StaticLoading &&
        _loadedFiles.size() >= _maxLoadedFiles)
    {
        File* oldest = _loadedFiles.front();
        oldest->status = File::FileStatus::Downloaded;
        oldest->texture = nullptr;
        _loadedFiles.pop();
    }
}

void RenderableTimeVaryingSphere::showCorrectFileName() {
    _textureSourcePath = _files[_activeTriggerTimeIndex].path.filename().string();
}

} // namespace openspace
