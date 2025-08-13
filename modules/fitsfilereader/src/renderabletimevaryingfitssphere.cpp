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

#include <modules/fitsfilereader/include/renderabletimevaryingfitssphere.h>

#include <modules/fitsfilereader/include/wsafitshelper.h>
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
#include <string>

namespace {
    constexpr std::string_view _loggerCat = "RenderableTimeVaryingFitsSphere";

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

        return openspace::Time::convertTime(oss.str());
    }

    constexpr openspace::properties::Property::PropertyInfo TextureSourceInfo = {
        "TextureSource",
        "Texture Source",
        "A directory on disk from which to load the texture files for the sphere.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo FitsLayerInfo = {
        "FitsLayer",
        "Texture Layer",
        "The index, a whole positive number, of the layer in the FITS file to use as "
        "texture. If not specified, the first layer in the data will be used regardless. "
        "When specified, that data layer will be the option used.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo FitsLayerNameInfo = {
        "LayerNames",
        "Texture Layer Options",
        "This value specifies which name of the fits layer to use as texture.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo TextureFilterInfo = {
        "TextureFilter",
        "Texture Filter",
        "Option to choose nearest neighbor or linear filtering for the texture.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo SaveDownloadsOnShutdown = {
        "SaveDownloadsOnShutdown",
        "Save Downloads On Shutdown",
        "This is an option for if dynamically downloaded files should be saved for the"
        "next run or not.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    // This `Renderable` reads a data sequence from specifically FITS files and makes
    // textures from them and wraps them onto a sphere. A sequence is a data source
    // consisting of multiple data files that each correspond to a specific
    // time and is therefore time varying like the name of the renderable suggests.
    //
    // `LoadingType` can be specified in two ways;
    //
    // 1. `StaticLoading`: In this case, all data will be loaded on startup, from the
    // location specified in the `TextureSource` parameter. Other required parameters are
    // `LayerNames` and `LayerMinMaxCapValues` that require at least one entry each.
    // `LayerNames` assigns a name to an index that corresponds to a layer in the FITS
    // data to use. For each entry in `LayerNames`, there should also be a matching entry
    // in the `LayerMinMaxCapValues` that specifies the min and max range for the data.
    //
    // 2. `DynamicDownloading`: This case downloads the data during runtime. In addition
    // to `LayerNames` and `LayerMinMaxCapValues`, a few more parameters are required in
    // this case: `InfoURL` together with `DataID` will construct a URL that is used for a
    // HTTP request that returns meta data. `DataURL` and `DataID`, together with this
    // meta data, will be used in constructing another HTTP request that returns the list
    // with data files. The `DataID` specify which data source to use.
    //
    // In addition, but not required, the `FitsLayer` parameter can be specified to use a
    // specific data layer for the sphere texture. The index should match one of the
    // layers in `LayerNames` and `LayerMinMaxCapValues`. `CacheData` and `showAtAllTimes`
    // are two other optional parameters.
    struct [[codegen::Dictionary(RenderableTimeVaryingFitsSphere)]] Parameters {
        // [[codegen::verbatim(TextureSourceInfo.description)]]
        std::optional<std::filesystem::path> textureSource;

        enum class [[codegen::map(
            openspace::RenderableTimeVaryingFitsSphere::LoadingType)]] LoadingType
        {
            StaticLoading,
            DynamicDownloading
        };

        // Choose type of loading:
        // StaticLoading: Download and load files on startup.
        // DynamicDownloading: Download and load files during run time.
        std::optional<LoadingType> loadingType;

        // This is a max value of the amount of files to queue up
        // so that not to big of a data set is downloaded.
        std::optional<int> numberOfFilesToQueue;

        // A data ID that corresponds to what dataset to use if using dynamic downloading.
        std::optional<int> dataID;

        // A URL that returns a JSON formated page with metadata needed for the dataURL.
        std::optional<std::string> infoURL;

        // A URL that returns a JSON formated page with a list of each available file.
        std::optional<std::string> dataURL;

        // [[codegen::verbatim(FitsLayerInfo.description)]]
        std::optional<int> fitsLayer;

        // [[codegen::verbatim(FitsLayerNameInfo.description)]]
        std::optional<ghoul::Dictionary> layerNames;

        // A range per layer to be used to cap where the color range will lie.
        // Values outside of range will be overexposed, i.e. data values below the min
        // or above the max, will all be set to the min and max color value in range.
        std::optional<ghoul::Dictionary> layerMinMaxCapValues;

        // This is set to false by default and will delete all the downloaded content when
        // OpenSpace is shut down, if using dynamic downloading. Set to true to save all
        // the downloaded files.
        std::optional<bool> cacheData;

        // Set if first/last file should render when time is outside of the sequence
        // interval. This can be used regardless of LoadingType. If this value is not
        // specified, the field lines are shown at all times.
        std::optional<bool> showAtAllTimes;
    };
#include "renderabletimevaryingfitssphere_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderableTimeVaryingFitsSphere::Documentation() {
    return codegen::doc<Parameters>(
        "fitsfilereader_renderable_time_varying_fits_sphere",
        RenderableSphere::Documentation()
    );
}

RenderableTimeVaryingFitsSphere::RenderableTimeVaryingFitsSphere(
                                                      const ghoul::Dictionary& dictionary)
    : RenderableSphere(dictionary)
    , _fitsLayerName(FitsLayerNameInfo)
    , _saveDownloadsOnShutdown(SaveDownloadsOnShutdown, false)
    , _textureFilterProperty(TextureFilterInfo)
    , _textureSource(TextureSourceInfo)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    if (p.textureSource.has_value()) {
        _textureSource = p.textureSource->string();
    }

    _textureSource.setReadOnly(true);
    addProperty(_textureSource); // Added only to show in GUI which file is active

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
        { static_cast<int>(ghoul::opengl::Texture::FilterMode::Nearest), "No Filter" },
        {
            static_cast<int>(ghoul::opengl::Texture::FilterMode::Linear),
            "Linear smoothing"
        }
    });
    _textureFilterProperty = static_cast<int>(
        ghoul::opengl::Texture::FilterMode::Nearest
    );

    _renderForever = p.showAtAllTimes.value_or(_renderForever);

    _textureFilterProperty.onChange([this]() {
        switch (_textureFilterProperty) {
            case static_cast<int>(ghoul::opengl::Texture::FilterMode::Nearest):
                for (File& file : _files) {
                    if (file.texture) {
                        file.texture->setFilter(
                            ghoul::opengl::Texture::FilterMode::Nearest
                        );
                    }
                }
                break;
            case static_cast<int>(ghoul::opengl::Texture::FilterMode::Linear):
                for (File& file : _files) {
                    if (file.texture) {
                        file.texture->setFilter(
                            ghoul::opengl::Texture::FilterMode::Linear
                        );
                    }
                }
                break;
        }
    });
    addProperty(_textureFilterProperty);

    _fitsLayerName.onChange([this]() {
        if (_loadingType == LoadingType::StaticLoading) {
            extractMandatoryInfoFromSourceFolder();
        }
        else {
            for (File& file : _files) {
                // This if-statement might seem redundant since we're loading a texture
                // here, but the files without texture should not be loaded to save memory
                if (file.status == File::FileStatus::Loaded) {
                    std::pair<float, float> minMax = _layerMinMaxCaps.at(_fitsLayerName);
                    file.texture = loadTextureFromFits(file.path, _fitsLayerName, minMax);
                    file.texture->uploadTexture();
                    using FM = ghoul::opengl::Texture::FilterMode;
                    if (_textureFilterProperty == static_cast<int>(FM::Nearest)) {
                        file.texture->setFilter(FM::Nearest);
                    }
                    else if (_textureFilterProperty == static_cast<int>(FM::Linear)) {
                        file.texture->setFilter(FM::Linear);
                    }
                }
            }
            loadTexture();
        }
    });
    addProperty(_fitsLayerName);

    if (_loadingType == LoadingType::StaticLoading) {
        extractMandatoryInfoFromSourceFolder();
        computeSequenceEndTime();
        loadTexture();
    }

    if (_loadingType == LoadingType::DynamicDownloading) {
        if (!p.dataID.has_value()) {
            throw ghoul::RuntimeError(
                "If running with dynamic downloading, dataID needs to be specified"
            );
        }
        _dataID = *p.dataID;

        _nFilesToQueue = p.numberOfFilesToQueue.value_or(_nFilesToQueue);

        if (!p.infoURL.has_value()) {
            throw ghoul::RuntimeError(
                "If running with dynamic downloading, infoURL needs to be specified"
            );
        }
        _infoUrl = *p.infoURL;

        if (!p.dataURL.has_value()) {
            throw ghoul::RuntimeError(
                "If running with dynamic downloading, dataURL needs to be specified"
            );
        }
        _dataUrl = *p.dataURL;

        if (p.layerMinMaxCapValues.has_value()) {
            const ghoul::Dictionary d = *p.layerMinMaxCapValues;
            for (std::string_view intKey : d.keys()) {
                const ghoul::Dictionary& pair = d.value<ghoul::Dictionary>(intKey);
                if (pair.hasValue<double>("1") && pair.hasValue<double>("2")) {
                    std::pair<float, float> minMax = std::pair(
                        static_cast<float>(pair.value<double>("1")),
                        static_cast<float>(pair.value<double>("2"))
                    );
                    std::pair<int, std::pair<float, float>> mapPair = std::pair(
                        std::stoi(std::string(intKey)),
                        minMax
                    );
                    _layerMinMaxCaps.emplace(mapPair);
                }
                else {
                    throw ghoul::RuntimeError(std::format(
                        "The two values at {} needs to be of type double, a number with "
                        "at least one decimal", intKey
                    ));
                }
            }
        }

        if (!p.layerNames.has_value()) {
            throw ghoul::RuntimeError("At least one name for one layer is required");
        }
        const ghoul::Dictionary names = *p.layerNames;
        for (std::string_view key : names.keys()) {
            const int k = std::stoi(std::string(key));
            std::string v = names.value<std::string>(key);
            _layerNames.emplace(k, std::move(v));
        }
        if (!p.fitsLayer.has_value()) {
            LWARNING(std::format(
                "Specify 'FitsLayer' for scene graph node with DataID: {}. ",
                "Assuming first layer",
                _dataID
            ));
            _fitsLayerTemp = 0;
        }
    }
    _saveDownloadsOnShutdown = p.cacheData.value_or(_saveDownloadsOnShutdown);
    addProperty(_saveDownloadsOnShutdown);
}

void RenderableTimeVaryingFitsSphere::deinitializeGL() {
    if (_loadingType == LoadingType::DynamicDownloading && _dynamicFileDownloader) {
        _dynamicFileDownloader->deinitialize(_saveDownloadsOnShutdown);
    }
    _texture = nullptr;
    _files.clear();
    RenderableSphere::deinitializeGL();
}

void RenderableTimeVaryingFitsSphere::readFileFromFits(std::filesystem::path path) {
    if (!_layerOptionsAdded) {
        for (const std::pair<const int, std::string>& name : _layerNames) {
            _fitsLayerName.addOption(name.first, name.second);
        }
        _fitsLayerName = _fitsLayerTemp;
        _layerOptionsAdded = true;
    }

    std::pair<float, float> minMax = _layerMinMaxCaps.at(_fitsLayerName);
    std::unique_ptr<ghoul::opengl::Texture> t =
        loadTextureFromFits(path, _fitsLayerName, minMax);

    if (!t) {
        return;
    }

    using FilterMode = ghoul::opengl::Texture::FilterMode;
    if (_textureFilterProperty == static_cast<int>(FilterMode::Nearest)) {
        t->setFilter(FilterMode::Nearest);
    }
    else if (_textureFilterProperty == static_cast<int>(FilterMode::Linear)) {
        t->setFilter(FilterMode::Linear);
    }

    glm::vec2 minMaxDataValues = minMaxTextureDataValues(t);

    File newFile = {
        .status = File::FileStatus::Loaded,
        .path = path,
        .time = extractTriggerTimeFromFitsFileName(path),
        .texture = std::move(t),
        .dataMinMax = minMaxDataValues
    };

    const std::deque<File>::const_iterator iter = std::upper_bound(
        _files.begin(),
        _files.end(),
        newFile.time,
        [](double timeRef, const File& fileRef) {
            return timeRef < fileRef.time;
        }
    );
    std::deque<File>::iterator it = _files.insert(iter, std::move(newFile));
    trackOldest(*it);
}

glm::vec2 RenderableTimeVaryingFitsSphere::minMaxTextureDataValues(
                                               std::unique_ptr<ghoul::opengl::Texture>& t)
{
    const glm::ivec3 dims = glm::ivec3(t->dimensions());
    const int width = dims.x;
    const int height = dims.y;

    std::vector<float> pixelValues;
    pixelValues.reserve(width * height * 4);

    for (int y = 0; y < height; ++y) {
        for (int x = 0; x < width; ++x) {
            glm::vec4 texel = t->texelAsFloat(x, y);
            pixelValues.push_back(texel.r);
            pixelValues.push_back(texel.g);
            pixelValues.push_back(texel.b);
            pixelValues.push_back(texel.a);
        }
    }
    if (!pixelValues.empty()) {
        float min = *std::min_element(pixelValues.begin(), pixelValues.end());
        float max = *std::max_element(pixelValues.begin(), pixelValues.end());
        return glm::vec2(min, max);
    }
    else {
        return glm::vec2(0.f, 1.f);
    }
}

void RenderableTimeVaryingFitsSphere::extractMandatoryInfoFromSourceFolder() {
    // Ensure that the source folder exists and then extract
    // the files with the same extension as <inputFileTypeString>
    namespace fs = std::filesystem;
    const fs::path sourceFolder = _textureSource.stringValue();
    if (!std::filesystem::is_directory(sourceFolder)) {
        throw ghoul::RuntimeError(std::format(
            "Source folder '{}' for RenderableTimeVaryingFitsSphere is not a valid "
            "directory", sourceFolder
        ));
    }
    // Extract all file paths from the provided folder
    _files.clear();
    namespace fs = std::filesystem;
    for (const fs::directory_entry& e : fs::directory_iterator(sourceFolder)) {
        if (!e.is_regular_file()) {
            continue;
        }
        if (e.path().extension() != ".fits") {
            throw ghoul::RuntimeError(std::format(
                "File extension for '{}' required to be .fits", e.path())
            );
        }

        readFileFromFits(e.path());
    }
    // Ensure that there are available and valid source files left
    if (_files.empty()) {
        throw ghoul::RuntimeError(
            "Source folder for RenderableTimeVaryingFitsSphere contains no files"
        );
    }

    computeSequenceEndTime();
}

void RenderableTimeVaryingFitsSphere::update(const UpdateData& data) {
    RenderableSphere::update(data);

    const double currentTime = data.time.j2000Seconds();
    const double deltaTime = global::timeManager->deltaTime();

    if (!_dynamicFileDownloader && _loadingType == LoadingType::DynamicDownloading) {
        const std::string& identifier = parent()->identifier();
        _dynamicFileDownloader = std::make_unique<DynamicFileSequenceDownloader>(
            _dataID,
            identifier,
            _infoUrl,
            _dataUrl,
            _nFilesToQueue
        );
    }

    if (_loadingType == LoadingType::DynamicDownloading) {
        updateDynamicDownloading(currentTime, deltaTime);
    }

    _inInterval = !_files.empty() &&
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
            updateActiveTriggerTimeIndex(currentTime);
            File& file = _files[_activeTriggerTimeIndex];
            if (file.status == File::FileStatus::Downloaded) {
                std::pair<float, float> minMax = _layerMinMaxCaps.at(_fitsLayerName);
                file.texture =
                    loadTextureFromFits(file.path, _fitsLayerName, minMax);
                using FilterMode = ghoul::opengl::Texture::FilterMode;
                if (_textureFilterProperty == static_cast<int>(FilterMode::Nearest)) {
                    file.texture->setFilter(FilterMode::Nearest);
                }
                else if (_textureFilterProperty == static_cast<int>(FilterMode::Linear)) {
                    file.texture->setFilter(FilterMode::Linear);
                }
                file.status = File::FileStatus::Loaded;
                trackOldest(file);
                loadTexture();
            }
        }
        // The case when we jumped passed last file where nextIdx is not < file.size()
        else if (currentTime >= _files[_activeTriggerTimeIndex].time && !_texture) {
            loadTexture();
        }
    }

    if (!_firstUpdate && _useColorMap && !_files.empty()) {
        _dataMinMaxValues = _files[_activeTriggerTimeIndex].dataMinMax;
    }

    if (_textureIsDirty) [[unlikely]] {
        loadTexture();
        _textureIsDirty = false;
    }
}

void RenderableTimeVaryingFitsSphere::render(const RenderData& data, RendererTasks& task)
{
    if (_files.empty() || (!_inInterval && !_renderForever)) {
        return;
    }
    RenderableSphere::render(data, task);
}

void RenderableTimeVaryingFitsSphere::bindTexture() {
    if (_texture) {
        _texture->bind();
    }
}

void RenderableTimeVaryingFitsSphere::updateActiveTriggerTimeIndex(double currentTime) {
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

void RenderableTimeVaryingFitsSphere::updateDynamicDownloading(double currentTime,
                                                               double deltaTime)
{
    _dynamicFileDownloader->update(currentTime, deltaTime);
    const std::vector<std::filesystem::path>& filesToRead =
        _dynamicFileDownloader->downloadedFiles();
    for (const std::filesystem::path& filePath : filesToRead) {
        if (filePath.extension() == ".fits") {
            readFileFromFits(filePath);
        }
    }
    if (!filesToRead.empty()) {
        computeSequenceEndTime();
        updateActiveTriggerTimeIndex(currentTime);
    }
    if (_firstUpdate) {
        const bool isInInterval = !_files.empty() &&
            currentTime >= _files[0].time &&
            currentTime < _sequenceEndTime;

        if (isInInterval &&
            _activeTriggerTimeIndex != -1 &&
            _activeTriggerTimeIndex < static_cast<int>(_files.size()))
        {
            _firstUpdate = false;
            loadTexture();
        }
    }
    // If all files are moved into _sourceFiles then we can
    // empty the DynamicFileSequenceDownloader's downloaded files list
    _dynamicFileDownloader->clearDownloaded();
}

void RenderableTimeVaryingFitsSphere::computeSequenceEndTime() {
    if (_files.empty()) {
        _sequenceEndTime = 0.f;
    }
    else if (_files.size() == 1) {
        _sequenceEndTime = _files[0].time + 7200.f;
        if (_loadingType == LoadingType::StaticLoading && !_renderForever) {
            // TODO (2025-06-10, Elon) Alternativly check at construction and throw
            // exeption.
            LWARNING(
                "Only one file in data set, but ShowAtAllTimes set to false. "
                "Using arbitrary 2 hours to visualize data file instead"
            );
        }
    }
    else if (_files.size() > 1) {
        const double lastTriggerTime = _files[_files.size() - 1].time;
        const double sequenceDuration = lastTriggerTime - _files[0].time;
        const double averageCadence =
            sequenceDuration / (static_cast<double>(_files.size()) - 1.0);
        // A multiplier of 3 to the average cadence is added at the end as a buffer
        // 3 because if you start it just before new data came in, you might just be
        // outside the sequence end time otherwise
        _sequenceEndTime = lastTriggerTime + 3 * averageCadence;
    }
}

void RenderableTimeVaryingFitsSphere::loadTexture() {
    if (_activeTriggerTimeIndex != -1 &&
        static_cast<size_t>(_activeTriggerTimeIndex) < _files.size())
    {
        _texture = _files[_activeTriggerTimeIndex].texture.get();
        showCorrectFileName();
    }
}

void RenderableTimeVaryingFitsSphere::trackOldest(File& file) {
    if (file.status == File::FileStatus::Loaded) {
        std::deque<File*>::iterator it =
            std::find(_loadedFiles.begin(), _loadedFiles.end(), &file);
        if (it == _loadedFiles.end()) {
            _loadedFiles.push_back(&file);
        }
        // Repopulate the queue if new File makes the queue full
        if (!_loadedFiles.empty() &&
            _loadingType == LoadingType::DynamicDownloading &&
            _loadedFiles.size() >= _maxLoadedFiles)
        {
            File* oldest = _loadedFiles.front();
            // The edge case of when queue just got full and user jumped back to where
            // they started which would make the oldes file in queue to be the active
            // file. In that case we need to make sure we do not unload it
            if (oldest == &file) {
                return;
            }
            oldest->status = File::FileStatus::Downloaded;
            oldest->texture = nullptr;
            _loadedFiles.pop_front();
        }
    }
}

void RenderableTimeVaryingFitsSphere::showCorrectFileName() {
    _textureSource = _files[_activeTriggerTimeIndex].path.filename().string();
}

} // namespace openspace
