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
#include <openspace/util/sphere.h>
#include <openspace/util/timemanager.h>
#include <openspace/util/updatestructures.h>
#include <openspace/properties/property.h>
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
        std::string fileName= filePath.stem().string();

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
        "This value specifies a directory or file names fomr where files loaded from "
        "disk and are used as a texture that is applied to this sphere. Images are "
        "expected to be an equirectangular projection.",
        openspace::properties::Property::Visibility::User
    };
    constexpr openspace::properties::Property::PropertyInfo FitsLayerInfo = {
        "FitsLayer",
        "Fits Layer",
        "This value specifies which index in the fits file to extract and use as texture",
        openspace::properties::Property::Visibility::User
    };
    constexpr openspace::properties::Property::PropertyInfo TextureFilterInfo = {
        "TextureFilter",
        "Texture Filter",
        "Option to choose nearest neighobr or linear filter to texture.",
        openspace::properties::Property::Visibility::User
    };
    constexpr openspace::properties::Property::PropertyInfo DeleteDownloadsOnShutdown = {
        "DeleteDownloadsOnShutdown",
        "Delete Downloads On Shutdown",
        "This is an option for if dynamically downloaded should be saved between runs "
        "or not. Deletes on default",
        openspace::properties::Property::Visibility::Developer
    };

    struct [[codegen::Dictionary(RenderableTimeVaryingSphere)]] Parameters {
        // [[codegen::verbatim(TextureSourceInfo.description)]]
        std::optional<std::string> textureSource;
        // choose type of loading:
        //0: static loading and static downloading
        //1: dynamic loading and static downloading
        //2: dynamic loading and dynamic downloading
        enum class [[codegen::map(openspace::RenderableTimeVaryingSphere::LoadingType)]] LoadingType {
            StaticLoading,
            DynamicLoading,
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
        // This is set to true by default and will delete all the downloaded content when
        // OpenSpace is shut down. Set to false to save all the downloaded fils.
        std::optional<bool> deleteDownloadsOnShutdown;
        // Set if first/last file should render forever
        std::optional<bool> showAtAllTimes;
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
    , _fitsLayer(FitsLayerInfo, properties::OptionProperty::DisplayType::Dropdown)
    , _deleteDownloadsOnShutdown(DeleteDownloadsOnShutdown, true)
    , _textureFilterProperty(TextureFilterInfo, properties::OptionProperty::DisplayType::Dropdown)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _textureSourcePath = p.textureSource.value_or(_textureSourcePath);
    if (p.loadingType.has_value()) {
        _loadingType = codegen::map<LoadingType>(*p.loadingType);
    }
    else {
        _loadingType = LoadingType::StaticLoading;
    }
    if (_loadingType == LoadingType::DynamicDownloading) {
        setupDynamicDownloading(p.dataID, p.numberOfFilesToQueue, p.infoURL, p.dataURL);
    }
    if (p.fitsLayer.has_value()) {
        _fitsLayerTemp = *p.fitsLayer;
    }
    _deleteDownloadsOnShutdown =
        p.deleteDownloadsOnShutdown.value_or(_deleteDownloadsOnShutdown);

    _textureFilterProperty.addOptions({
        {static_cast<int>(TextureFilter::NearestNeighbor), "No Filter"},
        {static_cast<int>(TextureFilter::Linear), "Linear smoothing"}
    });
    if (p.textureFilter.has_value()) {
        _textureFilter = codegen::map<TextureFilter>(*p.textureFilter);
    }
    else {
        _textureFilter = TextureFilter::Unspecified;
    }
    _renderForever = p.showAtAllTimes.value_or(_renderForever);

    if (_loadingType == LoadingType::StaticLoading) {
        extractMandatoryInfoFromSourceFolder();
        computeSequenceEndTime();
        loadTexture();
    }
    addProperty(_textureSourcePath);
    addProperty(_textureFilterProperty);
    addProperty(_fitsLayer);
    definePropertyCallbackFunctions();
}

bool RenderableTimeVaryingSphere::isReady() const {
    return RenderableSphere::isReady();
}

void RenderableTimeVaryingSphere::initializeGL() {
    RenderableSphere::initializeGL();
}

void RenderableTimeVaryingSphere::setupDynamicDownloading(
                                           const std::optional<int>& dataID,
                                           const std::optional<int>& numberOfFilesToQueue,
                                           const std::optional<std::string>& infoURL,
                                           const std::optional<std::string>& dataURL)
{
    _dataID = dataID.value_or(_dataID);
    if (!_dataID) {
        throw ghoul::RuntimeError(
            "If running with dynamic downloading, dataID needs to be specified"
        );
    }
    _nOfFilesToQueue = numberOfFilesToQueue.value_or(_nOfFilesToQueue);
    _infoURL = infoURL.value();
    if (_infoURL.empty()) { throw ghoul::RuntimeError("InfoURL has to be provided"); }
    _dataURL = dataURL.value();
    if (_dataURL.empty()) { throw ghoul::RuntimeError("DataURL has to be provided"); }
    _dynamicFileDownloader = std::make_unique<DynamicFileSequenceDownloader>(
        _dataID, _infoURL, _dataURL, _nOfFilesToQueue
    );
}

void RenderableTimeVaryingSphere::deinitializeGL() {
    _texture = nullptr;
    _files.clear();

    // Stall main thread until thread that's loading states is done
    bool printedWarning = false;
    while (_dynamicFileDownloader != nullptr &&
           _dynamicFileDownloader->filesCurrentlyDownloading())
    {
        if (!printedWarning) {
            LWARNING("Currently downloading file, exiting might take longer than usual");
            printedWarning = true;
        }
        _dynamicFileDownloader->checkForFinishedDownloads();
        std::this_thread::sleep_for(std::chrono::milliseconds(5));
    }
    if (_dynamicFileDownloader != nullptr &&
        _deleteDownloadsOnShutdown &&
        _loadingType == LoadingType::DynamicDownloading)
    {
        std::filesystem::path syncDir = _dynamicFileDownloader->destinationDirectory();
        if (!std::filesystem::exists(syncDir)) {
            return;
        }
        for (auto& file : std::filesystem::directory_iterator(syncDir)) {
            std::filesystem::remove(file);
        }
    }
    RenderableSphere::deinitializeGL();
}

void RenderableTimeVaryingSphere::readFileFromFits(std::filesystem::path path) {
    if (!_layerOptionsAdded) {
        for (int i = 0; i < nLayers(path); ++i) {
            _fitsLayer.addOption(i, std::to_string(i+1));
        }
        _fitsLayer = _fitsLayerTemp;
        _layerOptionsAdded = true;
    }

    File newFile;
    newFile.path = path;
    newFile.status = File::FileStatus::Loaded;
    //newFile.time = extractTriggerTimeFromISO8601FileName(path);
    newFile.time = extractTriggerTimeFromFitsFileName(path);
    std::unique_ptr<ghoul::opengl::Texture> t = loadTextureFromFits(path, _fitsLayer);
    if (t == nullptr) {
        return;
    }

    if (_textureFilter == TextureFilter::NearestNeighbor ||
        _textureFilter == TextureFilter::Unspecified)
    {
        t->setFilter(ghoul::opengl::Texture::FilterMode::Nearest);
    }
    else if(_textureFilter == TextureFilter::Linear) {
        t->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
    }

    setMinMaxValues(t, newFile);
    newFile.texture = std::move(t);

    const std::vector<File>::const_iterator iter = std::upper_bound(
        _files.begin(), _files.end(),
        newFile.time,
        [](double timeRef, const File& fileRef) {
            return timeRef < fileRef.time;
        }
    );
    _files.insert(iter, std::move(newFile));
    computeSequenceEndTime();
}

void RenderableTimeVaryingSphere::readFileFromImage(std::filesystem::path path) {
    File newFile;
    newFile.path = path;
    newFile.status = File::FileStatus::Loaded;
    newFile.time = extractTriggerTimeFromISO8601FileName(path);
    std::unique_ptr<ghoul::opengl::Texture> t =
        ghoul::io::TextureReader::ref().loadTexture(path.string(), 2);

    t->setInternalFormat(GL_COMPRESSED_RGBA);
    t->uploadTexture();
    if (_textureFilter == TextureFilter::Linear ||
        _textureFilter == TextureFilter::Unspecified)
    {
        t->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
    }
    else if (_textureFilter == TextureFilter::NearestNeighbor) {
        t->setFilter(ghoul::opengl::Texture::FilterMode::Nearest);
    }
    t->purgeFromRAM();

    if (_isUsingColorMap) {
        setMinMaxValues(t, newFile);
    }
    newFile.texture = std::move(t);

    const std::vector<File>::const_iterator iter = std::upper_bound(
        _files.begin(), _files.end(),
        newFile.time,
        [](double timeRef, const File& fileRef) {
            return timeRef < fileRef.time;
        }
    );
    _files.insert(iter, std::move(newFile));
    computeSequenceEndTime();
}

void RenderableTimeVaryingSphere::setMinMaxValues(
                                               std::unique_ptr<ghoul::opengl::Texture>& t,
                                                                               File& file)
{
    const void* rawData = t->pixelData();
    const float* pixelData = static_cast<const float*>(rawData);
    size_t dataSize = t->dimensions().x * t->dimensions().y;
    float min = *std::min_element(pixelData, pixelData + dataSize);
    float max = *std::max_element(pixelData, pixelData + dataSize);
    file.dataMinMax = glm::vec2(min, max);
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
            readFileFromFits(e.path());
            _isFitsFormat = true;
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

    if (!_inInterval && _renderForever) {
        updateActiveTriggerTimeIndex(currentTime);
    }

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
            if (previousIndex != _activeTriggerTimeIndex) {
                loadTexture();
                showCorrectFileName();
            }
        } // else {we're still in same state as previous frame (no changes needed)}
    }
    else {
        // not in interval => set everything to false
        _activeTriggerTimeIndex = 0;
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
            readFileFromFits(filePath);
            _isFitsFormat = true;
        }
        else {
            readFileFromImage(filePath);
        }
    }
    if (_firstUpdate) {
        const bool isInInterval = _files.size() > 0 &&
            currentTime >= _files[0].time &&
            currentTime < _sequenceEndTime;
        if (isInInterval && _activeTriggerTimeIndex == 0) {
            _firstUpdate = false;
            loadTexture();
            showCorrectFileName();
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
        const double averageStateDuration =
            sequenceDuration / (static_cast<double>(_files.size()) - 1.0);
        _sequenceEndTime = lastTriggerTime + averageStateDuration;
    }
}

void RenderableTimeVaryingSphere::loadTexture() {
    if (_activeTriggerTimeIndex != -1) {
        _texture = _files[_activeTriggerTimeIndex].texture.get();
    }
}

void RenderableTimeVaryingSphere::showCorrectFileName() {
    _textureSourcePath = _files[_activeTriggerTimeIndex].path.filename().string();
}

void RenderableTimeVaryingSphere::definePropertyCallbackFunctions() {
    _fitsLayer.onChange([this]() {
        if (_loadingType == LoadingType::StaticLoading) {
            extractMandatoryInfoFromSourceFolder();
        }
        else {
            if (_isFitsFormat) {
                for (auto file = _files.begin(); file != _files.end(); ++file) {
                    file->texture = loadTextureFromFits(file->path, _fitsLayer);
                    file->texture->uploadTexture();
                    file->texture->setFilter(ghoul::opengl::Texture::FilterMode::Nearest);
                }
                loadTexture();
            }
        }
    });

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
}

} // namespace openspace
