/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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
#include <cctype>
#include <modules/base/basemodule.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/io/texture/texturereader.h>
#include <modules/fitsfilereader/include/fitsfilereader.h>


// Why will it not compile without this library?
#include <openspace/util/sphere.h>

namespace {
    constexpr const char* _loggerCat = "RenderableTimeVaryingSphere";
    
    enum class MapType : int {
        Gong = 0,
        AdaptGong
        };
        
    MapType stringToMapType(const std::string& s) {
        if (s == "gong") {
            return MapType::Gong;
        } else if (s == "adaptgong" || s == "adapt") {
            return MapType::AdaptGong;
        } else if (s == "adapt") {
            return MapType::AdaptGong;
        }
        LERROR("MapType invalid. Using GONG.");
        return MapType::AdaptGong;
    }
    // --------------------- PROPERTIES FROM RENDERABLESPHERE ------------------------- //

    constexpr openspace::properties::Property::PropertyInfo MirrorTextureInfo = {
        "MirrorTexture",
        "Mirror Texture",
        "Mirror the texture along the x-axis."
    };

    constexpr openspace::properties::Property::PropertyInfo OrientationInfo = {
        "Orientation",
        "Orientation",
        "Specifies whether the texture is applied to the inside of the sphere, the "
        "outside of the sphere, or both."
    };

    constexpr openspace::properties::Property::PropertyInfo UseAdditiveBlendingInfo = {
        "UseAdditiveBlending",
        "Use Additive Blending",
        "Render the object using additive blending."
    };

    constexpr openspace::properties::Property::PropertyInfo SegmentsInfo = {
        "Segments",
        "Number of Segments",
        "This value specifies the number of segments that the sphere is separated in."
    };

    constexpr openspace::properties::Property::PropertyInfo SizeInfo = {
        "Size",
        "Size (in meters)",
        "This value specifies the radius of the sphere in meters."
    };

    constexpr openspace::properties::Property::PropertyInfo FadeOutThresholdInfo = {
        "FadeOutThreshold",
        "Fade-Out Threshold",
        "This value determines percentage of the sphere is visible before starting "
        "fading-out it."
    };

    constexpr openspace::properties::Property::PropertyInfo FadeInThresholdInfo = {
        "FadeInThreshold",
        "Fade-In Threshold",
        "Distance from center of MilkyWay from where the astronomical object starts to "
        "fade in."
    };

    constexpr openspace::properties::Property::PropertyInfo DisableFadeInOutInfo = {
        "DisableFadeInOut",
        "Disable Fade-In/Fade-Out effects",
        "Enables/Disables the Fade-In/Out effects."
    };

    constexpr openspace::properties::Property::PropertyInfo BackgroundInfo = {
        "Background",
        "Sets the current sphere rendering as a background rendering type",
        "Enables/Disables background rendering."
    };
    
    // --------------------- PROPERTIES TIMEVARYINGSPHERE ----------------------------- //
    constexpr openspace::properties::Property::PropertyInfo DefaultTextureInfo = {
        "DefaultTexture",
        "Default Texture",
        "This value specifies an image that is loaded from disk and is used as a default "
        "texture that is applied to this sphere, when online textures has not yet loaded,"
        " or data is missing. This image is expected to be an equirectangular projection."
    };
    
    constexpr openspace::properties::Property::PropertyInfo SourceFolderInfo = {
        "SourceFolder",
        "Source Folder",
        "Specify directory for texture sequence files, if loading from disk/locally."
    };
    
    constexpr openspace::properties::Property::PropertyInfo WebContentInfo = {
        "WebContent",
        "Web Content",
        "Set true if conent is online. The field SourceFolder will be disregarded if set "
        "to true."
    };
    
    constexpr openspace::properties::Property::PropertyInfo WebContentUrlInfo = {
        "WebContentUrl",
        "Web Content Url",
        "Endpoint for web content. Property WebContent needs to be set to true."
    };
        
    constexpr openspace::properties::Property::PropertyInfo MapTypeInfo = {
        "MapType",
        "Type of Photospheric Map",
        "GONG or ADAPT GONG. Entered as gong or adaptgong respectively. This will "
        "determine how the values on the map will translate to color intensity. Defaults "
        " to GONG."
    };
    
} // namespace

namespace openspace {

documentation::Documentation RenderableTimeVaryingSphere::Documentation() {
    using namespace documentation;
    return {
        "RenderableTimeVaryingSphere",
        "base_renderable_timevarying_sphere",
        {
            {
                SizeInfo.identifier,
                new DoubleVerifier,
                Optional::No,
                SizeInfo.description
            },
            {
                SegmentsInfo.identifier,
                new IntVerifier,
                Optional::No,
                SegmentsInfo.description
            },
            {
                OrientationInfo.identifier,
                new StringInListVerifier({ "Inside", "Outside", "Both" }),
                Optional::Yes,
                OrientationInfo.description
            },
            {
                UseAdditiveBlendingInfo.identifier,
                new BoolVerifier,
                Optional::Yes,
                UseAdditiveBlendingInfo.description
            },
            {
                MirrorTextureInfo.identifier,
                new BoolVerifier,
                Optional::Yes,
                MirrorTextureInfo.description
            },
            {
                FadeOutThresholdInfo.identifier,
                new DoubleInRangeVerifier(0.0, 1.0),
                Optional::Yes,
                FadeOutThresholdInfo.description
            },
            {
                FadeInThresholdInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                FadeInThresholdInfo.description
            },
            {
                DisableFadeInOutInfo.identifier,
                new BoolVerifier,
                Optional::Yes,
                DisableFadeInOutInfo.description
            },
            {
                BackgroundInfo.identifier,
                new BoolVerifier,
                Optional::Yes,
                BackgroundInfo.description
            },
            {
                DefaultTextureInfo.identifier,
                new StringVerifier,
                Optional::No,
                DefaultTextureInfo.description
            },
            {
                SourceFolderInfo.identifier,
                new StringVerifier,
                Optional::Yes,
                SourceFolderInfo.description
            },
            {
                WebContentInfo.identifier,
                new BoolVerifier,
                Optional::Yes,
                WebContentInfo.description
            },
            {
                WebContentUrlInfo.identifier,
                new StringVerifier,
                Optional::Yes,
                WebContentUrlInfo.description
            },
            {
                MapTypeInfo.identifier,
                new StringVerifier,
                Optional::Yes,
                MapTypeInfo.description
            },
        }
    };
}

RenderableTimeVaryingSphere::RenderableTimeVaryingSphere
    (const ghoul::Dictionary& dictionary)
    // Does this have any consequences?
    // Will there be an additional renderable in the scene?
    : Renderable(dictionary),
    _defaultTexturePath(DefaultTextureInfo)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "RenderableTimeVaryingSphere"
    );
    _dictionary = std::make_unique<ghoul::Dictionary>(dictionary);
    _defaultTexturePath = absPath(_dictionary->value<std::string>(
                                                        DefaultTextureInfo.identifier));
    createRenderableSphere();
}

bool RenderableTimeVaryingSphere::isReady() const {
    return  _renderableSphere->isReady();
}

void RenderableTimeVaryingSphere::initializeGL() {
    _renderableSphere->initializeGL();
    
    extractMandatoryInfoFromDictionary();
    extractOptionalInfoFromDictionary();
    
    prepareForTimeVaryingTexture();
    computeSequenceEndTime();
    
    _webFieldlinesManager.initializeSyncDirectory(_identifier);
    initializeWebManager();
}

void RenderableTimeVaryingSphere::deinitializeGL() {
    // Stall main thread until thread that's loading texture is done!
    bool printedWarning = false;
    while (_isLoadingTextureFromDisk) {
        if (!printedWarning) {
            LWARNING("Trying to destroy class when an active thread is still using it");
            printedWarning = true;
        }
        std::this_thread::sleep_for(std::chrono::milliseconds(5));
    }
    
    _renderableSphere->deinitializeGL();
}

void RenderableTimeVaryingSphere::render(
    const RenderData& data, RendererTasks& rendererTasks) {
    _renderableSphere->render(data, rendererTasks);
}

void RenderableTimeVaryingSphere::update(const UpdateData& data) {
    const double currentTime = data.time.j2000Seconds();
    
    if (_webContent) {
        if (!_webFieldlinesManager.hasUpdated &&
            _webFieldlinesManager.checkIfWindowIsReadyToLoad())
        {
            _triggerTimes.clear();
            extractTriggerTimesFromFileNames();
            
            // _startTimes are not sorted right now, have to do something about that ->
            std::sort(_triggerTimes.begin(), _triggerTimes.end());
            _nTextures = _triggerTimes.size();
            
            _webFieldlinesManager.hasUpdated = true;
            _webFieldlinesManager.notifyUpdate = true;
            _webFieldlinesManager.resetWorker();
        }
        _webFieldlinesManager.update();
        // we could also send time as a variable as we already have it
    }
    
    //Check if current time is within the sequence interval
    const bool isInInterval = (_nTextures > 0) &&
                              (currentTime >= _triggerTimes[0]) &&
                              (currentTime < _sequenceEndTime);
    
    if(isInInterval) {
        const size_t stepForwardIndex = _activeTriggerTimeIndex + 1;
        _hasExitedInterval = false;
        
        if (
            // true => Previous frame was not within the sequence interval
            _activeTriggerTimeIndex < 0 ||
            // true => We stepped back to a time represented by another texture
            currentTime < _triggerTimes[_activeTriggerTimeIndex] ||
            // true => We stepped forward to a time represented by another texture
            (stepForwardIndex < _nTextures &&
             currentTime >= _triggerTimes[stepForwardIndex])
            ){
            updateActiveTriggerTimeIndex(currentTime);
            _mustLoadNewTextureFromDisk = true;
        }// else {we're still in same state as previous frame (no changes needed)}
    }
    else {
        if (!_hasExitedInterval){
            // Exiting interval => set everything to false
            _activeTriggerTimeIndex = -1;
            _mustLoadNewTextureFromDisk = false;
            if(!_webContent){
                loadDefaultTexture();
                _renderableSphere->updateTexture(_activeTexture);
            }
            _hasExitedInterval = true;
        }
    }
    
    if (_webContent && _webFieldlinesManager.notifyUpdate) {
        updateActiveTriggerTimeIndex(currentTime);
        computeSequenceEndTime();
        _webFieldlinesManager.notifyUpdate = false;
    }
    
    if (_mustLoadNewTextureFromDisk) {
        if (!_isLoadingTextureFromDisk && !_newTextureIsReady) {
            _isLoadingTextureFromDisk = true;
            _mustLoadNewTextureFromDisk = false;
            std::string filePath = _sourceFiles[_activeTriggerTimeIndex];
            //Can't use threads, opengl...
//            std::thread readBinaryThread([this, f = std::move(filePath)]{
//                readNewTexture(f);
//            });
//            readBinaryThread.detach();
            readNewTexture(filePath);
        }
    }
    
    if (_newTextureIsReady) {
        _activeTexture = std::make_unique<ghoul::opengl::Texture>(
                                                                std::move(*_newTexture));
        // Everything is set and ready for sending to RenderableSphere
        _newTextureIsReady = false;
        _renderableSphere->updateTexture(_activeTexture);
    }

    _renderableSphere->update(data);
}
    
void RenderableTimeVaryingSphere::createRenderableSphere(){
    ghoul::Dictionary renderableSphereDictionary(*_dictionary);
    renderableSphereDictionary.setValue<std::string>("Texture",
                                                     _defaultTexturePath.value());
    _renderableSphere = std::make_unique<RenderableSphere>(renderableSphereDictionary);
}
    
void RenderableTimeVaryingSphere::initializeWebManager()
{
    _webFieldlinesManager.initializeWebFieldlinesManager(
        _identifier, _webContentUrl, _sourceFiles, _triggerTimes
    );
}

bool RenderableTimeVaryingSphere::extractMandatoryInfoFromDictionary() {
    _dictionary->getValue("Identifier", _identifier);
    
    std::string sourceFolderPath;
    
    // If web content is set to true
    if(_dictionary->getValue(WebContentInfo.identifier, _webContent)){
        _dictionary->getValue(WebContentUrlInfo.identifier, _webContentUrl);
        if (_dictionary->getValue(SourceFolderInfo.identifier, sourceFolderPath)) {
            LERROR(fmt::format(
                "{}: The field {} is provided while {} is set to true, and"
                " will be disregarded",
                _identifier, SourceFolderInfo.identifier,
                WebContentInfo.identifier
            ));
        }
        if (!_webContentUrl.empty()) {
            LINFO("Initializing sync-directory for web content.");
            sourceFolderPath = _webFieldlinesManager.initializeSyncDirectory(_identifier);
        }
        else {
            LERROR(fmt::format(
                "{}: The field {} is missing a url", _identifier,
                WebContentInfo.identifier
            ));
            return false;
        }
    }
    else{ // Else load local files from SourceFolder
        if (!_dictionary->getValue(SourceFolderInfo.identifier, sourceFolderPath)) {
            LERROR(fmt::format(
           "{}: The field {} is missing to load textures from disk. "
           "Provide it, or set {} to true to load content from web.",
           _identifier, SourceFolderInfo.identifier,
           WebContentInfo.identifier
           ));
            return false;
        }
        std::string fileExtension = "fits";
        
        // Ensure that the source folder exists and then extract
        // the files with the same extension as <fileExtension>
        ghoul::filesystem::Directory sourceFolder(sourceFolderPath);
        if (FileSys.directoryExists(sourceFolder)) {
            // Extract all file paths from the provided folder
            _sourceFiles = sourceFolder.readFiles(
                ghoul::filesystem::Directory::Recursive::No,
                ghoul::filesystem::Directory::Sort::Yes
            );
            
            // Remove all files that don't have fits as extension
            removeOtherFiles(fileExtension);
            
            // Ensure that there are available and valid source files left
            if (_sourceFiles.empty()) {
                LERROR(fmt::format(
                    "{}: {} contains no {} files",
                    _identifier, sourceFolderPath, fileExtension
                ));
                return false;
            }
        }
        else {
            LERROR(fmt::format(
                "{}: {} is not a valid directory for source files",
                _identifier, sourceFolderPath
            ));
            return false;
        }
    }
    
    return true;
}
    
void RenderableTimeVaryingSphere::extractOptionalInfoFromDictionary() {
    std::string s;
    if(!_dictionary->getValue(MapTypeInfo.identifier, s)){
        LERROR("No photospheric map type specified. Using default.");
    }
    else {
        std::transform(s.begin(), s.end(), s.begin(),
                       [](unsigned char c){ return std::tolower(c); }
                       );
        _mapType = stringToMapType(s);
    }
}
    
bool RenderableTimeVaryingSphere::prepareForTimeVaryingTexture() {
    extractTriggerTimesFromFileNames();
    _nTextures = _triggerTimes.size();
    return true;
}

void RenderableTimeVaryingSphere::extractTriggerTimesFromFileNames() {
    for (const std::string& filePath : _sourceFiles) {
        ghoul::filesystem::File file(filePath);
        // Extract the filename from the path (without extension)
        std::string fileName = file.baseName();
        
        std::string timeString;
        int magicalCounter = 0;
        for (char c : fileName) {
            if (std::isdigit(c))
            {
                if (magicalCounter >= 0 && magicalCounter < 12)
                {
                    timeString += c;
                }
                magicalCounter++;
            }
        }
        
        // Ensure the separators are correct
        timeString.insert(10, ":");
        timeString.insert(8, "T");
        timeString.insert(6, "-");
        timeString.insert(4, "-");
        const double triggerTime = Time::convertTime(timeString);
        _triggerTimes.push_back(triggerTime);
    }
}

//Expected endtime
void RenderableTimeVaryingSphere::computeSequenceEndTime() {
    if (_nTextures > 1) {
        const double lastTriggerTime = _triggerTimes[_nTextures - 1];
        const double sequenceDuration = lastTriggerTime - _triggerTimes[0];
        const double averageTextureDuration = sequenceDuration /
        (static_cast<double>(_nTextures) - 1.0);
        _sequenceEndTime = lastTriggerTime + averageTextureDuration;
    }
    else {
        // If there's just one texture it should never disappear!
        _sequenceEndTime = DBL_MAX;
    }
}

void RenderableTimeVaryingSphere::removeOtherFiles(std::string fileExtension) {
    _sourceFiles.erase(
        std::remove_if(
            _sourceFiles.begin(),
            _sourceFiles.end(),
            [fileExtension](const std::string& str) {
                const size_t extLength = fileExtension.length();
                std::string sub = str.substr(str.length() - extLength, extLength);
                std::transform(
                    sub.begin(),
                    sub.end(),
                    sub.begin(),
                    [](char c) { return static_cast<char>(::tolower(c)); }
                );
                return sub != fileExtension;
            }),
        _sourceFiles.end()
    );
}
    
// Assumes we already know that currentTime is within the sequence interval
void RenderableTimeVaryingSphere::updateActiveTriggerTimeIndex(double currentTime) {
    auto iter = std::upper_bound(_triggerTimes.begin(), _triggerTimes.end(), currentTime);
    if (iter != _triggerTimes.end()) {
        if (iter != _triggerTimes.begin()) {
            _activeTriggerTimeIndex = static_cast<int>(
                                                    std::distance(_triggerTimes.begin(),
                                                    iter) ) - 1;
        }
        else {
            _activeTriggerTimeIndex = 0;
        }
    }
    else {
        _activeTriggerTimeIndex = static_cast<int>(_nTextures) - 1;
    }
}
    
// TODO: Reading texture from disk. Must be thread safe! It's not...
void RenderableTimeVaryingSphere::readNewTexture(const std::string& filePath) {
    std::vector<float> imagedata;
    processWSAFitsFile(filePath, &imagedata);
    if (loadTextureData(&imagedata)) {
        _newTextureIsReady = true;
    }
    _isLoadingTextureFromDisk = false;
}
    
void RenderableTimeVaryingSphere::processWSAFitsFile(std::string filePath,
                                                  std::vector<float> *imagedata){
    FitsFileReader fitsFileReader(false);
    
    // Since WSA has more HDUs than the primary one, we want to force use of the primary
    // HDU, so that fitsfilereader won't default to the extension HDUs.
    fitsFileReader.forceUsePHDU();
    
    const auto fitsValues = fitsFileReader.readImageFloat(filePath);
    
    // Longitude leading edge of map header value
    const int long0 = *fitsFileReader.readHeaderValueFloat("CARRLONG");
    // Observatory header value
    //const std::string wsaMapType = *fitsFileReader.readHeaderValueString("OBSER");
    
    std::vector<std::vector<float>> rgbLayers;
    float r, g, b;
    
    // the numbers 64800, 16200 means, grab the fifth layer in the fits file, where the
    // photospheric map is, in the wsa file
    std::valarray<float> magnetogram = fitsValues->contents[std::slice(64800, 16200, 1)];
    float maxvalue = abs(magnetogram).max();
    
    //these values should be adjustable in the gui in the future,
    // since they have varying results depending on solar activity
    float damper = 1.0f;
    float multiplyer = 20.0f;
    
    for (float mapvalue : magnetogram) {
        float colorIntensity = abs(mapvalue) / maxvalue; // normalized value
        
        // would be neat to be able choose color scheme interactively
        //r = 1.0f, g = 1.0f, b = 1.0f; // white
        //r = 0.0f, g = 0.0f, b = 0.0f; // black
        r = 0.5f, g = 0.5f, b = 0.5f; // gray
        if(mapvalue != 0.0f){
            if(_mapType == MapType::Gong){
                // The values are be amplified to be visible for this type
                // Commenting out for now, since it has had varying results
                //colorIntensity = damper*log( 1 + multiplyer * colorIntensity );
            }
            // If negative
            if(mapvalue < 0) {
                //r = colorIntensity; //black
                //g = 1.0 - colorIntensity; // white
                //b = 1.0 - colorIntensity; // white
                r = colorIntensity*0.5 + 0.5; // gray
                g = colorIntensity*0.5 + 0.5; // gray
                b = colorIntensity*0.5 + 0.5; // gray
            }
            else{ // else if positive
                //b = colorIntensity; // black
                //r = 1.0 - colorIntensity; // white
                //g = 1.0 - colorIntensity; // white
                r = 1.0 - (colorIntensity*0.5 + 0.5); // gray
                g = 1.0 - (colorIntensity*0.5 + 0.5); // gray
                b = 1.0 - (colorIntensity*0.5 + 0.5); // gray
            }
        }
        std::vector<float> rgb = {r,g,b};
        rgbLayers.push_back(rgb);
    }
    
    // shift with leading edge value and divide by two to match resolution (180)
    int shift = (360 - long0) / 2;
    
    for(int i = 0; i < 90; i++) {
        std::rotate(rgbLayers.begin() + (i * 180),
                    rgbLayers.begin() + (i * 180) + shift,
                    rgbLayers.begin() + (i * 180) + 179 );
        for(int j = 0; j < 180; j++){
            int index = i * 180 + j;
            imagedata->push_back(rgbLayers[index][0]);
            imagedata->push_back(rgbLayers[index][1]);
            imagedata->push_back(rgbLayers[index][2]);
        }
    }
}
    
bool RenderableTimeVaryingSphere::loadTextureData(std::vector<float> *imagedata){
    auto texture = std::make_unique<ghoul::opengl::Texture>(
                                                    std::move(imagedata->data()),
                                                    glm::vec3(180, 90, 1),
                                                    ghoul::opengl::Texture::Format::RGB,
                                                    GL_RGB32F,
                                                    GL_FLOAT
                                                    );
    texture->setDataOwnership(ghoul::opengl::Texture::TakeOwnership::No);
    texture->uploadTexture();
    _newTexture = std::move(texture);
    return true;
}
    
void RenderableTimeVaryingSphere::loadDefaultTexture() {
    // TODO: Add functionality to be able to use a fits file as default texture
    if (!_defaultTexturePath.value().empty()) {
        std::unique_ptr<ghoul::opengl::Texture> texture =
        ghoul::io::TextureReader::ref().loadTexture(_defaultTexturePath);
        
        if (texture) {
            LDEBUGC(
                    _identifier,
                    fmt::format("Loaded default texture from '{}'",
                    absPath(_defaultTexturePath))
                    );
            texture->uploadTexture();
            texture->setFilter(ghoul::opengl::Texture::FilterMode::LinearMipMap);
            _activeTexture = std::move(texture);
        }
    }
}

} // namespace openspace
