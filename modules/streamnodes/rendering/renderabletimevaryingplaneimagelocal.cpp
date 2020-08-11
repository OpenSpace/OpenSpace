#include <modules/streamnodes/rendering/renderabletimevaryingplaneimagelocal.h>

#include <modules/base/basemodule.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/crc32.h>
#include <ghoul/opengl/texture.h>
#include <fstream>
#include <modules/fieldlinessequence/fieldlinessequencemodule.h>
#include <modules/fieldlinessequence/util/kameleonfieldlinehelper.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/interaction/orbitalnavigator.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/util/timemanager.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
// Test debugging tools more then logmanager
#include <ghoul/logging/consolelog.h>
#include <ghoul/logging/visualstudiooutputlog.h>
#include <ghoul/filesystem/cachemanager.h>

#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/textureunit.h>
#include <fstream>
#include <thread>
#include <openspace/json.h>
#include <openspace/query/query.h>

namespace {
    constexpr const char* KeyLazyLoading = "LazyLoading";
    constexpr const char* _loggerCat = "RenderableTimeVaryingPlaneImageLocal";

    constexpr openspace::properties::Property::PropertyInfo TextureInfo = {
        "Texture",
        "Texture",
        "This value specifies an image that is loaded from disk and is used as a texture "
        "that is applied to this plane. This image has to be square."
    };

    constexpr openspace::properties::Property::PropertyInfo RenderableTypeInfo = {
       "RenderableType",
       "RenderableType",
       "This value specifies if the plane should be rendered in the Background,"
       "Opaque, Transparent, or Overlay rendering step."
    };

} // namespace

namespace openspace {

    documentation::Documentation RenderableTimeVaryingPlaneImageLocal::Documentation() {
        using namespace documentation;
        return {
            "Renderable Plane Image Local",
            "base_renderable_plane_image_local",
            {
                {
                    TextureInfo.identifier,
                    new StringVerifier,
                    Optional::No,
                    TextureInfo.description
                },
                {
                    RenderableTypeInfo.identifier,
                    new StringVerifier,
                    Optional::Yes,
                    RenderableTypeInfo.description
                },
                {
                    KeyLazyLoading,
                    new BoolVerifier,
                    Optional::Yes,
                    "If this value is set to 'true', the image for this plane will not be "
                    "loaded at startup but rather when image is shown for the first time. "
                    "Additionally, if the plane is hidden, the image will automatically be "
                    "unloaded"
                }
            }
        };
    }

    RenderableTimeVaryingPlaneImageLocal::RenderableTimeVaryingPlaneImageLocal(const ghoul::Dictionary& dictionary)
        : RenderablePlane(dictionary)
        , _texturePath(TextureInfo)
    {
        documentation::testSpecificationAndThrow(
            Documentation(),
            dictionary,
            "RenderableTimeVaryingPlaneImageLocal"
        );

        addProperty(_blendMode);

        _texturePath = absPath(dictionary.value<std::string>(TextureInfo.identifier));
        _textureFile = std::make_unique<ghoul::filesystem::File>(_texturePath);

        addProperty(_texturePath);
        _texturePath.onChange([this]() {loadTexture(); });
        _textureFile->setCallback(
            [this](const ghoul::filesystem::File&) { _textureIsDirty = true; }
        );

        if (dictionary.hasKey(RenderableTypeInfo.identifier)) {
            std::string renderType = dictionary.value<std::string>(
                RenderableTypeInfo.identifier
                );
            if (renderType == "Background") {
                setRenderBin(Renderable::RenderBin::Background);
            }
            else if (renderType == "Opaque") {
                setRenderBin(Renderable::RenderBin::Opaque);
            }
            else if (renderType == "PreDeferredTransparent") {
                setRenderBin(Renderable::RenderBin::PreDeferredTransparent);
            }
            else if (renderType == "PostDeferredTransparent") {
                setRenderBin(Renderable::RenderBin::PostDeferredTransparent);
            }
            else if (renderType == "Overlay") {
                setRenderBin(Renderable::RenderBin::Overlay);
            }
        }
        else {
            setRenderBin(Renderable::RenderBin::Opaque);
        }

        if (dictionary.hasKey(KeyLazyLoading)) {
            _isLoadingLazily = dictionary.value<bool>(KeyLazyLoading);

            if (_isLoadingLazily) {
                _enabled.onChange([this]() {
                    if (!_enabled) {
                        BaseModule::TextureManager.release(_texture);
                        _texture = nullptr;
                    }
                    if (_enabled) {
                        _textureIsDirty = true;
                    }
                    });
            }
        }
    }

    bool RenderableTimeVaryingPlaneImageLocal::isReady() const {
        return RenderablePlane::isReady();
    }

    void RenderableTimeVaryingPlaneImageLocal::initializeGL() {
        RenderablePlane::initializeGL();
        LDEBUG("sourcefiles size:" + std::to_string(_sourceFiles.size()));
       
        if (!extractMandatoryInfoFromDictionary()) {
            return;
        }
        extractTriggerTimesFromFileNames();
        computeSequenceEndTime();

        if (!_isLoadingLazily) {
            loadTexture();
        }
    }

    bool RenderableTimeVaryingPlaneImageLocal::extractMandatoryInfoFromDictionary()
    {
        // Ensure that the source folder exists and then extract
       // the files with the same extension as <inputFileTypeString>
        ghoul::filesystem::Directory sourceFolder(_texturePath);
        if (FileSys.directoryExists(sourceFolder)) {
            // Extract all file paths from the provided folder
            _sourceFiles = sourceFolder.readFiles(
                ghoul::filesystem::Directory::Recursive::No,
                ghoul::filesystem::Directory::Sort::Yes
            );
            // Ensure that there are available and valid source files left
            if (_sourceFiles.empty()) {
                LERROR(fmt::format(
                    "{}: {} contains no {} files",
                    _identifier, _texturePath, "extension"
                ));
                return false;
            }
        }
        else {
            LERROR(fmt::format(
                "{}: FieldlinesSequence {} is not a valid directory",
                _identifier,
                _texturePath
            ));
            return false;
        }
        LDEBUG("returning true");
        return true;
    }
    void RenderableTimeVaryingPlaneImageLocal::deinitializeGL() {
        _textureFile = nullptr;

        BaseModule::TextureManager.release(_texture);
        RenderablePlane::deinitializeGL();
    }

    void RenderableTimeVaryingPlaneImageLocal::bindTexture() {
        _texture->bind();
    }

    void RenderableTimeVaryingPlaneImageLocal::update(const UpdateData& data) {
        RenderablePlane::update(data);
        if (!this->_enabled) {
            return;
        }
            const double currentTime = data.time.j2000Seconds();
            const bool isInInterval = (currentTime >= _startTimes[0]) &&
                (currentTime < _sequenceEndTime);
            //const bool isInInterval = true;
            if (isInInterval) {
                const size_t nextIdx = _activeTriggerTimeIndex + 1;
                if (
                    // true => Previous frame was not within the sequence interval
                    //_activeTriggerTimeIndex < 0 ||
                    // true => We stepped back to a time represented by another state
                    currentTime < _startTimes[_activeTriggerTimeIndex] ||
                    // true => We stepped forward to a time represented by another state
                    (nextIdx < _nStates && currentTime >= _startTimes[nextIdx]))
                {
                    updateActiveTriggerTimeIndex(currentTime);
                    //LDEBUG("Vi borde uppdatera1");

                    // _mustLoadNewStateFromDisk = true;
                    //LDEBUG("vi borde uppdatera");
                    _needsUpdate = true;

                } // else {we're still in same state as previous frame (no changes needed)}
            }
            else {
                //not in interval => set everything to false
            //LDEBUG("not in interval");
                _activeTriggerTimeIndex = 0;
                _needsUpdate = false;
            }

            if ((_needsUpdate || _textureIsDirty) && !_isLoadingTexture) {
                _isLoadingTexture = true;
                loadTexture();
                _textureIsDirty = false;
            }
    }
    // Extract J2000 time from file names
    // Requires files to be named as such: 'YYYY-MM-DDTHH-MM-SS-XXX.json'
    void RenderableTimeVaryingPlaneImageLocal::extractTriggerTimesFromFileNames() {
        // number of  characters in filename (excluding '.json')
        constexpr const int FilenameSize = 23;
        // size(".json")
        constexpr const int ExtSize = 4;

        for (const std::string& filePath : _sourceFiles) {
            LDEBUG("filepath " + filePath);
            const size_t strLength = filePath.size();
            // Extract the filename from the path (without extension)
            std::string timeString = filePath.substr(
                strLength - FilenameSize - ExtSize,
                FilenameSize - 1
            );
            // Ensure the separators are correct
            timeString.replace(4, 1, "-");
            timeString.replace(7, 1, "-");
            timeString.replace(13, 1, ":");
            timeString.replace(16, 1, ":");
            timeString.replace(19, 1, ".");
            const double triggerTime = Time::convertTime(timeString);
            LDEBUG("timestring " + timeString);
            _startTimes.push_back(triggerTime);
        }
    }
    void RenderableTimeVaryingPlaneImageLocal::updateActiveTriggerTimeIndex(double currentTime) {
        auto iter = std::upper_bound(_startTimes.begin(), _startTimes.end(), currentTime);
        if (iter != _startTimes.end()) {
            if (iter != _startTimes.begin()) {
                _activeTriggerTimeIndex = static_cast<int>(
                    std::distance(_startTimes.begin(), iter)
                    ) - 1;
            }
            else {
                _activeTriggerTimeIndex = 0;
            }
        }
        else {
            _activeTriggerTimeIndex = static_cast<int>(_nStates) - 1;
        }
    }
    void RenderableTimeVaryingPlaneImageLocal::computeSequenceEndTime() {
        if (_nStates > 1) {
            const double lastTriggerTime = _startTimes[_nStates - 1];
            const double sequenceDuration = lastTriggerTime - _startTimes[0];
            const double averageStateDuration = sequenceDuration /
                (static_cast<double>(_nStates) - 1.0);
            _sequenceEndTime = lastTriggerTime + averageStateDuration;
        }
        else {
            // If there's just one state it should never disappear!
            _sequenceEndTime = DBL_MAX;
        }
    }
    void RenderableTimeVaryingPlaneImageLocal::loadTexture() {
        if (_activeTriggerTimeIndex != -1) {
            ghoul::opengl::Texture* t = _texture;
           
            unsigned int hash = ghoul::hashCRC32File(_sourceFiles[_activeTriggerTimeIndex]);

            _texture = BaseModule::TextureManager.request(
                std::to_string(hash),
                [path = _sourceFiles[_activeTriggerTimeIndex]]() -> std::unique_ptr<ghoul::opengl::Texture> {
                    std::unique_ptr<ghoul::opengl::Texture> texture =
                        ghoul::io::TextureReader::ref().loadTexture(absPath(path));

                   // LDEBUGC(
                    //    "RenderableTimeVaryingPlaneImageLocal",
                    //    fmt::format("Loaded texture from '{}'", absPath(path))
                    //);
                    texture->uploadTexture();
                    texture->setFilter(ghoul::opengl::Texture::FilterMode::LinearMipMap);
                    texture->purgeFromRAM();

                    return texture;
                }
            );

            BaseModule::TextureManager.release(t);

            _textureFile = std::make_unique<ghoul::filesystem::File>(_sourceFiles[_activeTriggerTimeIndex]);
            _textureFile->setCallback(
                [&](const ghoul::filesystem::File&) { _textureIsDirty = true; }
            );
            _isLoadingTexture = false;
        }
    }

} // namespace openspace
