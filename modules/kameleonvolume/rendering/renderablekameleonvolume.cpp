/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#include <modules/kameleonvolume/rendering/renderablekameleonvolume.h>

#include <modules/kameleon/include/kameleonwrapper.h>
#include <modules/kameleonvolume/kameleonvolumereader.h>
#include <modules/volume/rawvolumereader.h>
#include <modules/volume/rawvolumewriter.h>
#include <modules/volume/rendering/basicvolumeraycaster.h>
#include <modules/volume/rendering/volumeclipplanes.h>
#include <modules/volume/transferfunctionhandler.h>
#include <modules/volume/volumegridtype.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/raycastermanager.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/texture.h>

namespace {
    constexpr const char* _loggerCat = "RenderableKameleonVolume";

    constexpr const char* KeyDimensions = "Dimensions";
    constexpr const char* KeyStepSize = "StepSize";
    constexpr const char* KeyTransferFunction = "TransferFunction";
    constexpr const char* KeySource = "Source";
    constexpr const char* KeyVariable = "Variable";
    constexpr const char* KeyLowerDomainBound = "LowerDomainBound";
    constexpr const char* KeyUpperDomainBound = "UpperDomainBound";
    constexpr const char* KeyDomainScale = "DomainScale";
    constexpr const char* KeyLowerValueBound = "LowerValueBound";
    constexpr const char* KeyUpperValueBound = "UpperValueBound";
    constexpr const char* KeyClipPlanes = "ClipPlanes";
    constexpr const char* KeyCache = "Cache";
    constexpr const char* KeyGridType = "GridType";
    constexpr const char* ValueSphericalGridType = "Spherical";

    constexpr openspace::properties::Property::PropertyInfo DimensionsInfo = {
        "Dimensions",
        "Dimensions",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo VariableInfo = {
        "Variable",
        "Variable",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo LowerDomainBoundInfo = {
        "LowerDomainBound",
        "Lower Domain Bound",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo UpperDomainBoundInfo = {
        "UpperDomainBound",
        "Upper Domain Bound",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo DomainScaleInfo = {
        "DomainScale",
        "Domain scale",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo LowerValueBoundInfo = {
        "LowerValueBound",
        "Lower Value Bound",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo UpperValueBoundInfo = {
        "UpperValueBound",
        "Upper Value Bound",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo GridTypeInfo = {
        "GridType",
        "Grid Type",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo StepSizeInfo = {
        "StepSize",
        "Step Size",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo SourcePathInfo = {
        "SourcePath",
        "Source Path",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo TransferFunctionInfo = {
        "TransferFunctionPath",
        "Transfer Function Path",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo CacheInfo = {
        "Cache",
        "Cache",
        "" // @TODO Missing documentation
    };
} // namespace

namespace openspace::kameleonvolume {

RenderableKameleonVolume::RenderableKameleonVolume(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _dimensions(DimensionsInfo)
    , _variable(VariableInfo)
    , _lowerDomainBound(LowerDomainBoundInfo)
    , _upperDomainBound(UpperDomainBoundInfo)
    , _domainScale(DomainScaleInfo, glm::vec3(1.f))
    , _lowerValueBound(LowerValueBoundInfo, 0.f, 0.f, 1.f)
    , _upperValueBound(UpperValueBoundInfo, 1.f, 0.01f, 1.f)
    , _gridType(GridTypeInfo, properties::OptionProperty::DisplayType::Dropdown)
    , _stepSize(StepSizeInfo, 0.02f, 0.01f, 1.f)
    , _sourcePath(SourcePathInfo)
    , _transferFunctionPath(TransferFunctionInfo)
    , _cache(CacheInfo)
{
    if (dictionary.hasKeyAndValue<glm::vec3>(KeyDimensions)) {
        _dimensions = dictionary.value<glm::vec3>(KeyDimensions);
    }
    else {
        LWARNING("No dimensions specified for volumetric data, falling back to 32^3");
        _dimensions = glm::uvec3(32);
    }

    _stepSize = dictionary.value<float>(KeyStepSize);

    if (dictionary.hasKeyAndValue<std::string>(KeyTransferFunction)) {
        _transferFunctionPath = dictionary.value<std::string>(KeyTransferFunction);
        _transferFunction = std::make_shared<openspace::TransferFunction>(
            _transferFunctionPath, [](const openspace::TransferFunction&) {}
        );
    }

    if (dictionary.hasKeyAndValue<std::string>(KeySource)) {
        _sourcePath = absPath(dictionary.value<std::string>(KeySource));
    }

    if (dictionary.hasKeyAndValue<std::string>(KeyVariable)) {
        _variable = dictionary.value<std::string>(KeyVariable);
    }

    if (dictionary.hasKeyAndValue<glm::vec3>(KeyLowerDomainBound)) {
        _lowerDomainBound = dictionary.value<glm::vec3>(KeyLowerDomainBound);
    }
    else {
        _autoDomainBounds = true;
    }

    if (dictionary.hasKeyAndValue<glm::vec3>(KeyUpperDomainBound)) {
        _upperDomainBound = dictionary.value<glm::vec3>(KeyUpperDomainBound);
    }
    else {
        _autoDomainBounds = true;
    }

    if (dictionary.hasKeyAndValue<glm::vec3>(KeyDomainScale)) {
        _domainScale = dictionary.value<glm::vec3>(KeyDomainScale);
    }

    if (dictionary.hasKeyAndValue<float>(KeyLowerValueBound)) {
        _lowerValueBound = dictionary.value<float>(KeyLowerValueBound);
    }
    else {
        _autoValueBounds = true;
    }

    if (dictionary.hasKeyAndValue<float>(KeyUpperValueBound)) {
        _upperValueBound = dictionary.value<float>(KeyUpperValueBound);
    }
    else {
        _autoValueBounds = true;
    }

    ghoul::Dictionary clipPlanesDictionary;
    dictionary.getValue(KeyClipPlanes, clipPlanesDictionary);
    _clipPlanes = std::make_shared<volume::VolumeClipPlanes>(clipPlanesDictionary);
    _clipPlanes->setIdentifier("clipPlanes");
    _clipPlanes->setGuiName("Clip Planes");

    if (dictionary.hasKeyAndValue<bool>(KeyCache)) {
        _cache = dictionary.value<bool>(KeyCache);
    }

    _gridType.addOption(
        static_cast<int>(volume::VolumeGridType::Cartesian),
        "Cartesian grid"
    );
    _gridType.addOption(
        static_cast<int>(volume::VolumeGridType::Spherical),
        "Spherical grid"
    );
    _gridType.setValue(static_cast<int>(volume::VolumeGridType::Cartesian));

    if (dictionary.hasKeyAndValue<std::string>(KeyGridType)) {
        const std::string& gridType = dictionary.value<std::string>(KeyGridType);
        if (gridType == ValueSphericalGridType) {
            _gridType.setValue(static_cast<int>(volume::VolumeGridType::Spherical));
        }
        else {
            _autoGridType = true;
        }
    }
}

RenderableKameleonVolume::~RenderableKameleonVolume() {}

void RenderableKameleonVolume::initializeGL() {
    load();

    _volumeTexture->uploadTexture();

    _raycaster = std::make_unique<volume::BasicVolumeRaycaster>(
        _volumeTexture,
        _transferFunction,
        _clipPlanes
    );

    _raycaster->setStepSize(_stepSize);
    _gridType.onChange([this]() { _raycaster->setStepSize(_stepSize); });
    _raycaster->setGridType(static_cast<volume::VolumeGridType>(_gridType.value()));
    _gridType.onChange([this]() {
        _raycaster->setGridType(static_cast<volume::VolumeGridType>(_gridType.value()));
    });

    updateRaycasterModelTransform();
    _lowerDomainBound.onChange([this]() { updateRaycasterModelTransform(); });
    _upperDomainBound.onChange([this]() { updateRaycasterModelTransform(); });

    _raycaster->initialize();

    global::raycasterManager.attachRaycaster(*_raycaster.get());

    auto onChange = [&](bool enabled) {
        if (enabled) {
            global::raycasterManager.attachRaycaster(*_raycaster.get());
        }
        else {
            global::raycasterManager.detachRaycaster(*_raycaster.get());
        }
    };

    onEnabledChange(onChange);

    _clipPlanes->initialize();

    addProperty(_dimensions);
    addProperty(_stepSize);
    addProperty(_transferFunctionPath);
    addProperty(_sourcePath);
    addProperty(_variable);
    addProperty(_lowerDomainBound);
    addProperty(_upperDomainBound);
    addProperty(_domainScale);
    addProperty(_lowerValueBound);
    addProperty(_upperValueBound);
    addProperty(_gridType);
    addProperty(_cache);
    addPropertySubOwner(_clipPlanes.get());
}

void RenderableKameleonVolume::updateRaycasterModelTransform() {
    const glm::vec3 lBBoxBound = _domainScale.value() * _lowerDomainBound.value();
    const glm::vec3 uBBoxBound = _domainScale.value() * _upperDomainBound.value();

    const glm::vec3 scale = uBBoxBound - lBBoxBound;
    const glm::vec3 translation = (lBBoxBound + uBBoxBound) * 0.5f;

    const glm::mat4 modelTransform = glm::scale(
        glm::translate(glm::mat4(1.f), translation),
        scale
    );
    _raycaster->setModelTransform(modelTransform);
}

bool RenderableKameleonVolume::isCachingEnabled() const {
    return _cache;
}

void RenderableKameleonVolume::load() {
    if (!FileSys.fileExists(ghoul::filesystem::File(_sourcePath))) {
        LERROR(fmt::format("File '{}' does not exist", _sourcePath.value()));
        return;
    }
    if (!isCachingEnabled()) {
        loadFromPath(_sourcePath);
        return;
    }
    ghoul::filesystem::File sourceFile(_sourcePath);
    std::string cachePath = FileSys.cacheManager()->cachedFilename(
        sourceFile.baseName(),
        cacheSuffix(),
        ghoul::filesystem::CacheManager::Persistent::Yes
    );
    if (FileSys.fileExists(cachePath)) {
        loadRaw(cachePath);
    }
    else {
        loadFromPath(_sourcePath);
        storeRaw(cachePath);
    }
}

std::string RenderableKameleonVolume::cacheSuffix() const {
    glm::uvec3 dims = _dimensions;
    return fmt::format(".{}.{}x{}x{}", _variable, dims[0], dims[1], dims[2]);
}

void RenderableKameleonVolume::loadFromPath(const std::string& path) {
    ghoul::filesystem::File file(path);
    std::string extension = file.fileExtension();
    std::transform(
        extension.begin(),
        extension.end(),
        extension.begin(),
        [](char v) { return static_cast<char>(tolower(v)); }
    );
    if (extension == "cdf") {
        loadCdf(path);
    }
    else {
        loadRaw(path);
    }
}

void RenderableKameleonVolume::loadRaw(const std::string& path) {
    volume::RawVolumeReader<float> reader(path, _dimensions);
    _rawVolume = reader.read();
    updateTextureFromVolume();
}

void RenderableKameleonVolume::loadCdf(const std::string& path) {
    KameleonVolumeReader reader(path);

    if (_autoValueBounds) {
        _lowerValueBound = static_cast<float>(reader.minValue(_variable));
        _upperValueBound = static_cast<float>(reader.maxValue(_variable));
    }

    std::array<std::string, 3> variables = reader.gridVariableNames();

    if (_autoDomainBounds) {
        _lowerDomainBound = glm::vec3(
            reader.minValue(variables[0]),
            reader.minValue(variables[1]),
            reader.minValue(variables[2])
        );

        _upperDomainBound = glm::vec3(
            reader.maxValue(variables[0]),
            reader.maxValue(variables[1]),
            reader.maxValue(variables[2])
        );
    }

    if (_autoGridType) {
        if (variables[0] == "r" && variables[0] == "theta" && variables[0] == "phi") {
            _gridType.setValue(static_cast<int>(volume::VolumeGridType::Spherical));
        }
        else {
            _gridType.setValue(static_cast<int>(volume::VolumeGridType::Cartesian));
        }
    }

    ghoul::Dictionary dict = reader.readMetaData();
    _rawVolume = reader.readFloatVolume(
        _dimensions,
        _variable,
        _lowerDomainBound,
        _upperDomainBound
    );
    updateTextureFromVolume();
}

void RenderableKameleonVolume::updateTextureFromVolume() {
    _normalizedVolume = std::make_unique<volume::RawVolume<GLfloat>>(_dimensions);
    float* in = _rawVolume->data();
    GLfloat* out = _normalizedVolume->data();
    float min = _lowerValueBound;
    float diff = _upperValueBound - _lowerValueBound;

    for (size_t i = 0; i < _normalizedVolume->nCells(); ++i) {
        out[i] = glm::clamp((in[i] - min) / diff, 0.f, 1.f);
    }

    _volumeTexture = std::make_shared<ghoul::opengl::Texture>(
        _dimensions,
        ghoul::opengl::Texture::Format::Red,
        GL_RED,
        GL_FLOAT,
        ghoul::opengl::Texture::FilterMode::Linear,
        ghoul::opengl::Texture::WrappingMode::Repeat
    );

    void* data = reinterpret_cast<void*>(_normalizedVolume->data());
    _volumeTexture->setPixelData(data, ghoul::opengl::Texture::TakeOwnership::No);
}

void RenderableKameleonVolume::storeRaw(const std::string& path) {
    volume::RawVolumeWriter<float> writer(path);
    writer.write(*_rawVolume);
}

void RenderableKameleonVolume::deinitializeGL() {
    if (_raycaster) {
        global::raycasterManager.detachRaycaster(*_raycaster.get());
        _raycaster = nullptr;
    }
}

bool RenderableKameleonVolume::isReady() const {
    return true;
}

void RenderableKameleonVolume::update(const UpdateData&) {
    if (_raycaster) {
        _raycaster->setStepSize(_stepSize);
    }
}

void RenderableKameleonVolume::render(const RenderData& data, RendererTasks& tasks) {
    tasks.raycasterTasks.push_back({ _raycaster.get(), data });
}

}  // openspace::kameleonvolume
