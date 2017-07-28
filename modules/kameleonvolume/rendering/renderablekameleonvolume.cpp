/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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
#include <modules/kameleonvolume/kameleonvolumereader.h>

#include <openspace/rendering/renderable.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/raycastermanager.h>
#include <ghoul/glm.h>
#include <glm/gtc/matrix_transform.hpp>

#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/logging/logmanager.h>

#include <modules/volume/rawvolumereader.h>
#include <modules/volume/rawvolumewriter.h>
#include <modules/volume/rawvolume.h>

namespace {
    const char* _loggerCat = "RenderableKameleonVolume";

    const char* KeyDimensions = "Dimensions";
    const char* KeyStepSize = "StepSize";
    const char* KeyTransferFunction = "TransferFunction";
    const char* KeySource = "Source";
    const char* KeyVariable = "Variable";
    const char* KeyLowerDomainBound = "LowerDomainBound";
    const char* KeyUpperDomainBound = "UpperDomainBound";
    const char* KeyDomainScale = "DomainScale";
    const char* KeyLowerValueBound = "LowerValueBound";
    const char* KeyUpperValueBound = "UpperValueBound";
    const char* KeyClipPlanes = "ClipPlanes";
    const char* KeyCache = "Cache";
    const char* KeyGridType = "GridType";
    const char* ValueSphericalGridType = "Spherical";

    static const openspace::properties::Property::PropertyInfo DimensionsInfo = {
        "Dimensions",
        "Dimensions",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo VariableInfo = {
        "Variable",
        "Variable",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo LowerDomainBoundInfo = {
        "LowerDomainBound",
        "Lower Domain Bound",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo UpperDomainBoundInfo = {
        "UpperDomainBound",
        "Upper Domain Bound",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo DomainScaleInfo = {
        "DomainScale",
        "Domain scale",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo LowerValueBoundInfo = {
        "LowerValueBound",
        "Lower Value Bound",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo UpperValueBoundInfo = {
        "UpperValueBound",
        "Upper Value Bound",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo GridTypeInfo = {
        "GridType",
        "Grid Type",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo StepSizeInfo = {
        "StepSize",
        "Step Size",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo SourcePathInfo = {
        "SourcePath",
        "Source Path",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo TransferFunctionInfo = {
        "TransferFunctionPath",
        "Transfer Function Path",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo CacheInfo = {
        "Cache",
        "Cache",
        "" // @TODO Missing documentation
    };
} // namespace

namespace openspace {

RenderableKameleonVolume::RenderableKameleonVolume(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _dimensions(DimensionsInfo)
    , _variable(VariableInfo)
    , _lowerDomainBound(LowerDomainBoundInfo)
    , _upperDomainBound(UpperDomainBoundInfo)
    , _domainScale(DomainScaleInfo)
    , _autoDomainBounds(false)
    , _lowerValueBound(LowerValueBoundInfo, 0.f, 0.f, 1.f)
    , _upperValueBound(UpperValueBoundInfo, 1.f, 0.01f, 1.f)
    , _autoValueBounds(false)
    , _gridType(GridTypeInfo, properties::OptionProperty::DisplayType::Dropdown)
    , _autoGridType(false)
    , _clipPlanes(nullptr)
    , _stepSize(StepSizeInfo, 0.02f, 0.01f, 1.f)
    , _sourcePath(SourcePathInfo)
    , _transferFunctionPath(TransferFunctionInfo)
    , _raycaster(nullptr)
    , _transferFunction(nullptr)
    , _cache(CacheInfo)
{

    glm::vec3 dimensions;
    if (dictionary.getValue(KeyDimensions, dimensions)) {
        _dimensions = dimensions;
    } else {
        LWARNING("No dimensions specified for volumetric data, falling back to 32^3");
        _dimensions = glm::uvec3(32, 32, 32);
    }

    float stepSize;
    if (dictionary.getValue(KeyStepSize, stepSize)) {
        _stepSize = stepSize;
    }

    std::string transferFunctionPath;
    if (dictionary.getValue(KeyTransferFunction, transferFunctionPath)) {
        _transferFunctionPath = transferFunctionPath;
        _transferFunction = std::make_shared<TransferFunction>(absPath(transferFunctionPath));
    }

    std::string sourcePath;
    if (dictionary.getValue(KeySource, sourcePath)) {
        _sourcePath = absPath(sourcePath);
    }
    
    std::string variable;
    if (dictionary.getValue(KeyVariable, variable)) {
        _variable = variable;
    }

    glm::vec3 lowerDomainBound;
    if (dictionary.getValue(KeyLowerDomainBound, lowerDomainBound)) {
        _lowerDomainBound = lowerDomainBound;
    }
    else {
        _autoDomainBounds = true;
    }

    glm::vec3 upperDomainBound;
    if (dictionary.getValue(KeyUpperDomainBound, upperDomainBound)) {
        _upperDomainBound = upperDomainBound;
    }
    else {
        _autoDomainBounds = true;
    }

    glm::vec3 domainScale;
    if (dictionary.getValue(KeyDomainScale, domainScale)) {
        _domainScale = domainScale;
    } else {
        _domainScale = glm::vec3(1, 1, 1); // Assume meters if nothing else is specified.
    }

    float lowerValueBound;
    if (dictionary.getValue(KeyLowerValueBound, lowerValueBound)) {
        _lowerValueBound = lowerValueBound;
    }
    else {
        _autoValueBounds = true;
    }

    float upperValueBound;
    if (dictionary.getValue(KeyUpperValueBound, upperValueBound)) {
        _upperValueBound = upperValueBound;
    }
    else {
        _autoValueBounds = true;
    }

    ghoul::Dictionary clipPlanesDictionary;
    dictionary.getValue(KeyClipPlanes, clipPlanesDictionary);
    _clipPlanes = std::make_shared<VolumeClipPlanes>(clipPlanesDictionary);
    _clipPlanes->setName("clipPlanes");

    bool cache;
    if (dictionary.getValue(KeyCache, cache)) {
        _cache = cache;
    }

    _gridType.addOption(static_cast<int>(VolumeGridType::Cartesian), "Cartesian grid");
    _gridType.addOption(static_cast<int>(VolumeGridType::Spherical), "Spherical grid");
    _gridType.setValue(static_cast<int>(VolumeGridType::Cartesian));

    std::string gridType;
    if (dictionary.getValue(KeyGridType, gridType)) {
        if (gridType == ValueSphericalGridType) {
            _gridType.setValue(static_cast<int>(VolumeGridType::Spherical));
        } else {
            _autoGridType = true;
        }
    }
}
    
RenderableKameleonVolume::~RenderableKameleonVolume() {}

bool RenderableKameleonVolume::initialize() {
    load();
    
    _volumeTexture->uploadTexture();
    _transferFunction->update();

    _raycaster = std::make_unique<KameleonVolumeRaycaster>(_volumeTexture, _transferFunction, _clipPlanes);

    _raycaster->setStepSize(_stepSize);
    _gridType.onChange([this] {
        _raycaster->setStepSize(_stepSize);
    });
    _raycaster->setGridType(static_cast<VolumeGridType>(_gridType.value()));
    _gridType.onChange([this] {
        _raycaster->setGridType(static_cast<VolumeGridType>(_gridType.value()));
    });

    updateRaycasterModelTransform();
    _lowerDomainBound.onChange([this] {
        updateRaycasterModelTransform();
    });
    _upperDomainBound.onChange([this] {
        updateRaycasterModelTransform();
    });

    _raycaster->initialize();

    OsEng.renderEngine().raycasterManager().attachRaycaster(*_raycaster.get());

    auto onChange = [&](bool enabled) {
        if (enabled) {
            OsEng.renderEngine().raycasterManager().attachRaycaster(*_raycaster.get());
        }
        else {
            OsEng.renderEngine().raycasterManager().detachRaycaster(*_raycaster.get());
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
    return true;
}

void RenderableKameleonVolume::updateRaycasterModelTransform() {
    glm::vec3 lowerBoundingBoxBound = _domainScale.value() * _lowerDomainBound.value();
    glm::vec3 upperBoundingBoxBound = _domainScale.value() * _upperDomainBound.value();
    
    glm::vec3 scale = upperBoundingBoxBound - lowerBoundingBoxBound;
    glm::vec3 translation = (lowerBoundingBoxBound + upperBoundingBoxBound) * 0.5f;

    glm::mat4 modelTransform = glm::translate(glm::mat4(1.0), translation); 
    modelTransform = glm::scale(modelTransform, scale);
    _raycaster->setModelTransform(modelTransform);
}


bool RenderableKameleonVolume::cachingEnabled() {
    return _cache;
}

void RenderableKameleonVolume::load() {
    if (!FileSys.fileExists(_sourcePath)) {
        LERROR("File " << _sourcePath << " does not exist."); 
        return;
    }
    if (!cachingEnabled()) {
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
    } else {
        loadFromPath(_sourcePath);
        storeRaw(cachePath);
    }
}

std::string RenderableKameleonVolume::cacheSuffix() {
    glm::vec3 dims = _dimensions.value();
    return "." + _variable.value() +
        "." + std::to_string(dims[0]) +
        "x" + std::to_string(dims[1]) +
        "x" + std::to_string(dims[2]);
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
    } else {
        loadRaw(path);
    }
}

void RenderableKameleonVolume::loadRaw(const std::string& path) {
    RawVolumeReader<float> reader(path, _dimensions);
    _rawVolume = reader.read();
    updateTextureFromVolume();
}

void RenderableKameleonVolume::loadCdf(const std::string& path) {
    KameleonVolumeReader reader(path);

    if (_autoValueBounds) {
        _lowerValueBound = reader.minValue(_variable);
        _upperValueBound = reader.maxValue(_variable);
    }

    std::vector<std::string> variables = reader.gridVariableNames();
   
    if (variables.size() == 3 && _autoDomainBounds) {
        _lowerDomainBound = glm::vec3(
            reader.minValue(variables[0]),
            reader.minValue(variables[1]),
            reader.minValue(variables[2]));

        _upperDomainBound = glm::vec3(
            reader.maxValue(variables[0]),
            reader.maxValue(variables[1]),
            reader.maxValue(variables[2]));
    }

    if (variables.size() == 3 && _autoGridType) {
        if (variables[0] == "r" && variables[0] == "theta" && variables[0] == "phi") {
            _gridType.setValue(static_cast<int>(VolumeGridType::Spherical));
        }
        else {
            _gridType.setValue(static_cast<int>(VolumeGridType::Cartesian));
        }       
    }

    ghoul::Dictionary dict = reader.readMetaData();
    _rawVolume = reader.readFloatVolume(_dimensions, _variable, _lowerDomainBound, _upperDomainBound);
    updateTextureFromVolume();
}

void RenderableKameleonVolume::updateTextureFromVolume() {
    _normalizedVolume = std::make_unique<RawVolume<GLfloat>>(_dimensions);
    float* in = _rawVolume->data();
    GLfloat* out = _normalizedVolume->data();
    float min = _lowerValueBound;
    float diff = _upperValueBound - _lowerValueBound;

    for (size_t i = 0; i < _normalizedVolume->nCells(); ++i) {
        out[i] = glm::clamp((in[i] - min) / diff, 0.0f, 1.0f);
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
    RawVolumeWriter<float> writer(path);
    writer.write(*_rawVolume);
}
    
bool RenderableKameleonVolume::deinitialize() {
    if (_raycaster) {
        OsEng.renderEngine().raycasterManager().detachRaycaster(*_raycaster.get());
        _raycaster = nullptr;
    }
    return true;
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
       
}
