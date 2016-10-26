/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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
#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>

#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/logging/logmanager.h>

#include <modules/volume/rawvolumereader.h>
#include <modules/volume/rawvolumewriter.h>
#include <modules/volume/rawvolume.h>

namespace {
    const std::string _loggerCat = "RenderableKameleonVolume";
}

namespace openspace {

RenderableKameleonVolume::RenderableKameleonVolume(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _dimensions("dimensions", "Dimensions")
    , _variable("variable", "Variable")
    , _lowerDomainBound("lowerDomainBound", "Lower Domain Bound")
    , _upperDomainBound("upperDomainBound", "Upper Domain Bound")
    , _domainScale("domainScale", "Domain scale")
    , _autoDomainBounds(false)
    , _lowerValueBound("lowerValueBound", "Lower Value Bound", 0.0, 0.0, 1)
    , _upperValueBound("upperValueBound", "Upper Value Bound", 1, 0.01, 1)
    , _autoValueBounds(false)
    , _gridType("gridType", "Grid Type", properties::OptionProperty::DisplayType::DROPDOWN)
    , _autoGridType(false)
    , _stepSize("stepSize", "Step Size", 0.02, 0.01, 1)
    , _sourcePath("sourcePath", "Source Path")
    , _transferFunctionPath("transferFunctionPath", "Transfer Function Path")
    , _raycaster(nullptr)
    , _transferFunction(nullptr)
    , _cache("cache", "Cache") {

    glm::vec3 dimensions;
    if (dictionary.getValue("Dimensions", dimensions)) {
        _dimensions = dimensions;
    } else {
        LWARNING("No dimensions specified for volumetric data, falling back to 32^3");
        _dimensions = glm::uvec3(32, 32, 32);
    }

    float stepSize;
    if (dictionary.getValue("StepSize", stepSize)) {
        _stepSize = stepSize;
    }

    std::string transferFunctionPath;
    if (dictionary.getValue("TransferFunction", transferFunctionPath)) {
        _transferFunctionPath = transferFunctionPath;
        _transferFunction = std::make_shared<TransferFunction>(absPath(transferFunctionPath));
    }

    std::string sourcePath;
    if (dictionary.getValue("Source", sourcePath)) {
        _sourcePath = absPath(sourcePath);
    }
    
    std::string variable;
    if (dictionary.getValue("Variable", variable)) {
        _variable = variable;
    }

    glm::vec3 lowerDomainBound;
    if (dictionary.getValue("LowerDomainBound", lowerDomainBound)) {
        _lowerDomainBound = lowerDomainBound;
    }
    else {
        _autoDomainBounds = true;
    }

    glm::vec3 upperDomainBound;
    if (dictionary.getValue("UpperDomainBound", upperDomainBound)) {
        _upperDomainBound = upperDomainBound;
    }
    else {
        _autoDomainBounds = true;
    }

    glm::vec3 domainScale;
    if (dictionary.getValue("DomainScale", domainScale)) {
        _domainScale = domainScale;
    } else {
        _domainScale = glm::vec3(1, 1, 1); // Assume meters if nothing else is specified.
    }

    float lowerValueBound;
    if (dictionary.getValue("LowerValueBound", lowerValueBound)) {
        _lowerValueBound = lowerValueBound;
    }
    else {
        _autoValueBounds = true;
    }

    float upperValueBound;
    if (dictionary.getValue("UpperValueBound", upperValueBound)) {
        _upperValueBound = upperValueBound;
    }
    else {
        _autoValueBounds = true;
    }


    bool cache;
    if (dictionary.getValue("Cache", cache)) {
        _cache = cache;
    }

    _gridType.addOption(static_cast<int>(VolumeGridType::Cartesian), "Cartesian grid");
    _gridType.addOption(static_cast<int>(VolumeGridType::Spherical), "Spherical grid");
    _gridType.setValue(static_cast<int>(VolumeGridType::Cartesian));

    std::string gridType;
    if (dictionary.getValue("GridType", gridType)) {
        if (gridType == "Spherical") {
            _gridType.setValue(static_cast<int>(VolumeGridType::Spherical));
        } else {
            _autoGridType = true;
        }
    }

    // TODO: read transformation: position/rotation/scale from dictionary.
}
    
RenderableKameleonVolume::~RenderableKameleonVolume() {}

bool RenderableKameleonVolume::initialize() {
    load();
    
    _volumeTexture->uploadTexture();
    _transferFunction->update();

    _raycaster = std::make_unique<KameleonVolumeRaycaster>(_volumeTexture, _transferFunction);

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

    std::function<void(bool)> onChange = [&](bool enabled) {
        if (enabled) {
            OsEng.renderEngine().raycasterManager().attachRaycaster(*_raycaster.get());
        }
        else {
            OsEng.renderEngine().raycasterManager().detachRaycaster(*_raycaster.get());
        }
    };

    onEnabledChange(onChange);

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
    std::transform(extension.begin(), extension.end(), extension.begin(), ::tolower);
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

    for (size_t i = 0; i < _normalizedVolume->nCells(); i++) {
        out[i] = glm::clamp((in[i] - min) / diff, 0.0f, 1.0f);
    }

    _volumeTexture = std::make_shared<ghoul::opengl::Texture>(
        _dimensions,
        ghoul::opengl::Texture::Format::Red,
        GL_RED,
        GL_FLOAT,
        ghoul::opengl::Texture::FilterMode::Linear,
        ghoul::opengl::Texture::WrappingMode::Repeat);

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
    
void RenderableKameleonVolume::update(const UpdateData& data) {
    // forward this transformation! 
    if (_raycaster) {
        _raycaster->setStepSize(_stepSize);
    }
}

void RenderableKameleonVolume::render(const RenderData& data, RendererTasks& tasks) {
    RaycasterTask task{ _raycaster.get(), data };
    tasks.raycasterTasks.push_back(task);
}
       
}
