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

#include <modules/volume/rendering/renderabletimevaryingvolume.h>
#include <modules/volume/rawvolumereader.h>
#include <modules/volume/rawvolume.h>

#include <openspace/rendering/renderable.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/raycastermanager.h>

#include <ghoul/glm.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/logging/logmanager.h>

#include <glm/gtc/matrix_transform.hpp>

namespace {
    const char* _loggerCat = "RenderableTimeVaryingVolume";
}

namespace {
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
}

namespace openspace {

RenderableTimeVaryingVolume::RenderableTimeVaryingVolume(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _lowerDomainBound("lowerDomainBound", "Lower Domain Bound")
    , _upperDomainBound("upperDomainBound", "Upper Domain Bound")
    , _domainScale("domainScale", "Domain scale")
    , _lowerValueBound("lowerValueBound", "Lower Value Bound", 0.0, 0.0, 1)
    , _upperValueBound("upperValueBound", "Upper Value Bound", 1, 0.01, 1)
    , _gridType("gridType", "Grid Type", properties::OptionProperty::DisplayType::Dropdown)
    , _clipPlanes(nullptr)
    , _stepSize("stepSize", "Step Size", 0.02, 0.01, 1)
    , _sourcePath("sourcePath", "Source Path")
    , _transferFunctionPath("transferFunctionPath", "Transfer Function Path")
    , _raycaster(nullptr)
    , _transferFunction(nullptr) {
    /*
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
    */
}
    
RenderableTimeVaryingVolume::~RenderableTimeVaryingVolume() {}


bool RenderableTimeVaryingVolume::initialize() {
    return true;
}
/*
void RenderableTimeVaryingVolume::updateRaycasterModelTransform() {
    glm::vec3 lowerBoundingBoxBound = _domainScale.value() * _lowerDomainBound.value();
    glm::vec3 upperBoundingBoxBound = _domainScale.value() * _upperDomainBound.value();
    
    glm::vec3 scale = upperBoundingBoxBound - lowerBoundingBoxBound;
    glm::vec3 translation = (lowerBoundingBoxBound + upperBoundingBoxBound) * 0.5f;

    glm::mat4 modelTransform = glm::translate(glm::mat4(1.0), translation); 
    modelTransform = glm::scale(modelTransform, scale);
    _raycaster->setModelTransform(modelTransform);
}


bool RenderableTimeVaryingVolume::cachingEnabled() {
    return _cache;
}

void RenderableTimeVaryingVolume::load() {
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

std::string RenderableTimeVaryingVolume::cacheSuffix() {
    glm::vec3 dims = _dimensions.value();
    return "." + _variable.value() +
        "." + std::to_string(dims[0]) +
        "x" + std::to_string(dims[1]) +
        "x" + std::to_string(dims[2]);
}

void RenderableTimeVaryingVolume::loadFromPath(const std::string& path) {
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

void RenderableTimeVaryingVolume::loadRaw(const std::string& path) {
    RawVolumeReader<float> reader(path, _dimensions);
    _rawVolume = reader.read();
    updateTextureFromVolume();
}

void RenderableTimeVaryingVolume::updateTextureFromVolume() {
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

void RenderableTimeVaryingVolume::storeRaw(const std::string& path) {
    RawVolumeWriter<float> writer(path);
    writer.write(*_rawVolume);
}
    
*/

void RenderableTimeVaryingVolume::update(const UpdateData& data) {
    if (_raycaster) {
        _raycaster->setStepSize(_stepSize);
    }
}

void RenderableTimeVaryingVolume::render(const RenderData& data, RendererTasks& tasks) {
    //tasks.raycasterTasks.push_back({ _raycaster.get(), data });
}

 

bool RenderableTimeVaryingVolume::isReady() const {
    return true;
}


bool RenderableTimeVaryingVolume::deinitialize() {
    if (_raycaster) {
        OsEng.renderEngine().raycasterManager().detachRaycaster(*_raycaster.get());
        _raycaster = nullptr;
    }
    return true;
}

}
