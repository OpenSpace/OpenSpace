/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#include <modules/digitaluniverse/rendering/renderableplanescloud.h>

#include <modules/digitaluniverse/digitaluniversemodule.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <array>
#include <fstream>
#include <optional>
#include <string>

namespace {
    constexpr const char* _loggerCat = "RenderablePlanesCloud";
    constexpr const char* ProgramObjectName = "RenderablePlanesCloud";

    constexpr std::array<const char*, 4> UniformNames = {
        "modelViewProjectionTransform", "alphaValue", "fadeInValue", "galaxyTexture"
    };

    constexpr int8_t CurrentCacheVersion = 2;
    constexpr double PARSEC = 0.308567756E17;

    enum BlendMode {
        BlendModeNormal = 0,
        BlendModeAdditive
    };

    constexpr openspace::properties::Property::PropertyInfo ScaleFactorInfo = {
        "ScaleFactor",
        "Scale Factor",
        "This value is used as a multiplicative factor that is applied to the apparent "
        "size of each point."
    };

    constexpr openspace::properties::Property::PropertyInfo TextColorInfo = {
        "TextColor",
        "Text Color",
        "The text color for the astronomical object."
    };

    constexpr openspace::properties::Property::PropertyInfo TextOpacityInfo = {
        "TextOpacity",
        "Text Opacity",
        "Determines the transparency of the text label, where 1 is completely opaque "
        "and 0 fully transparent."
    };

    constexpr openspace::properties::Property::PropertyInfo TextSizeInfo = {
        "TextSize",
        "Text Size",
        "The text size for the astronomical object labels."
    };

    constexpr openspace::properties::Property::PropertyInfo LabelFileInfo = {
        "LabelFile",
        "Label File",
        "The path to the label file that contains information about the astronomical "
        "objects being rendered."
    };

    constexpr openspace::properties::Property::PropertyInfo LabelMinSizeInfo = {
        "TextMinSize",
        "Text Min Size",
        "The minimal size (in pixels) of the text for the labels for the astronomical "
        "objects being rendered."
    };

    constexpr openspace::properties::Property::PropertyInfo LabelMaxSizeInfo = {
        "TextMaxSize",
        "Text Max Size",
        "The maximum size (in pixels) of the text for the labels for the astronomical "
        "objects being rendered."
    };

    constexpr openspace::properties::Property::PropertyInfo DrawElementsInfo = {
        "DrawElements",
        "Draw Elements",
        "Enables/Disables the drawing of the astronomical objects."
    };

    constexpr openspace::properties::Property::PropertyInfo TransformationMatrixInfo = {
        "TransformationMatrix",
        "Transformation Matrix",
        "Transformation matrix to be applied to each astronomical object."
    };

    constexpr openspace::properties::Property::PropertyInfo BlendModeInfo = {
        "BlendMode",
        "Blending Mode",
        "This determines the blending mode that is applied to this plane."
    };

    constexpr openspace::properties::Property::PropertyInfo TexturePathInfo = {
        "TexturePath",
        "Texture Path",
        "This value specifies the path for the textures in disk."
    };

    constexpr openspace::properties::Property::PropertyInfo LuminosityInfo = {
        "Luminosity",
        "Luminosity variable",
        "Datavar variable to control the luminosity/size of the astronomical objects."
    };

    constexpr openspace::properties::Property::PropertyInfo ScaleLuminosityInfo = {
        "ScaleLuminosity",
        "ScaleLuminosity variable",
        "Scaling control for the luminosity/size of the astronomical objects."
    };

    constexpr openspace::properties::Property::PropertyInfo RenderOptionInfo = {
        "RenderOption",
        "Render Option",
        "Debug option for rendering of billboards and texts."
    };

    constexpr openspace::properties::Property::PropertyInfo FadeInDistancesInfo = {
        "FadeInDistances",
        "Fade-In Start and End Distances",
        "These values determine the initial and final distances from the center of "
        "our galaxy from which the astronomical object will start and end "
        "fading-in."
    };

    constexpr openspace::properties::Property::PropertyInfo DisableFadeInInfo = {
        "DisableFadeIn",
        "Disable Fade-in effect",
        "Enables/Disables the Fade-in effect."
    };

    constexpr openspace::properties::Property::PropertyInfo PlaneMinSizeInfo = {
        "PlaneMinSize",
        "Plane Min Size in Pixels",
        "The min size (in pixels) for the plane representing the astronomical "
        "object."
    };

    struct [[codegen::Dictionary(RenderablePlanesCloud)]] Parameters {
        // The path to the SPECK file that contains information about the astronomical
        // object being rendered
        std::optional<std::string> file;

        // [[codegen::verbatim(ScaleFactorInfo.description)]]
        std::optional<float> scaleFactor;

        // [[codegen::verbatim(TextColorInfo.description)]]
        std::optional<glm::vec3> textColor [[codegen::color()]];

        // [[codegen::verbatim(TextOpacityInfo.description)]]
        std::optional<float> textOpacity;

        // [[codegen::verbatim(TextSizeInfo.description)]]
        std::optional<float> textSize;

        // [[codegen::verbatim(LabelFileInfo.description)]]
        std::optional<std::string> labelFile;

        // [[codegen::verbatim(LabelMinSizeInfo.description)]]
        std::optional<int> textMinSize;

        // [[codegen::verbatim(LabelMaxSizeInfo.description)]]
        std::optional<int> textMaxSize;

        // [[codegen::verbatim(TransformationMatrixInfo.description)]]
        std::optional<glm::dmat4x4> transformationMatrix;

        enum class BlendMode {
            Normal,
            Additive
        };

        // [[codegen::verbatim(BlendModeInfo.description)]]
        std::optional<BlendMode> blendMode;

        enum class Unit {
            Meter [[codegen::key("m")]],
            Kilometer [[codegen::key("Km")]],
            Parsec [[codegen::key("pc")]],
            Kiloparsec [[codegen::key("Kpc")]],
            Megaparsec [[codegen::key("Mpc")]],
            Gigaparsec [[codegen::key("Gpc")]],
            Gigalightyears [[codegen::key("Gly")]]
        };
        std::optional<Unit> unit;

        // [[codegen::verbatim(TexturePathInfo.description)]]
        std::string texturePath;

        // [[codegen::verbatim(LuminosityInfo.description)]]
        std::optional<std::string> luminosity;

        // [[codegen::verbatim(ScaleLuminosityInfo.description)]]
        std::optional<float> scaleLuminosity;

        // [[codegen::verbatim(FadeInDistancesInfo.description)]]
        std::optional<glm::vec2> fadeInDistances;

        // [[codegen::verbatim(DisableFadeInInfo.description)]]
        std::optional<bool> disableFadeIn;

        // [[codegen::verbatim(PlaneMinSizeInfo.description)]]
        std::optional<float> planeMinSize;
    };
#include "renderableplanescloud_codegen.cpp"
}  // namespace

namespace openspace {

documentation::Documentation RenderablePlanesCloud::Documentation() {
    documentation::Documentation doc = codegen::doc<Parameters>();
    doc.id = "digitaluniverse_RenderablePlanesCloud";
    return doc;
}

RenderablePlanesCloud::RenderablePlanesCloud(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _scaleFactor(ScaleFactorInfo, 1.f, 0.f, 300000.f)
    , _textColor(TextColorInfo, glm::vec3(1.f), glm::vec3(0.f), glm::vec3(1.f))
    , _textOpacity(TextOpacityInfo, 1.f, 0.f, 1.f)
    , _textSize(TextSizeInfo, 8.0, 0.5, 24.0)
    , _drawElements(DrawElementsInfo, true)
    , _blendMode(BlendModeInfo, properties::OptionProperty::DisplayType::Dropdown)
    , _fadeInDistance(
        FadeInDistancesInfo,
        glm::vec2(0.f),
        glm::vec2(0.f),
        glm::vec2(200000.f)
    )
    , _disableFadeInDistance(DisableFadeInInfo, true)
    , _planeMinSize(PlaneMinSizeInfo, 0.5, 0.0, 500.0)
    , _renderOption(RenderOptionInfo, properties::OptionProperty::DisplayType::Dropdown)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    addProperty(_opacity);

    if (p.file.has_value()) {
        _speckFile = absPath(*p.file);
        _hasSpeckFile = true;
        _drawElements.onChange([&]() { _hasSpeckFile = !_hasSpeckFile; });
        addProperty(_drawElements);
    }

    // DEBUG:
    _renderOption.addOption(0, "Camera View Direction");
    _renderOption.addOption(1, "Camera Position Normal");
    _renderOption.addOption(2, "Screen center Position Normal");
    addProperty(_renderOption);
    //_renderOption.set(1);

    if (p.unit.has_value()) {
        switch (*p.unit) {
            case Parameters::Unit::Meter:
                _unit = Meter;
                break;
            case Parameters::Unit::Kilometer:
                _unit = Kilometer;
                break;
            case Parameters::Unit::Parsec:
                _unit = Parsec;
                break;
            case Parameters::Unit::Kiloparsec:
                _unit = Kiloparsec;
                break;
            case Parameters::Unit::Megaparsec:
                _unit = Megaparsec;
                break;
            case Parameters::Unit::Gigaparsec:
                _unit = Gigaparsec;
                break;
            case Parameters::Unit::Gigalightyears:
                _unit = GigalightYears;
                break;
        }
    }
    else {
        LWARNING("No unit given for RenderablePlanesCloud. Using meters as units.");
        _unit = Meter;
    }

    _scaleFactor = p.scaleFactor.value_or(_scaleFactor);
    addProperty(_scaleFactor);
    _scaleFactor.onChange([&]() { _dataIsDirty = true; });

    if (p.labelFile.has_value()) {
        _labelFile = absPath(*p.labelFile);
        _hasLabel = true;

        _textColor = p.textColor.value_or(_textColor);
        _textColor.setViewOption(properties::Property::ViewOptions::Color);
        addProperty(_textColor);
        _textColor.onChange([&]() { _textColorIsDirty = true; });

        _textOpacity = p.textOpacity.value_or(_textOpacity);
        addProperty(_textOpacity);

        _textSize = p.textSize.value_or(_textSize);
        addProperty(_textSize);

        _textMinSize = p.textMinSize.value_or(_textMinSize);
        _textMaxSize = p.textMaxSize.value_or(_textMaxSize);
    }

    _transformationMatrix = p.transformationMatrix.value_or(_transformationMatrix);

    _blendMode.addOptions({
        { BlendModeNormal, "Normal" },
        { BlendModeAdditive, "Additive" }
    });
    _blendMode.onChange([&]() {
        switch (_blendMode) {
            case BlendModeNormal:
                setRenderBin(Renderable::RenderBin::Opaque);
                break;
            case BlendModeAdditive:
                setRenderBin(Renderable::RenderBin::PreDeferredTransparent);
                break;
            default:
                throw ghoul::MissingCaseException();
        }
    });

    if (p.blendMode.has_value()) {
        switch (*p.blendMode) {
            case Parameters::BlendMode::Normal:
                _blendMode = BlendModeNormal;
                break;
            case Parameters::BlendMode::Additive:
                _blendMode = BlendModeAdditive;
                break;
        }
    }

    _texturesPath = absPath(p.texturePath);

    _luminosityVar = p.luminosity.value_or(_luminosityVar);
    _sluminosity = p.scaleLuminosity.value_or(_sluminosity);


    if (p.fadeInDistances.has_value()) {
        _fadeInDistance = *p.fadeInDistances;
        _disableFadeInDistance = false;
        addProperty(_fadeInDistance);
        addProperty(_disableFadeInDistance);
    }

    _planeMinSize = p.planeMinSize.value_or(_planeMinSize);

    if (p.planeMinSize.has_value()) {
        addProperty(_planeMinSize);
    }
}

bool RenderablePlanesCloud::isReady() const {
    return ((_program != nullptr) && (!_fullData.empty())) || (!_labelData.empty());
}

void RenderablePlanesCloud::initialize() {
    ZoneScoped

    const bool success = loadData();
    if (!success) {
        throw ghoul::RuntimeError("Error loading data");
    }
}

void RenderablePlanesCloud::initializeGL() {
    ZoneScoped

    _program = DigitalUniverseModule::ProgramObjectManager.request(
        ProgramObjectName,
        []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
            return global::renderEngine->buildRenderProgram(
                "RenderablePlanesCloud",
                absPath("${MODULE_DIGITALUNIVERSE}/shaders/plane_vs.glsl"),
                absPath("${MODULE_DIGITALUNIVERSE}/shaders/plane_fs.glsl")
            );
        }
    );

    ghoul::opengl::updateUniformLocations(*_program, _uniformCache, UniformNames);

    createPlanes();

    loadTextures();

    if (_hasLabel) {
        if (!_font) {
            constexpr const int FontSize = 30;
            _font = global::fontManager->font(
                "Mono",
                static_cast<float>(FontSize),
                ghoul::fontrendering::FontManager::Outline::Yes,
                ghoul::fontrendering::FontManager::LoadGlyphs::No
            );
        }
    }
}


void RenderablePlanesCloud::deleteDataGPUAndCPU() {
    for (std::unordered_map<int, PlaneAggregate>::reference pAMapItem : _planesMap) {
        glDeleteBuffers(1, &pAMapItem.second.vbo);
        glDeleteVertexArrays(1, &pAMapItem.second.vao);
        pAMapItem.second.planesCoordinates.clear();
    }
    _planesMap.clear();
}

void RenderablePlanesCloud::deinitializeGL() {
    deleteDataGPUAndCPU();

    DigitalUniverseModule::ProgramObjectManager.release(
        ProgramObjectName,
        [](ghoul::opengl::ProgramObject* p) {
            global::renderEngine->removeRenderProgram(p);
        }
    );
}

void RenderablePlanesCloud::renderPlanes(const RenderData&,
                                         const glm::dmat4& modelViewMatrix,
                                         const glm::dmat4& projectionMatrix,
                                         const float fadeInVariable)
{
    glEnablei(GL_BLEND, 0);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE);
    //glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glDepthMask(false);

    _program->activate();

    glm::dmat4 modelViewProjectionMatrix = glm::dmat4(projectionMatrix) * modelViewMatrix;
    _program->setUniform(
        _uniformCache.modelViewProjectionTransform,
        modelViewProjectionMatrix
    );
    _program->setUniform(_uniformCache.alphaValue, _opacity);
    _program->setUniform(_uniformCache.fadeInValue, fadeInVariable);

    glDisable(GL_CULL_FACE);

    GLint viewport[4];
    global::renderEngine->openglStateCache().viewport(viewport);

    ghoul::opengl::TextureUnit unit;
    unit.activate();
    _program->setUniform(_uniformCache.galaxyTexture, unit);
    int currentTextureIndex = -1;

    for (std::unordered_map<int, PlaneAggregate>::reference pAMapItem : _planesMap)
    {
        // For planes with undefined textures references
        if (pAMapItem.first == 30) {
            continue;
        }

        // We only bind a new texture when it is needed
        if (currentTextureIndex != pAMapItem.first) {
            _textureMap[pAMapItem.first]->bind();
            currentTextureIndex = pAMapItem.first;
        }
        glBindVertexArray(pAMapItem.second.vao);
        glDrawArrays(GL_TRIANGLES, 0, 6 * pAMapItem.second.numberOfPlanes);
    }

    glBindVertexArray(0);
    _program->deactivate();

    // Restores OpenGL Rendering State
    global::renderEngine->openglStateCache().resetBlendState();
    global::renderEngine->openglStateCache().resetDepthState();
    global::renderEngine->openglStateCache().resetPolygonAndClippingState();
}

void RenderablePlanesCloud::renderLabels(const RenderData& data,
                                         const glm::dmat4& modelViewProjectionMatrix,
                                         const glm::dvec3& orthoRight,
                                         const glm::dvec3& orthoUp, float fadeInVariable)
{
    float scale = 0.f;
    switch (_unit) {
        case Meter:
            scale = 1.f;
            break;
        case Kilometer:
            scale = 1e3f;
            break;
        case Parsec:
            scale = static_cast<float>(PARSEC);
            break;
        case Kiloparsec:
            scale = static_cast<float>(1e3 * PARSEC);
            break;
        case Megaparsec:
            scale = static_cast<float>(1e6 * PARSEC);
            break;
        case Gigaparsec:
            scale = static_cast<float>(1e9 * PARSEC);
            break;
        case GigalightYears:
            scale = static_cast<float>(306391534.73091 * PARSEC);
            break;
    }

    glm::vec4 textColor = glm::vec4(
        glm::vec3(_textColor),
        _textOpacity * fadeInVariable
    );

    ghoul::fontrendering::FontRenderer::ProjectedLabelsInformation labelInfo;
    labelInfo.orthoRight = orthoRight;
    labelInfo.orthoUp = orthoUp;
    labelInfo.minSize = _textMinSize;
    labelInfo.maxSize = _textMaxSize;
    labelInfo.cameraPos = data.camera.positionVec3();
    labelInfo.cameraLookUp = data.camera.lookUpVectorWorldSpace();
    labelInfo.renderType = _renderOption;
    labelInfo.mvpMatrix = modelViewProjectionMatrix;
    labelInfo.scale = pow(10.f, _textSize);
    labelInfo.enableDepth = true;
    labelInfo.enableFalseDepth = false;

    for (const std::pair<glm::vec3, std::string>& pair : _labelData) {
        //glm::vec3 scaledPos(_transformationMatrix * glm::dvec4(pair.first, 1.0));
        glm::vec3 scaledPos(pair.first);
        scaledPos *= scale;
        ghoul::fontrendering::FontRenderer::defaultProjectionRenderer().render(
            *_font,
            scaledPos,
            pair.second,
            textColor,
            labelInfo
        );
    }
}

void RenderablePlanesCloud::render(const RenderData& data, RendererTasks&) {
    const double scale = unitToMeter(_unit);

    float fadeInVariable = 1.f;
    if (!_disableFadeInDistance) {
        float distCamera = static_cast<float>(glm::length(data.camera.positionVec3()));
        distCamera = static_cast<float>(distCamera / scale);
        const glm::vec2 fadeRange = _fadeInDistance;
        //const float a = 1.f / ((fadeRange.y - fadeRange.x) * scale);
        const float a = 1.f / ((fadeRange.y - fadeRange.x));
        const float b = -(fadeRange.x / (fadeRange.y - fadeRange.x));
        const float funcValue = a * distCamera + b;
        fadeInVariable *= funcValue > 1.f ? 1.f : funcValue;

        if (funcValue < 0.01f) {
            return;
        }
    }

    const glm::dmat4 modelMatrix =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) * // Translation
        glm::dmat4(data.modelTransform.rotation) *  // Spice rotation
        glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale));

    const glm::dmat4 modelViewMatrix = data.camera.combinedViewMatrix() * modelMatrix;
    const glm::mat4 projectionMatrix = data.camera.projectionMatrix();
    const glm::dmat4 modelViewProjectionMatrix = glm::dmat4(projectionMatrix) *
                                                 modelViewMatrix;

    const glm::dmat4 invMVPParts = glm::inverse(modelMatrix) *
                                   glm::inverse(data.camera.combinedViewMatrix()) *
                                   glm::inverse(glm::dmat4(projectionMatrix));
    const glm::dvec3 orthoRight = glm::normalize(
        glm::dvec3(invMVPParts * glm::dvec4(1.0, 0.0, 0.0, 0.0))
    );
    const glm::dvec3 orthoUp = glm::normalize(
        glm::dvec3(invMVPParts * glm::dvec4(0.0, 1.0, 0.0, 0.0))
    );

    if (_hasSpeckFile) {
        renderPlanes(data, modelViewMatrix, projectionMatrix, fadeInVariable);
    }

    if (_hasLabel) {
        renderLabels(
            data,
            modelViewProjectionMatrix,
            orthoRight,
            orthoUp,
            fadeInVariable
        );
    }
}

void RenderablePlanesCloud::update(const UpdateData&) {
    if (_dataIsDirty  && _hasSpeckFile) {
        deleteDataGPUAndCPU();
        createPlanes();
        _dataIsDirty = false;
    }

    if (_program->isDirty()) {
        _program->rebuildFromFile();
        ghoul::opengl::updateUniformLocations(*_program, _uniformCache, UniformNames);
    }
}

bool RenderablePlanesCloud::loadData() {
    bool success = false;
    if (_hasSpeckFile) {
        // I disabled the cache as it didn't work on Mac --- abock
        // std::string cachedFile = FileSys.cacheManager()->cachedFilename(
        //     _speckFile,
        //     ghoul::filesystem::CacheManager::Persistent::Yes
        // );

        // bool hasCachedFile = FileSys.fileExists(cachedFile);
        // if (hasCachedFile) {
        //     LINFO(
        //         "Cached file '" << cachedFile <<
        //         "' used for Speck file '" << _speckFile << "'"
        //     );

        //     success = loadCachedFile(cachedFile);
        //     if (!success) {
        //         FileSys.cacheManager()->removeCacheFile(_speckFile);
        //         // Intentional fall-through to the 'else' to generate the cache
        //         // file for the next run
        //     }
        // }
        // else
        // {
        //     LINFO("Cache for Speck file '" << _speckFile << "' not found");
        LINFO(fmt::format("Loading Speck file '{}'", _speckFile));

            success = readSpeckFile();
            if (!success) {
                return false;
            }

            // LINFO("Saving cache");
            //success &= saveCachedFile(cachedFile);
        // }
    }

    if (!_labelFile.empty()) {
        // I disabled the cache as it didn't work on Mac --- abock
        // std::string cachedFile = FileSys.cacheManager()->cachedFilename(
        //     _labelFile,
        //     ghoul::filesystem::CacheManager::Persistent::Yes
        // );
        // bool hasCachedFile = FileSys.fileExists(cachedFile);
        // if (hasCachedFile) {
        //     LINFO(
        //         "Cached file '" << cachedFile <<
        //         "' used for Label file '" << _labelFile << "'"
        //     );
        //
        //     success &= loadCachedFile(cachedFile);
        //     if (!success) {
        //         FileSys.cacheManager()->removeCacheFile(_labelFile);
        //         // Intentional fall-through to the 'else' to generate the cache
        //         // file for the next run
        //     }
        // }
        // else
        // {
        //     LINFO("Cache for Label file '" << _labelFile << "' not found");
            LINFO(fmt::format("Loading Label file '{}'", _labelFile));

            success &= readLabelFile();
            if (!success) {
                return false;
            }

        // }
    }

    return success;
}

bool RenderablePlanesCloud::loadTextures() {
    if (!_textureFileMap.empty()) {
        for (const std::pair<const int, std::string>& pair : _textureFileMap) {
            const auto& p = _textureMap.insert(std::make_pair(
                pair.first,
                ghoul::io::TextureReader::ref().loadTexture(pair.second)
            ));
            if (p.second) {
                LINFOC(
                    "RenderablePlanesCloud",
                    fmt::format("Loaded texture from '{}'", pair.second)
                );
                p.first->second->uploadTexture();
                p.first->second->setFilter(
                    ghoul::opengl::Texture::FilterMode::LinearMipMap
                );
                p.first->second->purgeFromRAM();
            }
        }
    }
    else {
        return false;
    }
    return true;
}

bool RenderablePlanesCloud::readSpeckFile() {
    std::ifstream file(_speckFile);
    if (!file.good()) {
        LERROR(fmt::format("Failed to open Speck file '{}'", _speckFile));
        return false;
    }

    _nValuesPerAstronomicalObject = 0;

    // The beginning of the speck file has a header that either contains comments
    // (signaled by a preceding '#') or information about the structure of the file
    // (signaled by the keywords 'datavar', 'texturevar', and 'texture')
    std::string line;
    while (true) {
        std::getline(file, line);

        // Guard against wrong line endings (copying files from Windows to Mac) causes
        // lines to have a final \r
        if (!line.empty() && line.back() == '\r') {
            line = line.substr(0, line.length() -1);
        }

        if (line.empty() || line[0] == '#') {
            continue;
        }

        if (line.substr(0, 7) != "datavar" &&
            line.substr(0, 10) != "texturevar" &&
            line.substr(0, 7) != "texture" &&
            line.substr(0, 10) != "polyorivar" &&
            line.substr(0, 10) != "maxcomment")
        {
            // Started reading data
            break;
        }

        if (line.substr(0, 7) == "datavar") {
            // datavar lines are structured as follows:
            // datavar # description
            // where # is the index of the data variable; so if we repeatedly overwrite
            // the 'nValues' variable with the latest index, we will end up with the total
            // number of values (+3 since X Y Z are not counted in the Speck file index)
            std::stringstream str(line);

            std::string dummy;
            str >> dummy; // command
            str >> _nValuesPerAstronomicalObject; // variable index
            dummy.clear();
            str >> dummy; // variable name

            // +3 because of the x, y and z at the begining of each line.
            _variableDataPositionMap.insert({ dummy, _nValuesPerAstronomicalObject + 3});

            if ((dummy == "orientation") || (dummy == "ori")) { // 3d vectors u and v
                // We want the number, but the index is 0 based
                _nValuesPerAstronomicalObject += 6;
            }
            else {
                // We want the number, but the index is 0 based
                _nValuesPerAstronomicalObject += 1;
            }
        }

        if (line.substr(0, 10) == "polyorivar") {
            _planeStartingIndexPos = 0;
            std::stringstream str(line);

            std::string dummy;
            str >> dummy; // command
            str >> _planeStartingIndexPos;
            _planeStartingIndexPos += 3; // 3 for xyz
        }

        if (line.substr(0, 10) == "texturevar") {
            _textureVariableIndex = 0;
            std::stringstream str(line);

            std::string dummy;
            str >> dummy; // command
            str >> _textureVariableIndex;
            _textureVariableIndex += 3; // 3 for xyz
        }

        if (line.substr(0, 8) == "texture ") {
            std::stringstream str(line);

            std::size_t found = line.find('-');

            int textureIndex = 0;

            std::string dummy;
            str >> dummy; // command

            if (found != std::string::npos) {
                std::string option; // Not being used right now.
                str >> option;
            }

            str >> textureIndex;
            std::string fileName;
            str >> fileName; // texture file name

            std::string fullPath = absPath(_texturesPath + '/' + fileName);
            std::string pngPath =
                ghoul::filesystem::File(fullPath).fullBaseName() + ".png";

            if (FileSys.fileExists(fullPath)) {
                _textureFileMap.insert({ textureIndex, fullPath });

            }
            else if (FileSys.fileExists(pngPath)) {
                _textureFileMap.insert({ textureIndex, pngPath });
            }
            else {
                LWARNING(fmt::format("Could not find image file {}", fileName));
                _textureFileMap.insert({ textureIndex, "" });
            }
        }
    }

    _nValuesPerAstronomicalObject += 3; // X Y Z are not counted in the Speck file indices

    do {

        // Guard against wrong line endings (copying files from Windows to Mac) causes
        // lines to have a final \r
        if (!line.empty() && line.back() == '\r') {
            line = line.substr(0, line.length() -1);
        }

        if (line.empty()) {
            std::getline(file, line);
            continue;
        }
        else if (line[0] == '#') {
            std::getline(file, line);
            continue;
        }

        std::stringstream str(line);

        glm::vec3 u(0.f);
        glm::vec3 v(0.f);

        std::vector<float> values(_nValuesPerAstronomicalObject);

        for (int i = 0; i < _nValuesPerAstronomicalObject; ++i) {
            str >> values[i];
            if ((i >= _planeStartingIndexPos) &&
                (i <= _planeStartingIndexPos + 6)) { // vectors u and v
                int index = i - _planeStartingIndexPos;
                switch (index) {
                    case 0:
                        u.x = values[i];
                        break;
                    case 1:
                        u.y = values[i];
                        break;
                    case 2:
                        u.z = values[i];
                        break;
                    case 3:
                        v.x = values[i];
                        break;
                    case 4:
                        v.y = values[i];
                        break;
                    case 5:
                        v.z = values[i];
                        break;
                }
            }
        }
        _fullData.insert(_fullData.end(), values.begin(), values.end());

        // reads new line
        std::getline(file, line);
    } while (!file.eof());

    return true;
}

bool RenderablePlanesCloud::readLabelFile() {
    std::ifstream file(_labelFile);
    if (!file.good()) {
        LERROR(fmt::format("Failed to open Label file '{}'", _labelFile));
        return false;
    }

    // The beginning of the speck file has a header that either contains comments
    // (signaled by a preceding '#') or information about the structure of the file
    // (signaled by the keywords 'datavar', 'texturevar', and 'texture')
    std::string line;
    while (true) {
        std::streampos position = file.tellg();
        std::getline(file, line);

        // Guard against wrong line endings (copying files from Windows to Mac) causes
        // lines to have a final \r
        if (!line.empty() && line.back() == '\r') {
            line = line.substr(0, line.length() -1);
        }

        if (line.empty() || line[0] == '#') {
            continue;
        }

        if (line.substr(0, 9) != "textcolor") {
            // we read a line that doesn't belong to the header, so we have to jump back
            // before the beginning of the current line
            file.seekg(position);
            continue;
        }

        if (line.substr(0, 9) == "textcolor") {
            // textcolor lines are structured as follows:
            // textcolor # description
            // where # is color text defined in configuration file
            std::stringstream str(line);

            // TODO: handle cases of labels with different colors
            break;
        }
    }

    do {
        std::vector<float> values(_nValuesPerAstronomicalObject);

        std::getline(file, line);

        // Guard against wrong line endings (copying files from Windows to Mac) causes
        // lines to have a final \r
        if (!line.empty() && line.back() == '\r') {
            line = line.substr(0, line.length() -1);
        }

        if (line.empty()) {
            continue;
        }

        std::stringstream str(line);

        glm::vec3 position = glm::vec3(0.f);
        for (int j = 0; j < 3; ++j) {
            str >> position[j];
        }

        std::string dummy;
        str >> dummy; // text keyword

        std::string label;
        str >> label;
        dummy.clear();

        while (str >> dummy) {
            label += " " + dummy;
            dummy.clear();
        }

        glm::vec3 transformedPos = glm::vec3(
            _transformationMatrix * glm::dvec4(position, 1.0)
        );
        _labelData.emplace_back(std::make_pair(transformedPos, label));

    } while (!file.eof());

    return true;
}

bool RenderablePlanesCloud::loadCachedFile(const std::string& file) {
    std::ifstream fileStream(file, std::ifstream::binary);
    if (fileStream.good()) {
        int8_t version = 0;
        fileStream.read(reinterpret_cast<char*>(&version), sizeof(int8_t));
        if (version != CurrentCacheVersion) {
            LINFO("The format of the cached file has changed: deleting old cache");
            fileStream.close();
            FileSys.deleteFile(file);
            return false;
        }

        int32_t nValues = 0;
        fileStream.read(reinterpret_cast<char*>(&nValues), sizeof(int32_t));
        fileStream.read(reinterpret_cast<char*>(
            &_nValuesPerAstronomicalObject),
            sizeof(int32_t)
        );

        _fullData.resize(nValues);
        fileStream.read(reinterpret_cast<char*>(&_fullData[0]),
            nValues * sizeof(_fullData[0]));

        bool success = fileStream.good();
        return success;
    }
    else {
        LERROR(fmt::format("Error opening file '{}' for loading cache file", file));
        return false;
    }
}

bool RenderablePlanesCloud::saveCachedFile(const std::string& file) const {
    std::ofstream fileStream(file, std::ofstream::binary);
    if (fileStream.good()) {
        fileStream.write(reinterpret_cast<const char*>(&CurrentCacheVersion),
            sizeof(int8_t));

        const int32_t nValues = static_cast<int32_t>(_fullData.size());
        if (nValues == 0) {
            LERROR("Error writing cache: No values were loaded");
            return false;
        }
        fileStream.write(reinterpret_cast<const char*>(&nValues), sizeof(int32_t));

        const int32_t nValuesPerAstronomicalObject = static_cast<int32_t>(
            _nValuesPerAstronomicalObject
        );
        fileStream.write(reinterpret_cast<const char*>(
            &nValuesPerAstronomicalObject),
            sizeof(int32_t)
        );

        const size_t nBytes = nValues * sizeof(_fullData[0]);
        fileStream.write(reinterpret_cast<const char*>(&_fullData[0]), nBytes);

        bool success = fileStream.good();
        return success;
    }
    else {
        LERROR(fmt::format("Error opening file '{}' for save cache file", file));
        return false;
    }
}

double RenderablePlanesCloud::unitToMeter(Unit unit) const {
    switch (_unit) {
        case Meter:          return 1.0;
        case Kilometer:      return 1e3;
        case Parsec:         return PARSEC;
        case Kiloparsec:     return 1000 * PARSEC;
        case Megaparsec:     return 1e6 * PARSEC;
        case Gigaparsec:     return 1e9 * PARSEC;
        case GigalightYears: return 306391534.73091 * PARSEC;
        default:             throw ghoul::MissingCaseException();
    }
}

void RenderablePlanesCloud::createPlanes() {
    if (_dataIsDirty && _hasSpeckFile) {
        const double scale = unitToMeter(_unit);

        LDEBUG("Creating planes...");
        float maxSize = 0.f;
        double maxRadius = 0.0;
        for (size_t p = 0; p < _fullData.size(); p += _nValuesPerAstronomicalObject) {
            const glm::vec4 transformedPos = glm::vec4(
                _transformationMatrix *
                glm::dvec4(_fullData[p + 0], _fullData[p + 1], _fullData[p + 2], 1.0)
            );

            const double r = glm::length(glm::dvec3(transformedPos) * scale);
            if (r > maxRadius) {
                maxRadius = r;
            }

            // Plane vectors u and v
            glm::vec4 u = glm::vec4(
                _transformationMatrix *
                glm::dvec4(
                    _fullData[p + _planeStartingIndexPos + 0],
                    _fullData[p + _planeStartingIndexPos + 1],
                    _fullData[p + _planeStartingIndexPos + 2],
                    1.f
                )
            );
            u /= 2.f;
            u.w = 0.f;

            glm::vec4 v = glm::vec4(
                _transformationMatrix *
                glm::dvec4(
                    _fullData[p + _planeStartingIndexPos + 3],
                    _fullData[p + _planeStartingIndexPos + 4],
                    _fullData[p + _planeStartingIndexPos + 5],
                    1.f
                )
            );
            v /= 2.f;
            v.w = 0.f;

            if (!_luminosityVar.empty()) {
                float lumS = _fullData[p + _variableDataPositionMap[_luminosityVar]] *
                             _sluminosity;
                u *= lumS;
                v *= lumS;
            }

            u *= _scaleFactor;
            v *= _scaleFactor;

            glm::vec4 vertex0 = transformedPos - u - v; // same as 3
            glm::vec4 vertex1 = transformedPos + u + v; // same as 5
            glm::vec4 vertex2 = transformedPos - u + v;
            glm::vec4 vertex4 = transformedPos + u - v;

            for (int i = 0; i < 3; ++i) {
                maxSize = std::max(maxSize, vertex0[i]);
                maxSize = std::max(maxSize, vertex1[i]);
                maxSize = std::max(maxSize, vertex2[i]);
                maxSize = std::max(maxSize, vertex4[i]);
            }

            vertex0 *= scale;
            vertex1 *= scale;
            vertex2 *= scale;
            vertex4 *= scale;

            GLfloat vertexData[] = {
                //  x          y          z       w    s    t
                vertex0.x, vertex0.y, vertex0.z, 1.f, 0.f, 0.f,
                vertex1.x, vertex1.y, vertex1.z, 1.f, 1.f, 1.f,
                vertex2.x, vertex2.y, vertex2.z, 1.f, 0.f, 1.f,
                vertex0.x, vertex0.y, vertex0.z, 1.f, 0.f, 0.f,
                vertex4.x, vertex4.y, vertex4.z, 1.f, 1.f, 0.f,
                vertex1.x, vertex1.y, vertex1.z, 1.f, 1.f, 1.f,
            };

            int textureIndex = static_cast<int>(_fullData[p + _textureVariableIndex]);
            std::unordered_map<int, PlaneAggregate>::iterator found =
                _planesMap.find(textureIndex);
            if (found != _planesMap.end()) {
                for (int i = 0; i < PLANES_VERTEX_DATA_SIZE; ++i) {
                    found->second.planesCoordinates.push_back(vertexData[i]);
                }
                found->second.numberOfPlanes++;
            }
            else {
                PlaneAggregate pA;
                pA.textureIndex = textureIndex;
                glGenVertexArrays(1, &pA.vao);
                glGenBuffers(1, &pA.vbo);
                pA.numberOfPlanes = 1;
                for (int i = 0; i < PLANES_VERTEX_DATA_SIZE; ++i) {
                    pA.planesCoordinates.push_back(vertexData[i]);
                }
                _planesMap.insert(std::pair<int, PlaneAggregate>(textureIndex, pA));
            }
        }

        // Send data to GPU
        for (const std::pair<const int, PlaneAggregate>& pAMapItem : _planesMap) {
            glBindVertexArray(pAMapItem.second.vao);
            glBindBuffer(GL_ARRAY_BUFFER, pAMapItem.second.vbo);
            glBufferData(
                GL_ARRAY_BUFFER,
                sizeof(GLfloat) * PLANES_VERTEX_DATA_SIZE *
                    pAMapItem.second.numberOfPlanes,
                pAMapItem.second.planesCoordinates.data(),
                GL_STATIC_DRAW
            );
            // in_position
            glEnableVertexAttribArray(0);
            glVertexAttribPointer(
                0,
                4,
                GL_FLOAT,
                GL_FALSE,
                sizeof(GLfloat) * 6,
                nullptr
            );

            // texture coords
            glEnableVertexAttribArray(1);
            glVertexAttribPointer(
                1,
                2,
                GL_FLOAT,
                GL_FALSE,
                sizeof(GLfloat) * 6,
                reinterpret_cast<GLvoid*>(sizeof(GLfloat) * 4)
            );

            glBindVertexArray(0);
        }

        _dataIsDirty = false;

        setBoundingSphere(maxRadius * _scaleFactor);
        _fadeInDistance.setMaxValue(glm::vec2(10.f * maxSize));
    }

    if (_hasLabel && _labelDataIsDirty) {
        _labelDataIsDirty = false;
    }
}

} // namespace openspace
