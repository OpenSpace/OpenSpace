/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <array>
#include <filesystem>
#include <fstream>
#include <optional>
#include <string>

namespace {
    constexpr std::string_view _loggerCat = "RenderablePlanesCloud";

    constexpr int PlanesVertexDataSize = 36;

    constexpr std::array<const char*, 4> UniformNames = {
        "modelViewProjectionTransform", "alphaValue", "fadeInValue", "galaxyTexture"
    };

    enum BlendMode {
        BlendModeNormal = 0,
        BlendModeAdditive
    };

    constexpr openspace::properties::Property::PropertyInfo ScaleFactorInfo = {
        "ScaleFactor",
        "Scale Factor",
        "This value is used as a multiplicative factor that is applied to the apparent "
        "size of each point",
        // @VISIBILITY(2.5)
        openspace::properties::Property::Visibility::User
    };

    static const openspace::properties::PropertyOwner::PropertyOwnerInfo LabelsInfo = {
        "Labels",
        "Labels",
        "The labels for the astronomical objects"
    };

    constexpr openspace::properties::Property::PropertyInfo DrawElementsInfo = {
        "DrawElements",
        "Draw Elements",
        "Enables/Disables the drawing of the astronomical objects",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo TransformationMatrixInfo = {
        "TransformationMatrix",
        "Transformation Matrix",
        "Transformation matrix to be applied to each astronomical object",
        openspace::properties::Property::Visibility::Developer
    };

    constexpr openspace::properties::Property::PropertyInfo BlendModeInfo = {
        "BlendMode",
        "Blending Mode",
        "This determines the blending mode that is applied to this plane",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo TexturePathInfo = {
        "TexturePath",
        "Texture Path",
        "This value specifies the path for the textures in disk",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo LuminosityInfo = {
        "Luminosity",
        "Luminosity variable",
        "Datavar variable to control the luminosity/size of the astronomical objects",
        // @VISIBILITY(2.67)
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo ScaleLuminosityInfo = {
        "ScaleLuminosity",
        "ScaleLuminosity variable",
        "Scaling control for the luminosity/size of the astronomical objects",
        // @VISIBILITY(2.67)
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo RenderOptionInfo = {
        "RenderOption",
        "Render Option",
        "Debug option for rendering of billboards and texts",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo FadeInDistancesInfo = {
        "FadeInDistances",
        "Fade-In Start and End Distances",
        "These values determine the initial and final distances from the center of "
        "our galaxy from which the astronomical object will start and end fading-in",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo DisableFadeInInfo = {
        "DisableFadeIn",
        "Disable Fade-in effect",
        "Enables/Disables the Fade-in effect",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo PlaneMinSizeInfo = {
        "PlaneMinSize",
        "Plane Min Size in Pixels",
        "The min size (in pixels) for the plane representing the astronomical object",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    struct [[codegen::Dictionary(RenderablePlanesCloud)]] Parameters {
        // The path to the SPECK file that contains information about the astronomical
        // object being rendered
        std::optional<std::string> file;

        // [[codegen::verbatim(ScaleFactorInfo.description)]]
        std::optional<float> scaleFactor;

        // [[codegen::verbatim(LabelsInfo.description)]]
        std::optional<ghoul::Dictionary> labels
            [[codegen::reference("labelscomponent")]];

        // [[codegen::verbatim(TransformationMatrixInfo.description)]]
        std::optional<glm::dmat4x4> transformationMatrix;

        enum class BlendMode {
            Normal,
            Additive
        };

        // [[codegen::verbatim(BlendModeInfo.description)]]
        std::optional<BlendMode> blendMode;

        enum class [[codegen::map(openspace::DistanceUnit)]] Unit {
            Meter [[codegen::key("m")]],
            Kilometer [[codegen::key("Km")]],
            Parsec [[codegen::key("pc")]],
            Kiloparsec [[codegen::key("Kpc")]],
            Megaparsec [[codegen::key("Mpc")]],
            Gigaparsec [[codegen::key("Gpc")]],
            Gigalightyear [[codegen::key("Gly")]]
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
    return codegen::doc<Parameters>("digitaluniverse_RenderablePlanesCloud");
}

RenderablePlanesCloud::RenderablePlanesCloud(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _scaleFactor(ScaleFactorInfo, 1.f, 0.f, 1000.f)
    , _drawElements(DrawElementsInfo, true)
    , _blendMode(BlendModeInfo, properties::OptionProperty::DisplayType::Dropdown)
    , _fadeInDistances(
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

    addProperty(Fadeable::_opacity);

    if (p.file.has_value()) {
        _speckFile = absPath(*p.file);
        _hasSpeckFile = true;
        _drawElements.onChange([this]() { _hasSpeckFile = !_hasSpeckFile; });
        addProperty(_drawElements);
    }

    // DEBUG:
    _renderOption.addOption(0, "Camera View Direction");
    _renderOption.addOption(1, "Camera Position Normal");
    _renderOption.addOption(2, "Screen center Position Normal");
    addProperty(_renderOption);
    //_renderOption = 1;

    if (p.unit.has_value()) {
        _unit = codegen::map<DistanceUnit>(*p.unit);
    }
    else {
        _unit = DistanceUnit::Meter;
    }

    _scaleFactor = p.scaleFactor.value_or(_scaleFactor);
    addProperty(_scaleFactor);
    _scaleFactor.onChange([this]() { _dataIsDirty = true; });

    if (p.labels.has_value()) {
        _labels = std::make_unique<LabelsComponent>(*p.labels);
        _hasLabels = true;
        addPropertySubOwner(_labels.get());
        // Fading of the labels should also depend on the fading of the renderable
        _labels->setParentFadeable(this);
    }

    _transformationMatrix = p.transformationMatrix.value_or(_transformationMatrix);

    _blendMode.addOptions({
        { BlendModeNormal, "Normal" },
        { BlendModeAdditive, "Additive" }
    });
    _blendMode.onChange([this]() {
        BlendMode m = static_cast<BlendMode>(_blendMode.value());
        switch (m) {
            case BlendModeNormal:
                setRenderBin(Renderable::RenderBin::Opaque);
                break;
            case BlendModeAdditive:
                setRenderBin(Renderable::RenderBin::PreDeferredTransparent);
                break;
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
        _fadeInDistances = *p.fadeInDistances;
        _disableFadeInDistance = false;
        _fadeInDistances.setViewOption(properties::Property::ViewOptions::MinMaxRange);
        addProperty(_fadeInDistances);
        addProperty(_disableFadeInDistance);
    }

    _planeMinSize = p.planeMinSize.value_or(_planeMinSize);

    if (p.planeMinSize.has_value()) {
        addProperty(_planeMinSize);
    }
}

bool RenderablePlanesCloud::isReady() const {
    bool isReady = _program && !_dataset.entries.empty();

    // If we have labels, they also need to be loaded
    if (_hasLabels) {
        isReady = isReady || _labels->isReady();
    }
    return isReady;
}

void RenderablePlanesCloud::initialize() {
    ZoneScoped;

    if (_hasSpeckFile && std::filesystem::is_regular_file(_speckFile)) {
        _dataset = dataloader::data::loadFileWithCache(_speckFile);
        if (_dataset.entries.empty()) {
            throw ghoul::RuntimeError("Error loading data");
        }
    }

    if (_hasLabels) {
        _labels->initialize();
    }
}

void RenderablePlanesCloud::initializeGL() {
    ZoneScoped;

    _program = DigitalUniverseModule::ProgramObjectManager.request(
        "RenderablePlanesCloud",
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
        "RenderablePlanesCloud",
        [](ghoul::opengl::ProgramObject* p) {
            global::renderEngine->removeRenderProgram(p);
        }
    );
}

void RenderablePlanesCloud::renderPlanes(const RenderData&,
                                         const glm::dmat4& modelViewTransform,
                                         const glm::dmat4& projectionTransform,
                                         const float fadeInVariable)
{
    glEnablei(GL_BLEND, 0);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE);
    //glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glDepthMask(false);

    _program->activate();

    glm::dmat4 modelViewProjectionTransform =
        glm::dmat4(projectionTransform) * modelViewTransform;
    _program->setUniform(
        _uniformCache.modelViewProjectionTransform,
        modelViewProjectionTransform
    );
    _program->setUniform(_uniformCache.alphaValue, opacity());
    _program->setUniform(_uniformCache.fadeInValue, fadeInVariable);

    glDisable(GL_CULL_FACE);

    GLint viewport[4];
    global::renderEngine->openglStateCache().viewport(viewport);

    ghoul::opengl::TextureUnit unit;
    unit.activate();
    _program->setUniform(_uniformCache.galaxyTexture, unit);
    int currentTextureIndex = -1;

    for (std::unordered_map<int, PlaneAggregate>::reference pAMapItem : _planesMap) {
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

void RenderablePlanesCloud::render(const RenderData& data, RendererTasks&) {
    const double scale = toMeter(_unit);

    float fadeInVariable = 1.f;
    if (!_disableFadeInDistance) {
        float distCamera = static_cast<float>(glm::length(data.camera.positionVec3()));
        distCamera = static_cast<float>(distCamera / scale);
        const glm::vec2 fadeRange = _fadeInDistances;
        //const float a = 1.f / ((fadeRange.y - fadeRange.x) * scale);
        const float a = 1.f / ((fadeRange.y - fadeRange.x));
        const float b = -(fadeRange.x / (fadeRange.y - fadeRange.x));
        const float funcValue = a * distCamera + b;
        fadeInVariable *= funcValue > 1.f ? 1.f : funcValue;

        if (funcValue < 0.01f) {
            return;
        }
    }

    const glm::dmat4 modelTransform = calcModelTransform(data);
    const glm::dmat4 modelViewTransform = calcModelViewTransform(data, modelTransform);
    const glm::mat4 projectionTransform = data.camera.projectionMatrix();

    if (_hasSpeckFile) {
        renderPlanes(data, modelViewTransform, projectionTransform, fadeInVariable);
    }

    if (_hasLabels) {
        const glm::dmat4 modelViewProjectionTransform =
            glm::dmat4(projectionTransform) * modelViewTransform;

        const glm::dmat4 invMVPParts = glm::inverse(modelTransform) *
            glm::inverse(data.camera.combinedViewMatrix()) *
            glm::inverse(glm::dmat4(projectionTransform));
        const glm::dvec3 orthoRight = glm::normalize(
            glm::dvec3(invMVPParts * glm::dvec4(1.0, 0.0, 0.0, 0.0))
        );
        const glm::dvec3 orthoUp = glm::normalize(
            glm::dvec3(invMVPParts * glm::dvec4(0.0, 1.0, 0.0, 0.0))
        );

        _labels->render(
            data,
            modelViewProjectionTransform,
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

void RenderablePlanesCloud::loadTextures() {
    for (const dataloader::Dataset::Texture& tex : _dataset.textures) {
        std::filesystem::path fullPath = absPath(_texturesPath / tex.file);
        std::filesystem::path pngPath = fullPath;
        pngPath.replace_extension(".png");

        std::filesystem::path path;
        if (std::filesystem::is_regular_file(fullPath)) {
            path = fullPath;
        }
        else if (std::filesystem::is_regular_file(pngPath)) {
            path = pngPath;
        }
        else {
            // We can't really recover from this as it would crash during rendering anyway
            throw ghoul::RuntimeError(std::format(
                "Could not find image file '{}'", tex.file
            ));
        }

        std::unique_ptr<ghoul::opengl::Texture> t =
            ghoul::io::TextureReader::ref().loadTexture(path, 2);

        if (t) {
            LINFOC("RenderablePlanesCloud", std::format("Loaded texture '{}'", path));
            t->uploadTexture();
            t->setFilter(ghoul::opengl::Texture::FilterMode::LinearMipMap);
            t->purgeFromRAM();
        }
        else {
            // Same here, we won't be able to recover from this nullptr
            throw ghoul::RuntimeError(std::format(
                "Could not find image file '{}'", tex.file
            ));
        }

        _textureMap.insert(std::pair(tex.index, std::move(t)));
    }
}

void RenderablePlanesCloud::createPlanes() {
    if (_dataIsDirty && _hasSpeckFile) {
        const int lumIdx = std::max(_dataset.index(_luminosityVar), 0);
        const double scale = toMeter(_unit);

        LDEBUG("Creating planes...");
        float maxSize = 0.f;
        double maxRadius = 0.0;
        for (const dataloader::Dataset::Entry& e : _dataset.entries) {
            const glm::vec4 transformedPos = glm::vec4(
                _transformationMatrix * glm::dvec4(e.position, 1.0)
            );

            const double r = glm::length(glm::dvec3(transformedPos) * scale);
            maxRadius = std::max(maxRadius, r);

            // Plane vectors u and v
            glm::vec4 u = glm::vec4(
                _transformationMatrix *
                glm::dvec4(
                    e.data[_dataset.orientationDataIndex + 0],
                    e.data[_dataset.orientationDataIndex + 1],
                    e.data[_dataset.orientationDataIndex + 2],
                    1.f
                )
            );
            u /= 2.f;
            u.w = 0.f;

            glm::vec4 v = glm::vec4(
                _transformationMatrix *
                glm::dvec4(
                    e.data[_dataset.orientationDataIndex + 3],
                    e.data[_dataset.orientationDataIndex + 4],
                    e.data[_dataset.orientationDataIndex + 5],
                    1.f
                )
            );
            v /= 2.f;
            v.w = 0.f;

            if (!_luminosityVar.empty()) {
                float lumS = e.data[lumIdx] * _sluminosity;
                u *= lumS;
                v *= lumS;
            }

            u *= _scaleFactor;
            v *= _scaleFactor;

            glm::vec4 vertex0 = transformedPos - u - v; // same as 3
            glm::vec4 vertex1 = transformedPos + u + v; // same as 5
            glm::vec4 vertex2 = transformedPos - u + v;
            glm::vec4 vertex4 = transformedPos + u - v;

            for (int i = 0; i < 3; i++) {
                maxSize = std::max(maxSize, vertex0[i]);
                maxSize = std::max(maxSize, vertex1[i]);
                maxSize = std::max(maxSize, vertex2[i]);
                maxSize = std::max(maxSize, vertex4[i]);
            }

            vertex0 = glm::vec4(glm::dvec4(vertex0) * scale);
            vertex1 = glm::vec4(glm::dvec4(vertex1) * scale);
            vertex2 = glm::vec4(glm::dvec4(vertex2) * scale);
            vertex4 = glm::vec4(glm::dvec4(vertex4) * scale);

            const std::array<GLfloat, 36> VertexData = {
                //  x          y          z       w    s    t
                vertex0.x, vertex0.y, vertex0.z, 1.f, 0.f, 0.f,
                vertex1.x, vertex1.y, vertex1.z, 1.f, 1.f, 1.f,
                vertex2.x, vertex2.y, vertex2.z, 1.f, 0.f, 1.f,
                vertex0.x, vertex0.y, vertex0.z, 1.f, 0.f, 0.f,
                vertex4.x, vertex4.y, vertex4.z, 1.f, 1.f, 0.f,
                vertex1.x, vertex1.y, vertex1.z, 1.f, 1.f, 1.f,
            };

            int textureIndex = static_cast<int>(e.data[_dataset.textureDataIndex]);
            std::unordered_map<int, PlaneAggregate>::iterator found =
                _planesMap.find(textureIndex);
            if (found != _planesMap.end()) {
                for (int i = 0; i < PlanesVertexDataSize; i++) {
                    found->second.planesCoordinates.push_back(VertexData[i]);
                }
                found->second.numberOfPlanes++;
            }
            else {
                PlaneAggregate pA;
                pA.textureIndex = textureIndex;
                glGenVertexArrays(1, &pA.vao);
                glGenBuffers(1, &pA.vbo);
                pA.numberOfPlanes = 1;
                for (int i = 0; i < PlanesVertexDataSize; i++) {
                    pA.planesCoordinates.push_back(VertexData[i]);
                }
                _planesMap.insert(std::pair(textureIndex, pA));
            }
        }

        // Send data to GPU
        for (const std::pair<const int, PlaneAggregate>& pAMapItem : _planesMap) {
            glBindVertexArray(pAMapItem.second.vao);
            glBindBuffer(GL_ARRAY_BUFFER, pAMapItem.second.vbo);
            glBufferData(
                GL_ARRAY_BUFFER,
                sizeof(GLfloat) * PlanesVertexDataSize * pAMapItem.second.numberOfPlanes,
                pAMapItem.second.planesCoordinates.data(),
                GL_STATIC_DRAW
            );
            // in_position
            glEnableVertexAttribArray(0);
            glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, 6 * sizeof(GLfloat), nullptr);

            // texture coords
            glEnableVertexAttribArray(1);
            glVertexAttribPointer(
                1,
                2,
                GL_FLOAT,
                GL_FALSE,
                6 * sizeof(GLfloat),
                reinterpret_cast<GLvoid*>(4 * sizeof(GLfloat))
            );

            glBindVertexArray(0);
        }

        _dataIsDirty = false;

        setBoundingSphere(maxRadius * _scaleFactor);
        _fadeInDistances.setMaxValue(glm::vec2(10.f * maxSize));
    }
}

} // namespace openspace
