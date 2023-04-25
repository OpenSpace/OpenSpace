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

#include <modules/cosmiclife/rendering/renderablecosmicpoints.h>
#include <modules/cosmiclife/cosmiclifemodule.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/util/updatestructures.h>
#include <openspace/rendering/renderengine.h>
#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>
#include <ghoul/glm.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/crc32.h>
#include <ghoul/misc/templatefactory.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <glm/gtx/string_cast.hpp>
#include <array>
#include <cstdint>
#include <filesystem>
#include <fstream>
#include <locale>
#include <optional>
#include <string>

namespace {
    constexpr const char* _loggerCat = "RenderableCosmicPoints";
    constexpr const char* ProgramObjectName = "RenderableCosmicPoints";

    constexpr const std::array<const char*, 20> UniformNames = {
        "cameraViewProjectionMatrix", "modelMatrix", "cameraPosition", "cameraLookUp",
        "renderOption", "minBillboardSize", "maxBillboardSize",
        "correctionSizeEndDistance", "correctionSizeFactor", "color", "alphaValue",
        "scaleFactor", "up", "right", "screenSize", "spriteTexture",
        "hasColorMap", "enabledRectSizeControl", "hasDvarScaling", "frameColor"
    };

    enum RenderOption {
        ViewDirection = 0,
        PositionNormal
    };

    constexpr openspace::properties::Property::PropertyInfo SpriteTextureInfo = {
        "Texture",
        "Point Sprite Texture",
        "The path to the texture that should be used as the point sprite."
    };

    constexpr openspace::properties::Property::PropertyInfo FadeInfo = {
        "FadeInfo",
        "Fade Info",
        "This value is used to tell if the asset should be faded or not."
    };

    constexpr openspace::properties::Property::PropertyInfo FrameColorInfo = {
        "FrameColor",
        "Frame Color",
        "This value gives the color of the frame around each point."
    };

    constexpr openspace::properties::Property::PropertyInfo FadeThresholdInfo = {
        "FadeThresholdInfo",
        "Fade Threshold Info",
        "This value is used to tell if the asset should be faded or not."
    };

    constexpr openspace::properties::Property::PropertyInfo ScaleFactorInfo = {
        "ScaleFactor",
        "Scale Factor",
        "This value is used as a multiplicative factor that is applied to the apparent "
        "size of each point."
    };

    constexpr openspace::properties::Property::PropertyInfo ColorInfo = {
        "Color",
        "Color",
        "This value is used to define the color of the object."
    };

    constexpr openspace::properties::Property::PropertyInfo ColorMapInfo = {
        "ColorMap",
        "Color Map File",
        "The path to the color map file of the object."
    };

    constexpr openspace::properties::Property::PropertyInfo DrawElementsInfo = {
        "DrawElements",
        "Draw Elements",
        "Enables/Disables the drawing of the objects."
    };

    constexpr openspace::properties::Property::PropertyInfo ColorOptionInfo = {
        "ColorOption",
        "Color Option",
        "This value determines which paramenter is used for default color of the "
        "objects."
    };

    constexpr openspace::properties::Property::PropertyInfo OptionColorRangeInfo = {
        "OptionColorRange",
        "Option Color Range",
        "This value changes the range of values to be mapped with the current color map."
    };

    constexpr openspace::properties::Property::PropertyInfo SizeOptionInfo = {
        "SizeOption",
        "Size Option Variable",
        "This value determines which paramenter (datavar) is used for scaling "
        "of the objects."
    };

    constexpr openspace::properties::Property::PropertyInfo RenderOptionInfo = {
        "RenderOption",
        "Render Option",
        "Debug option for rendering of billboards and texts."
    };

    constexpr openspace::properties::Property::PropertyInfo PixelSizeControlInfo = {
        "EnablePixelSizeControl",
        "Enable Pixel Size Control",
        "Enable pixel size control for rectangular projections. If set to true, the "
        "billboard size is restricted by the min/max size in pixels property."
    };

    constexpr openspace::properties::Property::PropertyInfo BillboardMinMaxSizeInfo = {
        "BillboardMinMaxSize",
        "Billboard Min/Max Size in Pixels",
        "The minimum and maximum size (in pixels) for the billboard representing the "
        "object."
    };

    constexpr openspace::properties::Property::PropertyInfo
        CorrectionSizeEndDistanceInfo =
    {
        "CorrectionSizeEndDistance",
        "Distance in 10^X meters where correction size stops acting",
        "Distance in 10^X meters where correction size stops acting."
    };

    constexpr openspace::properties::Property::PropertyInfo CorrectionSizeFactorInfo = {
        "CorrectionSizeFactor",
        "Control variable for distance size",
        ""
    };

    constexpr openspace::properties::Property::PropertyInfo UseLinearFiltering = {
        "UseLinearFiltering",
        "Use Linear Filtering",
        "Determines whether the provided color map should be sampled nearest neighbor "
        "(=off) or linearly (=on)"
    };

    constexpr openspace::properties::Property::PropertyInfo SetRangeFromData = {
        "SetRangeFromData",
        "Set Data Range from Data",
        "Set the data range based on the available data"
    };

    struct [[codegen::Dictionary(RenderableCosmicPoints)]] Parameters {
        // The path to the SPECK file that contains information about the astronomical
        // object being rendered
        std::optional<std::string> file;

        // [[codegen::verbatim(ColorInfo.description)]]
        glm::vec3 color [[codegen::color()]];

        // [[codegen::verbatim(SpriteTextureInfo.description)]]
        std::optional<std::string> texture;

        // [[codegen::verbatim(FadeInfo.description)]]
        std::optional<bool> useFade;

        // [[codegen::verbatim(FadeInfo.description)]]
        std::optional<glm::vec3> frameColor;

        // [[codegen::verbatim(FadeThresholdInfo.description)]]
        std::optional<float> fadeThreshold;

        // [[codegen::verbatim(DrawElementsInfo.description)]]
        std::optional<bool> drawElements;

        enum class [[codegen::map(RenderOption)]] RenderOption {
            ViewDirection [[codegen::key("Camera View Direction")]],
            PositionNormal [[codegen::key("Camera Position Normal")]]
        };
        // [[codegen::verbatim(RenderOptionInfo.description)]]
        std::optional<RenderOption> renderOption;

        enum class [[codegen::map(openspace::DistanceUnit)]] Unit {
            Meter [[codegen::key("m")]],
            Kilometer [[codegen::key("Km")]],
            Parsec [[codegen::key("pc")]],
            Kiloparsec [[codegen::key("Kpc")]],
            Megaparsec [[codegen::key("Mpc")]],
            Gigaparsec [[codegen::key("Gpc")]],
            Gigalightyear [[codegen::key("Gly")]]
        };
        // The unit used for all distances. Must match the unit of any
        // distances/positions in the data files
        std::optional<Unit> unit;

        // [[codegen::verbatim(ScaleFactorInfo.description)]]
        std::optional<float> scaleFactor;

        // [[codegen::verbatim(ColorMapInfo.description)]]
        std::optional<std::string> colorMap;

        // Set a 1 to 1 relationship between the color index variable and the colormap
        // entrered value
        std::optional<bool> exactColorMap;

        // [[codegen::verbatim(ColorOptionInfo.description)]]
        std::optional<std::vector<std::string>> colorOption;

        // [[codegen::verbatim(SizeOptionInfo.description)]]
        std::optional<std::vector<std::string>> sizeOption;

        // This value determines the colormap ranges for the color parameters of the
        // astronomical objects
        std::optional<std::vector<glm::vec2>> colorRange;

        // Transformation matrix to be applied to each astronomical object
        std::optional<glm::dmat4x4> transformationMatrix;

        // [[codegen::verbatim(BillboardMinMaxSizeInfo.description)]]
        std::optional<glm::vec2> billboardMinMaxSize;

        // [[codegen::verbatim(CorrectionSizeEndDistanceInfo.description)]]
        std::optional<float> correctionSizeEndDistance;

        // [[codegen::verbatim(CorrectionSizeFactorInfo.description)]]
        std::optional<float> correctionSizeFactor;

        // [[codegen::verbatim(PixelSizeControlInfo.description)]]
        std::optional<bool> enablePixelSizeControl;

        // [[codegen::verbatim(UseLinearFiltering.description)]]
        std::optional<bool> useLinearFiltering;

        std::optional<std::string> uniqueSpecies;
    };
#include "renderablecosmicpoints_codegen.cpp"
}  // namespace

namespace openspace {

documentation::Documentation RenderableCosmicPoints::Documentation() {
    return codegen::doc<Parameters>("cosmiclife_renderablecosmicpoints");
}

RenderableCosmicPoints::RenderableCosmicPoints(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _scaleFactor(ScaleFactorInfo, 1.f, 0.f, 600.f)
    , _pointColor(ColorInfo, glm::vec3(1.f), glm::vec3(0.f), glm::vec3(1.f))
    , _frameColor(FrameColorInfo, glm::vec3(0.f), glm::vec3(0.f), glm::vec3(1.f))
    , _spriteTexturePath(SpriteTextureInfo)
    , _useFade(FadeInfo, false)
    , _fadeThreshold(FadeThresholdInfo, 10000.f)
    , _drawElements(DrawElementsInfo, true)
    , _pixelSizeControl(PixelSizeControlInfo, false)
    , _colorOption(ColorOptionInfo, properties::OptionProperty::DisplayType::Dropdown)
    , _optionColorRangeData(OptionColorRangeInfo, glm::vec2(0.f))
    , _datavarSizeOption(
        SizeOptionInfo,
        properties::OptionProperty::DisplayType::Dropdown
    )
    , _billboardMinMaxSize(
        BillboardMinMaxSizeInfo,
        glm::vec2(0.f, 400.f),
        glm::vec2(0.f),
        glm::vec2(1000.f)
    )
    , _correctionSizeEndDistance(CorrectionSizeEndDistanceInfo, 17.f, 12.f, 25.f)
    , _correctionSizeFactor(CorrectionSizeFactorInfo, 8.f, 0.f, 20.f)
    , _useLinearFiltering(UseLinearFiltering, false)
    , _setRangeFromData(SetRangeFromData)
    , _renderOption(RenderOptionInfo, properties::OptionProperty::DisplayType::Dropdown)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    if (p.file.has_value()) {
        _speckFile = absPath(*p.file).string();

        _hasSpeckFile = p.file.has_value();

        _drawElements = p.drawElements.value_or(_drawElements);
        _drawElements.onChange([&]() { _hasSpeckFile = !_hasSpeckFile; });
        addProperty(_drawElements);

        _uniqueSpecies = p.uniqueSpecies;
        _useFade = p.useFade.value_or(_useFade);
        _fadeThreshold = p.fadeThreshold.value_or(_fadeThreshold);
        _frameColor = p.frameColor.value_or(_frameColor);
    }
    
    _renderOption.addOption(RenderOption::ViewDirection, "Camera View Direction");
    _renderOption.addOption(RenderOption::PositionNormal, "Camera Position Normal");

    if (p.renderOption.has_value()) {
        _renderOption = codegen::map<RenderOption>(*p.renderOption);
    }
    else {
        _renderOption = RenderOption::ViewDirection;
    }
    addProperty(_renderOption);

    if (p.unit.has_value()) {
        _unit = codegen::map<DistanceUnit>(*p.unit);
    }
    else {
        _unit = DistanceUnit::Meter;
    }

    if (p.texture.has_value()) {
        _spriteTexturePath = absPath(*p.texture).string();
        _spriteTexturePath.onChange([&]() { _spriteTextureIsDirty = true; });
        addProperty(_spriteTexturePath);
    }
    _hasSpriteTexture = p.texture.has_value();

    if (p.colorMap.has_value()) {
        _colorMapFile = absPath(*p.colorMap).string();
        _hasColorMapFile = true;

        if (p.colorOption.has_value()) {
            std::vector<std::string> opts = *p.colorOption;
            for (size_t i = 0; i < opts.size(); ++i) {
                _colorOption.addOption(static_cast<int>(i), opts[i]);
                _optionConversionMap.insert({ static_cast<int>(i), opts[i] });
                _colorOptionString = opts[i];
            }
        }
        _colorOption.onChange([&]() {
            _dataIsDirty = true;
            const glm::vec2 colorRange = _colorRangeData[_colorOption.value()];
            _optionColorRangeData = colorRange;
            _colorOptionString = _optionConversionMap[_colorOption.value()];
        });
        addProperty(_colorOption);

        _colorRangeData = p.colorRange.value_or(_colorRangeData);
        if (!_colorRangeData.empty()) {
            _optionColorRangeData = _colorRangeData[_colorRangeData.size() - 1];
        }
        _optionColorRangeData.onChange([&]() {
            const glm::vec2 colorRange = _optionColorRangeData;
            _colorRangeData[_colorOption.value()] = colorRange;
            _dataIsDirty = true;
        });
        addProperty(_optionColorRangeData);

        _isColorMapExact = p.exactColorMap.value_or(_isColorMapExact);
    }
    else {
        _pointColor = p.color;
        _pointColor.setViewOption(properties::Property::ViewOptions::Color);
        addProperty(_pointColor);
    }

    addProperty(_opacity);

    _scaleFactor = p.scaleFactor.value_or(_scaleFactor);
    addProperty(_scaleFactor);

    if (p.sizeOption.has_value()) {
        std::vector<std::string> opts = *p.sizeOption;
        for (size_t i = 0; i < opts.size(); ++i) {
            _datavarSizeOption.addOption(static_cast<int>(i), opts[i]);
            _optionConversionSizeMap.insert({ static_cast<int>(i), opts[i] });
            _datavarSizeOptionString = opts[i];
        }

        _datavarSizeOption.onChange([&]() {
            _dataIsDirty = true;
            _datavarSizeOptionString = _optionConversionSizeMap[_datavarSizeOption];
        });
        addProperty(_datavarSizeOption);

        _hasDatavarSize = true;
    }

    _transformationMatrix = p.transformationMatrix.value_or(_transformationMatrix);

    _pixelSizeControl = p.enablePixelSizeControl.value_or(_pixelSizeControl);
    addProperty(_pixelSizeControl);

    _billboardMinMaxSize = p.billboardMinMaxSize.value_or(_billboardMinMaxSize);
    _billboardMinMaxSize.setViewOption(properties::Property::ViewOptions::MinMaxRange);
    addProperty(_billboardMinMaxSize);

    _correctionSizeEndDistance =
        p.correctionSizeEndDistance.value_or(_correctionSizeEndDistance);
    addProperty(_correctionSizeEndDistance);

    _correctionSizeFactor = p.correctionSizeFactor.value_or(_correctionSizeFactor);
    if (p.correctionSizeFactor.has_value()) {
        addProperty(_correctionSizeFactor);
    }

    _setRangeFromData.onChange([this]() {
        const int colorMapInUse =
            _hasColorMapFile ? _dataset.index(_colorOptionString) : 0;

        float minValue = std::numeric_limits<float>::max();
        float maxValue = -std::numeric_limits<float>::max();
        for (const speck::Dataset::Entry& e : _dataset.entries) {
            float color = e.data[colorMapInUse];
            minValue = std::min(minValue, color);
            maxValue = std::max(maxValue, color);
        }

        _optionColorRangeData = glm::vec2(minValue, maxValue);
    });
    addProperty(_setRangeFromData);

    _useLinearFiltering = p.useLinearFiltering.value_or(_useLinearFiltering);
    _useLinearFiltering.onChange([&]() { _dataIsDirty = true; });
    addProperty(_useLinearFiltering);

}

bool RenderableCosmicPoints::isReady() const {
   return (_program && (!_dataset.entries.empty()));
}

void RenderableCosmicPoints::initialize() {
    if (_hasSpeckFile) {
        _dataset = speck::data::loadFileWithCache(_speckFile);

        if (_uniqueSpecies.has_value()) {
            //Find all occurences in _dataset that correspond to specie store those instead
            std::vector<openspace::speck::Dataset::Entry> newdataset;
            std::copy_if(_dataset.entries.begin(), _dataset.entries.end(), std::back_inserter(newdataset), [this](const openspace::speck::Dataset::Entry& entry) {
                //function to find species
                return entry.comment == _uniqueSpecies.value();
            });

            _dataset.entries = std::move(newdataset);
        }
    }

    if (_hasColorMapFile) {
        _colorMap = speck::color::loadFileWithCache(_colorMapFile);
    }

    if (!_colorOptionString.empty() && (_colorRangeData.size() > 1)) {
        // Following DU behavior here. The last colormap variable
        // entry is the one selected by default.
        _colorOption.setValue(static_cast<int>(_colorRangeData.size() - 1));
    }

    setRenderBin(Renderable::RenderBin::PreDeferredTransparent);
}

void RenderableCosmicPoints::initializeGL() {

    _program = CosmicLifeModule::ProgramObjectManager.request(
        ProgramObjectName,
        []() {
            return global::renderEngine->buildRenderProgram(
                ProgramObjectName,
                absPath("${MODULE_COSMICLIFE}/shaders/points_vs.glsl"),
                absPath("${MODULE_COSMICLIFE}/shaders/points_fs.glsl"),
                absPath("${MODULE_COSMICLIFE}/shaders/points_gs.glsl")
            );
        }
    );

    ghoul::opengl::updateUniformLocations(*_program, _uniformCache, UniformNames);
}

// vao = vertex array object
// vbo = vertex buffer object
void RenderableCosmicPoints::deinitializeGL() {
    glDeleteBuffers(1, &_vbo);
    _vbo = 0;
    glDeleteVertexArrays(1, &_vao);
    _vao = 0;

    CosmicLifeModule::ProgramObjectManager.release(
        ProgramObjectName,
        [](ghoul::opengl::ProgramObject* p) {
            global::renderEngine->removeRenderProgram(p);
        }
    );
    _program = nullptr;

    CosmicLifeModule::TextureManager.release(_spriteTexture);
    _spriteTexture = nullptr;
}

void RenderableCosmicPoints::renderPoints(const RenderData& data,
                                                 const glm::dmat4& modelMatrix,
                                                 const glm::dvec3& orthoRight,
                                                 const glm::dvec3& orthoUp)
{
    glDepthMask(true);
    glEnablei(GL_BLEND, 0);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    _program->activate();

    _program->setUniform(
        "screenSize",
        glm::vec2(global::renderEngine->renderingResolution())
    );

    _program->setUniform(_uniformCache.cameraPos, data.camera.positionVec3());
    _program->setUniform(
        _uniformCache.cameraLookup,
        glm::vec3(data.camera.lookUpVectorWorldSpace())
    );
    _program->setUniform(_uniformCache.renderOption, _renderOption.value());
    _program->setUniform(_uniformCache.modelMatrix, modelMatrix);
    _program->setUniform(
        _uniformCache.cameraViewProjectionMatrix,
        glm::mat4(
            glm::dmat4(data.camera.projectionMatrix()) * data.camera.combinedViewMatrix()
        )
    );

    const float minBillboardSize = _billboardMinMaxSize.value().x; // in pixels
    const float maxBillboardSize = _billboardMinMaxSize.value().y; // in pixels

    _program->setUniform(_uniformCache.minBillboardSize, minBillboardSize);
    _program->setUniform(_uniformCache.maxBillboardSize, maxBillboardSize);
    _program->setUniform(_uniformCache.color, _pointColor);
    _program->setUniform(_uniformCache.alphaValue, opacity());
    _program->setUniform(_uniformCache.scaleFactor, _scaleFactor);
    _program->setUniform(_uniformCache.up, glm::vec3(orthoUp));
    _program->setUniform(_uniformCache.right, glm::vec3(orthoRight));

    _program->setUniform(
        _uniformCache.correctionSizeEndDistance,
        _correctionSizeEndDistance
    );
    _program->setUniform(_uniformCache.correctionSizeFactor, _correctionSizeFactor);

    _program->setUniform(_uniformCache.enabledRectSizeControl, _pixelSizeControl);

    _program->setUniform(_uniformCache.hasDvarScaling, _hasDatavarSize);

    _program->setUniform(_uniformCache.frameColor, _frameColor);

    GLint viewport[4];
    glGetIntegerv(GL_VIEWPORT, viewport);
    _program->setUniform(_uniformCache.screenSize, glm::vec2(viewport[2], viewport[3]));

    ghoul::opengl::TextureUnit textureUnit;
    textureUnit.activate(); 
    if (_spriteTexture) {
        _spriteTexture->bind();
    }
    _program->setUniform(_uniformCache.spriteTexture, textureUnit);
    _program->setUniform(_uniformCache.hasColormap, _hasColorMapFile);

    glBindVertexArray(_vao);
    glDrawArrays(GL_POINTS, 0, static_cast<GLsizei>(_dataset.entries.size()));
    glBindVertexArray(0);
    _program->deactivate();

    global::renderEngine->openglStateCache().resetBlendState();
    global::renderEngine->openglStateCache().resetDepthState();
}

float RenderableCosmicPoints::fadeObjectDependingOnDistance(const RenderData& data, const speck::Dataset::Entry& e, float thresholdDistance) {
    // Calculate distance between object and camera
    float distance = sqrt(pow((e.position.x*toMeter(_unit)) - data.camera.positionVec3().x, 2) + pow((e.position.y * toMeter(_unit)) - data.camera.positionVec3().y, 2) + pow((e.position.z * toMeter(_unit)) - data.camera.positionVec3().z, 2));
    float max_distance = 1000000;
    
    // Set alpha value based on distance
    float alpha = 1.0f;
    if (distance > thresholdDistance) {
        alpha = 1.0f - ((distance - thresholdDistance) / (max_distance - thresholdDistance));
    }

    // Set object alpha value
    return alpha;
}

void RenderableCosmicPoints::render(const RenderData& data, RendererTasks&) {

    updateRenderData(data);

    glm::dmat4 modelMatrix =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) * // Translation
        glm::dmat4(data.modelTransform.rotation) *  // Spice rotation
        glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale));

    glm::dmat4 modelViewMatrix = data.camera.combinedViewMatrix() * modelMatrix;
    glm::mat4 projectionMatrix = data.camera.projectionMatrix();

    glm::dmat4 modelViewProjectionMatrix = glm::dmat4(projectionMatrix) * modelViewMatrix;

    glm::dvec3 cameraViewDirectionWorld = -data.camera.viewDirectionWorldSpace();
    glm::dvec3 cameraUpDirectionWorld = data.camera.lookUpVectorWorldSpace();
    glm::dvec3 orthoRight = glm::normalize(
        glm::cross(cameraUpDirectionWorld, cameraViewDirectionWorld)
    );
    if (orthoRight == glm::dvec3(0.0)) {
        glm::dvec3 otherVector(
            cameraUpDirectionWorld.y,
            cameraUpDirectionWorld.x,
            cameraUpDirectionWorld.z
        );
        orthoRight = glm::normalize(glm::cross(otherVector, cameraViewDirectionWorld));
    }
    glm::dvec3 orthoUp = glm::normalize(glm::cross(cameraViewDirectionWorld, orthoRight));

    if (_hasSpeckFile && _drawElements) {
        renderPoints(data, modelMatrix, orthoRight, orthoUp);
    }
}

void RenderableCosmicPoints::updateRenderData(const RenderData& data) {

    if (_hasSpeckFile) {
        ZoneScopedN("Data dirty")
            TracyGpuZone("Data dirty")
            LDEBUG("Regenerating data");

        std::vector<float> slice = createDataSlice(data);

        int size = static_cast<int>(slice.size());

        if (_vao == 0) {
            glGenVertexArrays(1, &_vao);
            LDEBUG(fmt::format("Generating Vertex Array id '{}'", _vao));
        }
        if (_vbo == 0) {
            glGenBuffers(1, &_vbo);
            LDEBUG(fmt::format("Generating Vertex Buffer Object id '{}'", _vbo));
        }

        glBindVertexArray(_vao);
        glBindBuffer(GL_ARRAY_BUFFER, _vbo);
        glBufferData(GL_ARRAY_BUFFER, size * sizeof(float), slice.data(), GL_STATIC_DRAW);
        GLint positionAttrib = _program->attributeLocation("in_position");

        if (_hasColorMapFile && _hasDatavarSize) {
            glEnableVertexAttribArray(positionAttrib);
            glVertexAttribPointer(
                positionAttrib,
                4,
                GL_FLOAT,
                GL_FALSE,
                10 * sizeof(float),
                nullptr
            );

            GLint colorMapAttrib = _program->attributeLocation("in_colormap");
            glEnableVertexAttribArray(colorMapAttrib);
            glVertexAttribPointer(
                colorMapAttrib,
                4,
                GL_FLOAT,
                GL_FALSE,
                10 * sizeof(float),
                reinterpret_cast<void*>(4 * sizeof(float))
            );

            GLint dvarScalingAttrib = _program->attributeLocation("in_dvarScaling");
            glEnableVertexAttribArray(dvarScalingAttrib);
            glVertexAttribPointer(
                dvarScalingAttrib,
                1,
                GL_FLOAT,
                GL_FALSE,
                10 * sizeof(float),
                reinterpret_cast<void*>(8 * sizeof(float))
            );

            GLint opacityAttrib = _program->attributeLocation("in_opacity");
            glEnableVertexAttribArray(opacityAttrib);
            glVertexAttribPointer(
                opacityAttrib,
                1,
                GL_FLOAT,
                GL_FALSE,
                10 * sizeof(float),
                reinterpret_cast<void*>(9 * sizeof(float))
            );
        }
        //Does not have scaling
        else if (_hasColorMapFile) {
            glEnableVertexAttribArray(positionAttrib);
            glVertexAttribPointer(
                positionAttrib,
                4,
                GL_FLOAT,
                GL_FALSE,
                9 * sizeof(float),
                nullptr
            );

            GLint colorMapAttrib = _program->attributeLocation("in_colormap");
            glEnableVertexAttribArray(colorMapAttrib);
            glVertexAttribPointer(
                colorMapAttrib,
                4,
                GL_FLOAT,
                GL_FALSE,
                9 * sizeof(float),
                reinterpret_cast<void*>(4 * sizeof(float))
            );

            GLint opacityAttrib = _program->attributeLocation("in_opacity");
            glEnableVertexAttribArray(opacityAttrib);
            glVertexAttribPointer(
                opacityAttrib,
                1,
                GL_FLOAT,
                GL_FALSE,
                9 * sizeof(float),
                reinterpret_cast<void*>(8 * sizeof(float))
            );
        }
        //Doees not have colormap 
        else if (_hasDatavarSize) {
            glEnableVertexAttribArray(positionAttrib);
            glVertexAttribPointer(
                positionAttrib,
                4,
                GL_FLOAT,
                GL_FALSE,
                6 * sizeof(float),
                nullptr
            );

            GLint dvarScalingAttrib = _program->attributeLocation("in_dvarScaling");
            glEnableVertexAttribArray(dvarScalingAttrib);
            glVertexAttribPointer(
                dvarScalingAttrib,
                1,
                GL_FLOAT,
                GL_FALSE,
                6 * sizeof(float),
                reinterpret_cast<void*>(4 * sizeof(float))
            );


            GLint opacityAttrib = _program->attributeLocation("in_opacity");
            glEnableVertexAttribArray(opacityAttrib);
            glVertexAttribPointer(
                opacityAttrib,
                1,
                GL_FLOAT,
                GL_FALSE,
                6 * sizeof(float),
                reinterpret_cast<void*>(5 * sizeof(float))
            );
        }
        else {
            glEnableVertexAttribArray(positionAttrib);
            glVertexAttribPointer(
                positionAttrib,
                4,
                GL_FLOAT,
                GL_FALSE,
                5 * sizeof(float),
                nullptr
            );

            GLint opacityAttrib = _program->attributeLocation("in_opacity");
            glEnableVertexAttribArray(opacityAttrib);
            glVertexAttribPointer(
                opacityAttrib,
                1,
                GL_FLOAT,
                GL_FALSE,
                5 * sizeof(float),
                reinterpret_cast<void*>(4 * sizeof(float))
            );
        }

        glBindVertexArray(0);
        _dataIsDirty = false;
    }
}

void RenderableCosmicPoints::update(const UpdateData&) {

    if (_hasSpriteTexture && _spriteTextureIsDirty && !_spriteTexturePath.value().empty())
    {
        ZoneScopedN("Sprite texture")
        TracyGpuZone("Sprite texture")

        ghoul::opengl::Texture* texture = _spriteTexture;

        unsigned int hash = ghoul::hashCRC32File(_spriteTexturePath);

        _spriteTexture = CosmicLifeModule::TextureManager.request(
            std::to_string(hash),
            [path = _spriteTexturePath]() -> std::unique_ptr<ghoul::opengl::Texture> {
                std::filesystem::path p = absPath(path);
                LINFO(fmt::format("Loaded texture from {}", p));
                std::unique_ptr<ghoul::opengl::Texture> t =
                    ghoul::io::TextureReader::ref().loadTexture(p.string(), 2);
                t->uploadTexture();
                t->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);
                t->purgeFromRAM();
                return t;
            }
        );

        CosmicLifeModule::TextureManager.release(texture);
        _spriteTextureIsDirty = false;
    }
}


std::vector<float> RenderableCosmicPoints::createDataSlice(const RenderData& data) {

    if (_dataset.entries.empty()) {
        return std::vector<float>();
    }

    std::vector<float> result;
    if (_hasColorMapFile) {
        result.reserve(8 * _dataset.entries.size());
    }
    else {
        result.reserve(4 * _dataset.entries.size());
    }

    // what datavar in use for the index color
    int colorMapInUse = _hasColorMapFile ? _dataset.index(_colorOptionString) : 0;

    // what datavar in use for the size scaling (if present)
    int sizeScalingInUse =
        _hasDatavarSize ? _dataset.index(_datavarSizeOptionString) : -1;

    float minColorIdx = std::numeric_limits<float>::max();
    float maxColorIdx = -std::numeric_limits<float>::max();
    for (const speck::Dataset::Entry& e : _dataset.entries) {
        if (e.data.size() > 0) {
            float color = e.data[colorMapInUse];
            minColorIdx = std::min(color, minColorIdx);
            maxColorIdx = std::max(color, maxColorIdx);
        } else {
            minColorIdx = 0;
            maxColorIdx = 0;
        }
    }

    double maxRadius = 0.0;

    float biggestCoord = -1.f;
    for (const speck::Dataset::Entry& e : _dataset.entries) {
        glm::vec3 transformedPos = glm::vec3(_transformationMatrix * glm::vec4(
            e.position, 1.0
        ));

        float unitValue = 0.f;
        // (abock, 2022-01-02)  This is vestigial from a previous rewrite. I just want to
        // make it work for now and we can rewrite it properly later
        switch (_unit) {
            case DistanceUnit::Meter:
                unitValue = 0.f;
                break;
            case DistanceUnit::Kilometer:
                unitValue = 1.f;
                break;
            case DistanceUnit::Parsec:
                unitValue = 2;
                break;
            case DistanceUnit::Kiloparsec:
                unitValue = 3;
                break;
            case DistanceUnit::Megaparsec:
                unitValue = 4;
                break;
            case DistanceUnit::Gigaparsec:
                unitValue = 5;
                break;
            case DistanceUnit::Gigalightyear:
                unitValue = 6;
                break;
            default:
                throw ghoul::MissingCaseException();
        }

        glm::vec4 position(transformedPos, unitValue);

        const double unitMeter = toMeter(_unit);
        glm::dvec3 p = glm::dvec3(position) * unitMeter;
        const double r = glm::length(p);
        maxRadius = std::max(maxRadius, r);

        if (_hasColorMapFile) {
            for (int j = 0; j < 4; ++j) {
                result.push_back(position[j]);
            }
            biggestCoord = std::max(biggestCoord, glm::compMax(position));
            // Note: if exact colormap option is not selected, the first color and the
            // last color in the colormap file are the outliers colors.
            float variableColor = e.data[colorMapInUse];

            float cmax, cmin;
            if (_colorRangeData.empty()) {
                cmax = maxColorIdx; // Max value of datavar used for the index color
                cmin = minColorIdx; // Min value of datavar used for the index color
            }
            else {
                glm::vec2 currentColorRange = _colorRangeData[_colorOption.value()];
                cmax = currentColorRange.y;
                cmin = currentColorRange.x;
            }

            if (_isColorMapExact) {
                int colorIndex = static_cast<int>(variableColor + cmin);
                for (int j = 0; j < 4; ++j) {
                    result.push_back(_colorMap.entries[colorIndex][j]);
                }
            }
            else {
                if (_useLinearFiltering) {
                    float valueT = (variableColor - cmin) / (cmax - cmin); // in [0, 1)
                    valueT = std::clamp(valueT, 0.f, 1.f);

                    const float idx = valueT * (_colorMap.entries.size() - 1);
                    const int floorIdx = static_cast<int>(std::floor(idx));
                    const int ceilIdx = static_cast<int>(std::ceil(idx));

                    const glm::vec4 floorColor = _colorMap.entries[floorIdx];
                    const glm::vec4 ceilColor = _colorMap.entries[ceilIdx];

                    if (floorColor != ceilColor) {
                        const glm::vec4 c = floorColor + idx * (ceilColor - floorColor);
                        result.push_back(c.r);
                        result.push_back(c.g);
                        result.push_back(c.b);
                        result.push_back(c.a);
                    }
                    else {
                        result.push_back(floorColor.r);
                        result.push_back(floorColor.g);
                        result.push_back(floorColor.b);
                        result.push_back(floorColor.a);
                    }
                }
                else {
                    float ncmap = static_cast<float>(_colorMap.entries.size());
                    float normalization = ((cmax != cmin) && (ncmap > 2.f)) ?
                        (ncmap - 2.f) / (cmax - cmin) : 0;
                    int colorIndex = static_cast<int>(
                        (variableColor - cmin) * normalization + 1.f
                    );
                    colorIndex = colorIndex < 0 ? 0 : colorIndex;
                    colorIndex = colorIndex >= ncmap ?
                        static_cast<int>(ncmap - 1.f) : colorIndex;

                    for (int j = 0; j < 4; ++j) {
                        result.push_back(_colorMap.entries[colorIndex][j]);
                    }
                }
            }

            if (_hasDatavarSize) {
                result.push_back(e.data[sizeScalingInUse]);
            }
        }
        else if (_hasDatavarSize) {
            for (int j = 0; j < 4; ++j) {
                result.push_back(position[j]);
            }
            result.push_back(e.data[sizeScalingInUse]);
        }
        else {
            for (int j = 0; j < 4; ++j) {
                result.push_back(position[j]);
            }
        }

        if (_useFade) {
            float fade = fadeObjectDependingOnDistance(data, e, _fadeThreshold);
            result.push_back(fade);
        }
        else {
            result.push_back(1);
        }
        
    }

    setBoundingSphere(maxRadius);
    return result;
}

void RenderableCosmicPoints::renderToTexture(GLuint textureToRenderTo,
                                                GLuint textureWidth, GLuint textureHeight)
{
    LDEBUG("Rendering to Texture");

    // Saves initial Application's OpenGL State
    GLint defaultFBO;
    GLint viewport[4];
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFBO);
    glGetIntegerv(GL_VIEWPORT, viewport);

    GLuint textureFBO;
    glGenFramebuffers(1, &textureFBO);
    glBindFramebuffer(GL_FRAMEBUFFER, textureFBO);
    GLenum drawBuffers[1] = { GL_COLOR_ATTACHMENT0 };
    glDrawBuffers(1, drawBuffers);

    glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, textureToRenderTo, 0);

    glViewport(viewport[0], viewport[1], textureWidth, textureHeight);

    // Restores Applications' OpenGL State
    glBindFramebuffer(GL_FRAMEBUFFER, defaultFBO);
    glViewport(viewport[0], viewport[1], viewport[2], viewport[3]);

    glDeleteFramebuffers(1, &textureFBO);
}

} // namespace openspace


