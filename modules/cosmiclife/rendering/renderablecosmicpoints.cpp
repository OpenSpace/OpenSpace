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
#include <openspace/rendering/renderengine.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/misc/templatefactory.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <array>
#include <filesystem>
#include <fstream>
#include <locale>
#include <cstdint>
#include <string>
#include <optional>

namespace {
    constexpr const char* _loggerCat = "RenderableCosmicPoints";

    constexpr const std::array<const char*, 6> UniformNames = {
        "modelViewProjectionTransform", "color", "alphaValue", "scaleFactor",
        "spriteTexture", "hasColorMap"
    };

    constexpr openspace::properties::Property::PropertyInfo SpriteTextureInfo = {
        "Texture",
        "Point Sprite Texture",
        "The path to the texture that should be used as the point sprite."
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
        "This value is used to define the color of the astronomical object."
    };

    constexpr openspace::properties::Property::PropertyInfo ColorMapInfo = {
        "ColorMap",
        "Color Map File",
        "The path to the color map file of the astronomical object."
    };

    constexpr openspace::properties::Property::PropertyInfo UseLinearFiltering = {
        "UseLinearFiltering",
        "Use Linear Filtering",
        "Determines whether the provided color map should be sampled nearest neighbor "
        "(=off) or linearly (=on)"
    };

    struct [[codegen::Dictionary(RenderablePoints)]] Parameters {
        // The path to the SPECK file that contains information about the astronomical
        // object being rendered
        std::string file;

        // Astronomical Object Color (r,g,b)
        glm::vec3 color [[codegen::color()]];

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

        // [[codegen::verbatim(SpriteTextureInfo.description)]]
        std::optional<std::string> texture;

        // [[codegen::verbatim(ScaleFactorInfo.description)]]
        std::optional<float> scaleFactor;

        // [[codegen::verbatim(ColorMapInfo.description)]]
        std::optional<std::string> colorMap;

        // [[codegen::verbatim(UseLinearFiltering.description)]]
        std::optional<bool> useLinearFiltering;
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
    , _pointColor(
        ColorInfo,
        glm::vec3(1.f),
        glm::vec3(0.f),
        glm::vec3(1.f)
    )
    , _spriteTexturePath(SpriteTextureInfo)
    , _useLinearFiltering(UseLinearFiltering, false)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    addProperty(_opacity);
    registerUpdateRenderBinFromOpacity();

    _speckFile = absPath(p.file);

    if (p.unit.has_value()) {
        _unit = codegen::map<DistanceUnit>(*p.unit);
    }
    else {
        _unit = DistanceUnit::Meter;
    }

    _pointColor = p.color;
    _pointColor.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_pointColor);

    if (p.texture.has_value()) {
        _spriteTexturePath = absPath(*p.texture).string();
        _spriteTextureFile = std::make_unique<ghoul::filesystem::File>(
            _spriteTexturePath.value()
        );

        _spriteTexturePath.onChange([this]() { _spriteTextureIsDirty = true; });
        _spriteTextureFile->setCallback([this]() { _spriteTextureIsDirty = true; });
        addProperty(_spriteTexturePath);

        _hasSpriteTexture = true;
    }

    if (p.colorMap.has_value()) {
        _colorMapFile = absPath(*p.colorMap).string();
        _hasColorMapFile = true;
    }

    _scaleFactor = p.scaleFactor.value_or(_scaleFactor);
    addProperty(_scaleFactor);

    _useLinearFiltering = p.useLinearFiltering.value_or(_useLinearFiltering);
    _useLinearFiltering.onChange([&]() { _dataIsDirty = true; });
    addProperty(_useLinearFiltering);
}

bool RenderableCosmicPoints::isReady() const {
    return _program && (!_dataset.entries.empty());
}

void RenderableCosmicPoints::initialize() {
    ZoneScoped

    _dataset = speck::data::loadFileWithCache(_speckFile);

    if (_hasColorMapFile) {
         //readColorMapFile();
        _colorMapData = speck::color::loadFileWithCache(_colorMapFile);
    }
}

void RenderableCosmicPoints::initializeGL() {
    ZoneScoped

    if (_hasSpriteTexture) {
        _program = CosmicLifeModule::ProgramObjectManager.request(
            "RenderableCosmicPoints Sprite",
            []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
                return global::renderEngine->buildRenderProgram(
                    "RenderableCosmicPoints Sprite",
                    absPath("${MODULE_COSMICLIFE}/shaders/points_vs.glsl"),
                    absPath("${MODULE_COSMICLIFE}/shaders/points_fs.glsl")
                );
            }
        );
    }
    else {
        _program = CosmicLifeModule::ProgramObjectManager.request(
            "RenderableCosmicPoints Sprite",
            []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
                return global::renderEngine->buildRenderProgram(
                    "RenderableCosmicPoints Sprite",
                    absPath("${MODULE_COSMICLIFE}/shaders/points_vs.glsl"),
                    absPath("${MODULE_COSMICLIFE}/shaders/points_fs.glsl")
                );
            }
        );
    }
    ghoul::opengl::updateUniformLocations(*_program, _uniformCache, UniformNames);
}

void RenderableCosmicPoints::deinitializeGL() {
    glDeleteBuffers(1, &_vbo);
    _vbo = 0;
    glDeleteVertexArrays(1, &_vao);
    _vao = 0;

    CosmicLifeModule::ProgramObjectManager.release(
        _program->name(),
        [](ghoul::opengl::ProgramObject* p) {
            global::renderEngine->removeRenderProgram(p);
        }
    );

    if (_hasSpriteTexture) {
        _spriteTexture = nullptr;
    }
}

void RenderableCosmicPoints::render(const RenderData& data, RendererTasks&) {
    glDepthMask(false);
    _program->activate();

    _program->setUniform(
        _uniformCache.modelViewProjectionTransform,
        glm::dmat4(data.camera.projectionMatrix()) *
            data.camera.combinedViewMatrix() * glm::dmat4(1.0)
    );

    _program->setUniform(_uniformCache.color, _pointColor);
    _program->setUniform(_uniformCache.alphaValue, _opacity);
    _program->setUniform(_uniformCache.scaleFactor, _scaleFactor);
    _program->setUniform(_uniformCache.hasColorMap, _hasColorMapFile);

    if (_hasSpriteTexture) {
        ghoul::opengl::TextureUnit spriteTextureUnit;
        spriteTextureUnit.activate();
        _spriteTexture->bind();
        _program->setUniform(_uniformCache.spriteTexture, spriteTextureUnit);
    }

    glEnable(GL_PROGRAM_POINT_SIZE);
    glBindVertexArray(_vao);
    glDrawArrays(GL_POINTS, 0, static_cast<GLsizei>(_dataset.entries.size()));

    glDisable(GL_PROGRAM_POINT_SIZE);
    glBindVertexArray(0);
    _program->deactivate();

    glDepthMask(true);
}

void RenderableCosmicPoints::update(const UpdateData&) {
    if (_dataIsDirty) {
        LDEBUG("Regenerating data");

        std::vector<double> slice = createDataSlice();
        int size = static_cast<int>(slice.size());

        if (_vao == 0) {
            glGenVertexArrays(1, &_vao);
        }
        if (_vbo == 0) {
            glGenBuffers(1, &_vbo);
        }

        glBindVertexArray(_vao);
        glBindBuffer(GL_ARRAY_BUFFER, _vbo);
        glBufferData(GL_ARRAY_BUFFER, size * sizeof(double), slice.data(), GL_STATIC_DRAW);
        GLint positionAttrib = _program->attributeLocation("in_position");

        if (_hasColorMapFile) {
            glEnableVertexAttribArray(positionAttrib);
            glVertexAttribLPointer(
                positionAttrib, 4, GL_DOUBLE, sizeof(double) * 8, nullptr
            );

            GLint colorMapAttrib = _program->attributeLocation("in_colormap");
            glEnableVertexAttribArray(colorMapAttrib);
            glVertexAttribLPointer(
                colorMapAttrib,
                4,
                GL_DOUBLE,
                8 * sizeof(double),
                reinterpret_cast<void*>(4 * sizeof(double))
            );
        }
        else {
            glEnableVertexAttribArray(positionAttrib);
            glVertexAttribLPointer(positionAttrib, 4, GL_DOUBLE, 0, nullptr);
        }

        glBindVertexArray(0);

        _dataIsDirty = false;
    }

    if (_hasSpriteTexture && _spriteTextureIsDirty) {
        LDEBUG("Reloading Sprite Texture");
        _spriteTexture = nullptr;
        if (!_spriteTexturePath.value().empty()) {
            _spriteTexture = ghoul::io::TextureReader::ref().loadTexture(
                absPath(_spriteTexturePath).string(),
                2
            );
            if (_spriteTexture) {
                LDEBUG(
                    fmt::format("Loaded texture from {}", absPath(_spriteTexturePath))
                );
                _spriteTexture->uploadTexture();
            }
            _spriteTexture->setFilter(
                ghoul::opengl::Texture::FilterMode::AnisotropicMipMap
            );

            _spriteTextureFile = std::make_unique<ghoul::filesystem::File>(
                _spriteTexturePath.value()
            );
            _spriteTextureFile->setCallback([this]() { _spriteTextureIsDirty = true; });
        }
        _spriteTextureIsDirty = false;
    }
}

std::vector<double> RenderableCosmicPoints::createDataSlice() {
    if (_dataset.entries.empty()) {
        return std::vector<double>();
    }

    std::vector<double> slice;
    if (_hasColorMapFile) {
        slice.reserve(8 * _dataset.entries.size()); // reserve: Increase the capacity of the vector
    }
    else {
        slice.reserve(4 * _dataset.entries.size());
    }

    // Finds the colorrange for the chosen column (static at the moment)
    float minColorIdx = std::numeric_limits<float>::max();
    float maxColorIdx = -std::numeric_limits<float>::max();
    for (const speck::Dataset::Entry& e : _dataset.entries) {
        if (e.data.size() > 0) {
            // TODO: make this dynamic
            float color = e.data[15]; // Detta get vilken column vi anpassar färgen efter
            minColorIdx = std::min(color, minColorIdx);
            maxColorIdx = std::max(color, maxColorIdx);
        }
        else {
            minColorIdx = 0;
            maxColorIdx = 0;
        }
    }

    double maxRadius = 0.0;

    int colorIndex = 0;
    for (const speck::Dataset::Entry& e : _dataset.entries) {
        glm::dvec3 p = e.position;
        double scale = toMeter(_unit);
        p *= scale;

        const double r = glm::length(p); // längden på vectorn p (positionen)
        maxRadius = std::max(maxRadius, r);

        glm::dvec4 position(p, 1.0);

        for (int j = 0; j < 4; ++j) {
            slice.push_back(position[j]);
        }

        if (_hasColorMapFile) {
            float colorColumn = e.data[15]; // Detta get vilken column vi anpassar färgen efter

            // TODO: make dynamic
            float cmax, cmin;
            cmax = maxColorIdx; // Max value of datavar used for the index color
            cmin = minColorIdx; // Min value of datavar used for the index color

            if (_useLinearFiltering) {
                float valueT = (colorColumn - cmin) / (cmax - cmin); // in [0, 1)
                valueT = std::clamp(valueT, 0.f, 1.f);

                const float idx = valueT * (_colorMapData.entries.size() - 1);
                const int floorIdx = static_cast<int>(std::floor(idx));
                const int ceilIdx = static_cast<int>(std::ceil(idx));

                const glm::vec4 floorColor = _colorMapData.entries[floorIdx];
                const glm::vec4 ceilColor = _colorMapData.entries[ceilIdx];

                if (floorColor != ceilColor) {
                    const glm::vec4 c = floorColor + idx * (ceilColor - floorColor);
                    slice.push_back(c.r);
                    slice.push_back(c.g);
                    slice.push_back(c.b);
                    slice.push_back(c.a);
                }
                else {
                    slice.push_back(floorColor.r);
                    slice.push_back(floorColor.g);
                    slice.push_back(floorColor.b);
                    slice.push_back(floorColor.a);
                }
            }
            else {
                float ncmap = static_cast<float>(_colorMapData.entries.size());
                float normalization = ((cmax != cmin) && (ncmap > 2.f)) ?
                    (ncmap - 2.f) / (cmax - cmin) : 0;
                int colorIndex = static_cast<int>(
                    (colorColumn - cmin) * normalization + 1.f
                    );
                colorIndex = colorIndex < 0 ? 0 : colorIndex;
                colorIndex = colorIndex >= ncmap ?
                    static_cast<int>(ncmap - 1.f) : colorIndex;

                for (int j = 0; j < 4; ++j) {
                    slice.push_back(_colorMapData.entries[colorIndex][j]);
                }
            }
        }

        // QUESTION
        colorIndex = (colorIndex == static_cast<int>(_colorMapData.entries.size() - 1)) ?
            0 :
            colorIndex + 1;
    }
    setBoundingSphere(maxRadius);

    return slice;
}

} // namespace openspace
