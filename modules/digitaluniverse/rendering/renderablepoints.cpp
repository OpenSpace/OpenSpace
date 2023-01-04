/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#include <modules/digitaluniverse/rendering/renderablepoints.h>

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
    constexpr std::string_view _loggerCat = "RenderablePoints";

    constexpr std::array<const char*, 7> UniformNames = {
        "modelViewProjectionTransform", "color", "sides", "alphaValue", "scaleFactor",
        "spriteTexture", "hasColorMap"
    };

    constexpr openspace::properties::Property::PropertyInfo SpriteTextureInfo = {
        "Texture",
        "Point Sprite Texture",
        "The path to the texture that should be used as the point sprite"
    };

    constexpr openspace::properties::Property::PropertyInfo ScaleFactorInfo = {
        "ScaleFactor",
        "Scale Factor",
        "This value is used as a multiplicative factor that is applied to the apparent "
        "size of each point"
    };

    constexpr openspace::properties::Property::PropertyInfo ColorInfo = {
        "Color",
        "Color",
        "This value is used to define the color of the astronomical object"
    };

    constexpr openspace::properties::Property::PropertyInfo ColorMapInfo = {
        "ColorMap",
        "Color Map File",
        "The path to the color map file of the astronomical object"
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
    };
#include "renderablepoints_codegen.cpp"
}  // namespace

namespace openspace {

documentation::Documentation RenderablePoints::Documentation() {
    return codegen::doc<Parameters>("digitaluniverse_renderablepoints");
}

RenderablePoints::RenderablePoints(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _scaleFactor(ScaleFactorInfo, 1.f, 0.f, 64.f)
    , _pointColor(
        ColorInfo,
        glm::vec3(1.f, 0.4f, 0.2f),
        glm::vec3(0.f, 0.f, 0.f),
        glm::vec3(1.f, 1.f, 1.f)
    )
    , _spriteTexturePath(SpriteTextureInfo)
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
        _colorMapFile = absPath(*p.colorMap);
        _hasColorMapFile = true;
    }

    _scaleFactor = p.scaleFactor.value_or(_scaleFactor);
    addProperty(_scaleFactor);
}

bool RenderablePoints::isReady() const {
    return _program && (!_dataset.entries.empty());
}

void RenderablePoints::initialize() {
    ZoneScoped

    _dataset = speck::data::loadFileWithCache(_speckFile);

    if (_hasColorMapFile) {
         readColorMapFile();
    }
}

void RenderablePoints::initializeGL() {
    ZoneScoped

    if (_hasSpriteTexture) {
        _program = DigitalUniverseModule::ProgramObjectManager.request(
            "RenderablePoints Sprite",
            []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
                return global::renderEngine->buildRenderProgram(
                    "RenderablePoints Sprite",
                    absPath("${MODULE_DIGITALUNIVERSE}/shaders/points_vs.glsl"),
                    absPath("${MODULE_DIGITALUNIVERSE}/shaders/points_sprite_fs.glsl")
                );
            }
        );
    }
    else {
        _program = DigitalUniverseModule::ProgramObjectManager.request(
            "RenderablePoints",
            []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
                return global::renderEngine->buildRenderProgram(
                    "RenderablePoints",
                    absPath("${MODULE_DIGITALUNIVERSE}/shaders/points_vs.glsl"),
                    absPath("${MODULE_DIGITALUNIVERSE}/shaders/points_sprite_fs.glsl")
                );
            }
        );
    }
    ghoul::opengl::updateUniformLocations(*_program, _uniformCache, UniformNames);
}

void RenderablePoints::deinitializeGL() {
    glDeleteBuffers(1, &_vbo);
    _vbo = 0;
    glDeleteVertexArrays(1, &_vao);
    _vao = 0;

    DigitalUniverseModule::ProgramObjectManager.release(
        _program->name(),
        [](ghoul::opengl::ProgramObject* p) {
            global::renderEngine->removeRenderProgram(p);
        }
    );

    if (_hasSpriteTexture) {
        _spriteTexture = nullptr;
    }
}

void RenderablePoints::render(const RenderData& data, RendererTasks&) {
    glDepthMask(false);
    _program->activate();

    _program->setUniform(
        _uniformCache.modelViewProjectionTransform,
        glm::dmat4(data.camera.projectionMatrix()) *
            data.camera.combinedViewMatrix() * glm::dmat4(1.0)
    );

    _program->setUniform(_uniformCache.color, _pointColor);
    _program->setUniform(_uniformCache.sides, 4);
    _program->setUniform(_uniformCache.alphaValue, opacity());
    _program->setUniform(_uniformCache.scaleFactor, _scaleFactor);

    if (_hasSpriteTexture) {
        ghoul::opengl::TextureUnit spriteTextureUnit;
        spriteTextureUnit.activate();
        _spriteTexture->bind();
        _program->setUniform(_uniformCache.spriteTexture, spriteTextureUnit);
    }

    _program->setUniform(_uniformCache.hasColorMap, _hasColorMapFile);

    glEnable(GL_PROGRAM_POINT_SIZE);
    glBindVertexArray(_vao);
    glDrawArrays(GL_POINTS, 0, static_cast<GLsizei>(_dataset.entries.size()));

    glDisable(GL_PROGRAM_POINT_SIZE);
    glBindVertexArray(0);
    _program->deactivate();

    glDepthMask(true);
}

void RenderablePoints::update(const UpdateData&) {
    if (_dataIsDirty) {
        LDEBUG("Regenerating data");

        std::vector<double> slice = createDataSlice();

        if (_vao == 0) {
            glGenVertexArrays(1, &_vao);
        }
        if (_vbo == 0) {
            glGenBuffers(1, &_vbo);
        }

        glBindVertexArray(_vao);
        glBindBuffer(GL_ARRAY_BUFFER, _vbo);
        glBufferData(
            GL_ARRAY_BUFFER,
            slice.size() * sizeof(double),
            slice.data(),
            GL_STATIC_DRAW
        );
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

void RenderablePoints::readColorMapFile() {
    std::ifstream file(_colorMapFile);
    if (!file.good()) {
        throw ghoul::RuntimeError(fmt::format(
            "Failed to open Color Map file {}", _colorMapFile
        ));
    }

    std::size_t numberOfColors = 0;

    // The beginning of the speck file has a header that either contains comments
    // (signaled by a preceding '#') or information about the structure of the file
    // (signaled by the keywords 'datavar', 'texturevar', and 'texture')
    std::string line;
    while (true) {
        // std::streampos position = file.tellg();
        std::getline(file, line);

        if (line.empty() || line[0] == '#') {
            continue;
        }

        // Initial number of colors
        std::locale loc;
        if (std::isdigit(line[0], loc)) {
            std::string::size_type sz;
            numberOfColors = std::stoi(line, &sz);
            break;
        }
        else if (file.eof()) {
            throw ghoul::RuntimeError(fmt::format(
                "Failed to load colors from Color Map file {}", _colorMapFile
            ));
        }
    }

    for (size_t i = 0; i < numberOfColors; ++i) {
        std::getline(file, line);
        std::stringstream str(line);

        glm::vec4 color;
        str >> color.r >> color.g >> color.b >> color.a;

        _colorMapData.push_back(color);
    }
}

std::vector<double> RenderablePoints::createDataSlice() {
    std::vector<double> slice;
    if (_hasColorMapFile) {
        slice.reserve(8 * _dataset.entries.size());
    }
    else {
        slice.reserve(4 * _dataset.entries.size());
    }

    double maxRadius = 0.0;

    int colorIndex = 0;
    for (const speck::Dataset::Entry& e : _dataset.entries) {
        glm::dvec3 p = e.position;
        double scale = toMeter(_unit);
        p *= scale;

        const double r = glm::length(p);
        maxRadius = std::max(maxRadius, r);

        glm::dvec4 position(p, 1.0);

        if (_hasColorMapFile) {
            for (int j = 0; j < 4; ++j) {
                slice.push_back(position[j]);
            }
            for (int j = 0; j < 4; ++j) {
                slice.push_back(_colorMapData[colorIndex][j]);
            }
        }
        else {
            for (int j = 0; j < 4; ++j) {
                slice.push_back(position[j]);
            }
        }

        colorIndex = (colorIndex == static_cast<int>(_colorMapData.size() - 1)) ?
            0 :
            colorIndex + 1;
    }
    setBoundingSphere(maxRadius);

    return slice;
}

} // namespace openspace
