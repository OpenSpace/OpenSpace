/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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
#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/templatefactory.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <array>
#include <fstream>
#include <locale>
#include <stdint.h>
#include <string>

namespace {
    constexpr const char* _loggerCat = "RenderablePoints";

    constexpr const std::array<const char*, 7> UniformNames = {
        "modelViewProjectionTransform", "color", "sides", "alphaValue", "scaleFactor",
        "spriteTexture", "hasColorMap"
    };

    constexpr const char* KeyFile = "File";
    constexpr const char* keyColor = "Color";
    constexpr const char* keyUnit = "Unit";
    constexpr const char* MeterUnit = "m";
    constexpr const char* KilometerUnit = "Km";
    constexpr const char* ParsecUnit = "pc";
    constexpr const char* KiloparsecUnit = "Kpc";
    constexpr const char* MegaparsecUnit = "Mpc";
    constexpr const char* GigaparsecUnit = "Gpc";
    constexpr const char* GigalightyearUnit = "Gly";

    constexpr int8_t CurrentCacheVersion = 1;
    constexpr double PARSEC = 0.308567756E17;

    constexpr openspace::properties::Property::PropertyInfo SpriteTextureInfo = {
        "Texture",
        "Point Sprite Texture",
        "The path to the texture that should be used as the point sprite."
    };

    constexpr openspace::properties::Property::PropertyInfo TransparencyInfo = {
        "Transparency",
        "Transparency",
        "This value is a multiplicative factor that is applied to the transparency of "
        "all points."
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
        "The path to the color map file of the astronomical onject."
    };
}  // namespace

namespace openspace {

documentation::Documentation RenderablePoints::Documentation() {
    using namespace documentation;
    return {
        "RenderablePoints",
        "digitaluniverse_renderablepoints",
        {
            {
                "Type",
                new StringEqualVerifier("RenderablePoints"),
                Optional::No
            },
            {
                KeyFile,
                new StringVerifier,
                Optional::No,
                "The path to the SPECK file that contains information about the "
                "astronomical object being rendered."
            },
            {
                keyColor,
                new Vector3Verifier<float>,
                Optional::No,
                "Astronomical Object Color (r,g,b)."
            },
            {
                SpriteTextureInfo.identifier,
                new StringVerifier,
                Optional::Yes,
                SpriteTextureInfo.description
            },
            {
                TransparencyInfo.identifier,
                new DoubleVerifier,
                Optional::No,
                TransparencyInfo.description
            },
            {
                ScaleFactorInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                ScaleFactorInfo.description
            },
            {
                ColorMapInfo.identifier,
                new StringVerifier,
                Optional::Yes,
                ColorMapInfo.description
            },

        }
    };
}


RenderablePoints::RenderablePoints(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _alphaValue(TransparencyInfo, 1.f, 0.f, 1.f)
    , _scaleFactor(ScaleFactorInfo, 1.f, 0.f, 64.f)
    , _pointColor(
        ColorInfo,
        glm::vec3(1.f, 0.4f, 0.2f),
        glm::vec3(0.f, 0.f, 0.f),
        glm::vec3(1.0f, 1.0f, 1.0f)
    )
    , _spriteTexturePath(SpriteTextureInfo)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "RenderablePoints"
    );

    _speckFile = absPath(dictionary.value<std::string>(KeyFile));

    if (dictionary.hasKey(keyUnit)) {
        const std::string& unit = dictionary.value<std::string>(keyUnit);
        if (unit == MeterUnit) {
            _unit = Meter;
        }
        else if (unit == KilometerUnit) {
            _unit = Kilometer;
        }
        else if (unit == ParsecUnit) {
            _unit = Parsec;
        }
        else if (unit == KiloparsecUnit) {
            _unit = Kiloparsec;
        }
        else if (unit == MegaparsecUnit) {
            _unit = Megaparsec;
        }
        else if (unit == GigaparsecUnit) {
            _unit = Gigaparsec;
        }
        else if (unit == GigalightyearUnit) {
            _unit = GigalightYears;
        }
        else {
            LWARNING("No unit given for RenderablePoints. Using meters as units.");
            _unit = Meter;
        }
    }

    if (dictionary.hasKey(keyColor)) {
        _pointColor = dictionary.value<glm::vec3>(keyColor);
    }
    addProperty(_pointColor);

    if (dictionary.hasKey(SpriteTextureInfo.identifier)) {
        _spriteTexturePath = absPath(dictionary.value<std::string>(
            SpriteTextureInfo.identifier
        ));
        _spriteTextureFile = std::make_unique<ghoul::filesystem::File>(
            _spriteTexturePath
        );

        _spriteTexturePath.onChange([&] { _spriteTextureIsDirty = true; });
        _spriteTextureFile->setCallback(
            [&](const ghoul::filesystem::File&) { _spriteTextureIsDirty = true; }
        );
        addProperty(_spriteTexturePath);

        _hasSpriteTexture = true;
    }

    if (dictionary.hasKey(ColorMapInfo.identifier)) {
        _colorMapFile = absPath(dictionary.value<std::string>(
            ColorMapInfo.identifier
        ));
        _hasColorMapFile = true;
    }

    if (dictionary.hasKey(TransparencyInfo.identifier)) {
        _alphaValue = static_cast<float>(
            dictionary.value<double>(TransparencyInfo.identifier)
        );
    }
    addProperty(_alphaValue);

    if (dictionary.hasKey(ScaleFactorInfo.identifier)) {
        _scaleFactor = static_cast<float>(
            dictionary.value<double>(ScaleFactorInfo.identifier)
        );
    }
    addProperty(_scaleFactor);
}

bool RenderablePoints::isReady() const {
    return (_program != nullptr) && (!_fullData.empty());
}

void RenderablePoints::initialize() {
    bool success = loadData();
    if (!success) {
        throw ghoul::RuntimeError("Error loading data");
    }
}

void RenderablePoints::initializeGL() {
    // OBS:  The ProgramObject name is later used to release the program as well, so the
    //       name parameter to requestProgramObject and the first parameter to
    //       buildRenderProgram has to be the same or an assertion will be thrown at the
    //       end of the program.

    if (_hasSpriteTexture) {
        _program = DigitalUniverseModule::ProgramObjectManager.request(
            "RenderablePoints Sprite",
            []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
                return global::renderEngine.buildRenderProgram(
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
                return global::renderEngine.buildRenderProgram(
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
            global::renderEngine.removeRenderProgram(p);
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
    _program->setUniform(_uniformCache.alphaValue, _alphaValue);
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
    const GLsizei nAstronomicalObjects = static_cast<GLsizei>(
        _fullData.size() / _nValuesPerAstronomicalObject
    );
    glDrawArrays(GL_POINTS, 0, nAstronomicalObjects);

    glDisable(GL_PROGRAM_POINT_SIZE);
    glBindVertexArray(0);
    _program->deactivate();

    glDepthMask(true);
}

void RenderablePoints::update(const UpdateData&) {
    if (_dataIsDirty) {
        LDEBUG("Regenerating data");

        createDataSlice();

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
            _slicedData.size() * sizeof(double),
            &_slicedData[0],
            GL_STATIC_DRAW
        );
        GLint positionAttrib = _program->attributeLocation("in_position");

        if (_hasColorMapFile) {

            // const size_t nAstronomicalObjects = _fullData.size() /
                                                        // _nValuesPerAstronomicalObject;
            // const size_t nValues = _slicedData.size() / nAstronomicalObjects;
            // GLsizei stride = static_cast<GLsizei>(sizeof(double) * nValues);

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
                sizeof(double) * 8,
                reinterpret_cast<void*>(sizeof(double) * 4)
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
        if (_spriteTexturePath.value() != "") {
            _spriteTexture = ghoul::io::TextureReader::ref().loadTexture(
                absPath(_spriteTexturePath)
            );
            if (_spriteTexture) {
                LDEBUG(fmt::format(
                    "Loaded texture from '{}'",absPath(_spriteTexturePath)
                ));
                _spriteTexture->uploadTexture();
            }
            _spriteTexture->setFilter(
                ghoul::opengl::Texture::FilterMode::AnisotropicMipMap
            );

            _spriteTextureFile = std::make_unique<ghoul::filesystem::File>(
                _spriteTexturePath
            );
            _spriteTextureFile->setCallback(
                [&](const ghoul::filesystem::File&) { _spriteTextureIsDirty = true; }
            );
        }
        _spriteTextureIsDirty = false;
    }
}

bool RenderablePoints::loadData() {
    std::string cachedFile = FileSys.cacheManager()->cachedFilename(
        _speckFile,
        ghoul::filesystem::CacheManager::Persistent::Yes
    );

    bool hasCachedFile = FileSys.fileExists(cachedFile);
    if (hasCachedFile) {
        LINFO(fmt::format(
            "Cached file '{}' used for Speck file '{}'",
            cachedFile, _speckFile
        ));

        bool success = loadCachedFile(cachedFile);
        if (success) {
            if (_hasColorMapFile) {
                success &= readColorMapFile();
            }
            return success;
        }
        else {
            FileSys.cacheManager()->removeCacheFile(_speckFile);
            // Intentional fall-through to the 'else' to generate the cache file for
            // the next run
        }
    }
    else {
        LINFO(fmt::format("Cache for Speck file '{}' not found", _speckFile));
    }
    LINFO(fmt::format("Loading Speck file '{}'", _speckFile));

    bool success = readSpeckFile();
    if (!success) {
        return false;
    }

    LINFO("Saving cache");
    success = saveCachedFile(cachedFile);

    if (_hasColorMapFile) {
        success &= readColorMapFile();
    }

    return success;
}

bool RenderablePoints::readSpeckFile() {
    std::ifstream file(_speckFile);
    if (!file.good()) {
        LERROR(fmt::format("Failed to open Speck file '{}'", _speckFile));
        return false;
    }

    _nValuesPerAstronomicalObject = 0;

    // The beginning of the speck file has a header that either contains comments
    // (signaled by a preceding '#') or information about the structure of the file
    // (signaled by the keywords 'datavar', 'texturevar', and 'texture')
    std::string line = "";
    while (true) {
        std::streampos position = file.tellg();
        std::getline(file, line);

        if (line[0] == '#' || line.empty()) {
            continue;
        }

        if (line.substr(0, 7) != "datavar" &&
            line.substr(0, 10) != "texturevar" &&
            line.substr(0, 7) != "texture")
        {
            // we read a line that doesn't belong to the header, so we have to jump
            // back before the beginning of the current line
            file.seekg(position);
            break;
        }

        if (line.substr(0, 7) == "datavar") {
            // datavar lines are structured as follows:
            // datavar # description
            // where # is the index of the data variable; so if we repeatedly
            // overwrite the 'nValues' variable with the latest index, we will end up
            // with the total number of values (+3 since X Y Z are not counted in the
            // Speck file index)
            std::stringstream str(line);

            std::string dummy;
            str >> dummy;
            str >> _nValuesPerAstronomicalObject;
            // We want the number, but the index is 0 based
            _nValuesPerAstronomicalObject += 1;
        }
    }

    // X Y Z are not counted in the Speck file indices
    _nValuesPerAstronomicalObject += 3;

    do {
        std::vector<float> values(_nValuesPerAstronomicalObject);

        std::getline(file, line);
        std::stringstream str(line);

        for (int i = 0; i < _nValuesPerAstronomicalObject; ++i) {
            str >> values[i];
        }

        _fullData.insert(_fullData.end(), values.begin(), values.end());
    } while (!file.eof());

    return true;
}

bool RenderablePoints::readColorMapFile() {
    std::ifstream file(_colorMapFile);
    if (!file.good()) {
        LERROR(fmt::format("Failed to open Color Map file '{}'", _colorMapFile));
        return false;
    }

    std::size_t numberOfColors = 0;

    // The beginning of the speck file has a header that either contains comments
    // (signaled by a preceding '#') or information about the structure of the file
    // (signaled by the keywords 'datavar', 'texturevar', and 'texture')
    std::string line = "";
    while (true) {
        // std::streampos position = file.tellg();
        std::getline(file, line);

        if (line[0] == '#' || line.empty()) {
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
            return false;
        }
    }

    for (size_t i = 0; i < numberOfColors; ++i) {
        std::getline(file, line);
        std::stringstream str(line);

        glm::vec4 color;
        for (int j = 0; j < 4; ++j) {
            str >> color[j];
        }

        _colorMapData.push_back(color);
    }

    return true;
}

bool RenderablePoints::loadCachedFile(const std::string& file) {
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
        fileStream.read(
            reinterpret_cast<char*>(&_nValuesPerAstronomicalObject),
            sizeof(int32_t)
        );

        _fullData.resize(nValues);
        fileStream.read(reinterpret_cast<char*>(
            &_fullData[0]),
            nValues * sizeof(_fullData[0])
        );

        const bool success = fileStream.good();
        return success;
    }
    else {
        LERROR(fmt::format(
            "Error opening file '{}' for loading cache file",
            file
        ));
        return false;
    }
}

bool RenderablePoints::saveCachedFile(const std::string& file) const {
    std::ofstream fileStream(file, std::ofstream::binary);
    if (fileStream.good()) {
        fileStream.write(
            reinterpret_cast<const char*>(&CurrentCacheVersion),
            sizeof(int8_t)
        );

        const int32_t nValues = static_cast<int32_t>(_fullData.size());
        if (nValues == 0) {
            LERROR("Error writing cache: No values were loaded");
            return false;
        }
        fileStream.write(reinterpret_cast<const char*>(&nValues), sizeof(int32_t));

        const int32_t nValuesPerAstronomicalObject = static_cast<int32_t>(
            _nValuesPerAstronomicalObject
        );
        fileStream.write(
            reinterpret_cast<const char*>(&nValuesPerAstronomicalObject),
            sizeof(int32_t)
        );

        const size_t nBytes = nValues * sizeof(_fullData[0]);
        fileStream.write(reinterpret_cast<const char*>(&_fullData[0]), nBytes);

        const bool success = fileStream.good();
        return success;
    }
    else {
        LERROR(fmt::format("Error opening file '{}' for save cache file", file));
        return false;
    }
}

void RenderablePoints::createDataSlice() {
    _slicedData.clear();
    if (_hasColorMapFile) {
        _slicedData.reserve(8 * (_fullData.size() / _nValuesPerAstronomicalObject));
    }
    else {
        _slicedData.reserve(4 * (_fullData.size()/_nValuesPerAstronomicalObject));
    }

    int colorIndex = 0;
    for (size_t i = 0; i < _fullData.size(); i += _nValuesPerAstronomicalObject) {
        glm::dvec3 p = glm::dvec3(
            _fullData[i + 0],
            _fullData[i + 1],
            _fullData[i + 2]
        );

        // Converting untis
        if (_unit == Kilometer) {
            p *= 1E3;
        }
        else if (_unit == Parsec) {
            p *= PARSEC;
        }
        else if (_unit == Kiloparsec) {
            p *= 1E3 * PARSEC;
        }
        else if (_unit == Megaparsec) {
            p *= 1E6 * PARSEC;
        }
        else if (_unit == Gigaparsec) {
            p *= 1E9 * PARSEC;
        }
        else if (_unit == GigalightYears) {
            p *= 306391534.73091 * PARSEC;
        }

        glm::dvec4 position(p, 1.0);

        if (_hasColorMapFile) {
            for (int j = 0; j < 4; ++j) {
                _slicedData.push_back(position[j]);
            }
            for (int j = 0; j < 4; ++j) {
                _slicedData.push_back(_colorMapData[colorIndex][j]);
            }
        }
        else {
            for (int j = 0; j < 4; ++j) {
                _slicedData.push_back(position[j]);
            }
        }

        colorIndex = (colorIndex == static_cast<int>(_colorMapData.size() - 1)) ?
            0 :
            colorIndex + 1;
    }
}

} // namespace openspace
