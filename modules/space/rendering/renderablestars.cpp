/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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

#include <modules/space/rendering/renderablestars.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/util/updatestructures.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/templatefactory.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <array>
#include <cstdint>
#include <fstream>

#include <type_traits>

namespace {
    constexpr const char* _loggerCat = "RenderableStars";

    constexpr const char* KeyFile = "File";
    constexpr const char* KeyStaticFilterValue = "StaticFilter";
    constexpr const char* KeyStaticFilterReplacement = "StaticFilterReplacement";

    constexpr const std::array<const char*, 13> UniformNames = {
        "view", "projection", "colorOption", "alphaValue", "scaleFactor",
        "minBillboardSize", "screenSize", "scaling", "psfTexture", "colorTexture",
        "otherDataTexture", "otherDataRange", "filterOutOfRange"
    };

    constexpr int8_t CurrentCacheVersion = 2;

    struct ColorVBOLayout {
        std::array<float, 4> position; // (x,y,z,e)
        float value;
        float luminance;
        float absoluteMagnitude;
    };

    struct VelocityVBOLayout {
        std::array<float, 4> position; // (x,y,z,e)
        float value;
        float luminance;
        float absoluteMagnitude;

        float vx; // v_x
        float vy; // v_y
        float vz; // v_z
    };

    struct SpeedVBOLayout {
        std::array<float, 4> position; // (x,y,z,e)
        float value;
        float luminance;
        float absoluteMagnitude;

        float speed;
    };

    struct OtherDataLayout {
        std::array<float, 4> position; // (x,y,z,e)
        float value;
        float luminance;
        float absoluteMagnitude;
    };

    constexpr openspace::properties::Property::PropertyInfo SpeckFileInfo = {
        "SpeckFile",
        "Speck File",
        "The speck file that is loaded to get the data for rendering these stars."
    };

    constexpr openspace::properties::Property::PropertyInfo PsfTextureInfo = {
        "Texture",
        "Point Spread Function Texture",
        "The path to the texture that should be used as a point spread function for the "
        "stars."
    };

    constexpr openspace::properties::Property::PropertyInfo ColorTextureInfo = {
        "ColorMap",
        "ColorBV Texture",
        "The path to the texture that is used to convert from the B-V value of the star "
        "to its color. The texture is used as a one dimensional lookup function."
    };

    constexpr openspace::properties::Property::PropertyInfo ColorOptionInfo = {
        "ColorOption",
        "Color Option",
        "This value determines which quantity is used for determining the color of the "
        "stars."
    };

    constexpr openspace::properties::Property::PropertyInfo TransparencyInfo = {
        "Transparency",
        "Transparency",
        "This value is a multiplicative factor that is applied to the transparency of "
        "all stars."
    };

    constexpr openspace::properties::Property::PropertyInfo ScaleFactorInfo = {
        "ScaleFactor",
        "Scale Factor",
        "This value is used as a multiplicative factor that is applied to the apparent "
        "size of each star."
    };

    constexpr openspace::properties::Property::PropertyInfo MinBillboardSizeInfo = {
        "MinBillboardSize",
        "Min Billboard Size",
        "This value is used as a lower limit on the size of stars that are rendered. Any "
        "stars that have a smaller apparent size will be discarded entirely."
    };

    constexpr openspace::properties::Property::PropertyInfo OtherDataOptionInfo = {
        "OtherData",
        "Other Data Column",
        "The index of the speck file data column that is used as the color input"
    };

    constexpr openspace::properties::Property::PropertyInfo OtherDataValueRangeInfo = {
        "OtherDataValueRange",
        "Range of the other data values",
        "This value is the min/max value range that is used to normalize the other data "
        "values so they can be used by the specified color map."
    };

    constexpr openspace::properties::Property::PropertyInfo OtherDataColorMapInfo = {
        "OtherDataColorMap",
        "Other Data Color Map",
        "The color map that is used if the 'Other Data' rendering method is selected"
    };

    constexpr openspace::properties::Property::PropertyInfo FilterOutOfRangeInfo = {
        "FilterOutOfRange",
        "Filter Out of Range",
        "Determines whether other data values outside the value range should be visible "
        "or filtered away"
    };
}  // namespace

namespace openspace {

documentation::Documentation RenderableStars::Documentation() {
    using namespace documentation;
    return {
        "RenderableStars",
        "space_renderablestars",
        {
            {
                "Type",
                new StringEqualVerifier("RenderableStars"),
                Optional::No
            },
            {
                KeyFile,
                new StringVerifier,
                Optional::No,
                "The path to the SPECK file that contains information about the stars "
                "being rendered."
            },
            {
                PsfTextureInfo.identifier,
                new StringVerifier,
                Optional::No,
                PsfTextureInfo.description
            },
            {
                ColorTextureInfo.identifier,
                new StringVerifier,
                Optional::No,
                ColorTextureInfo.description
            },
            {
                ColorOptionInfo.identifier,
                new StringInListVerifier({ "Color", "Velocity", "Speed", "Other Data" }),
                Optional::Yes,
                ColorOptionInfo.description
            },
            {
                OtherDataOptionInfo.identifier,
                new StringVerifier,
                Optional::Yes,
                OtherDataOptionInfo.description
            },
            {
                OtherDataColorMapInfo.identifier,
                new StringVerifier,
                Optional::Yes,
                OtherDataColorMapInfo.description
            },
            {
                FilterOutOfRangeInfo.identifier,
                new BoolVerifier,
                Optional::Yes,
                FilterOutOfRangeInfo.description
            },
            {
                KeyStaticFilterValue,
                new DoubleVerifier,
                Optional::Yes,
                "This value specifies a value that is always filtered out of the value "
                "ranges on loading. This can be used to trim the dataset's automatic "
                "value range."
            },
            {
                KeyStaticFilterReplacement,
                new DoubleVerifier,
                Optional::Yes,
                "This is the value that is used to replace statically filtered values. "
                "Setting this value only makes sense if 'StaticFilter' is 'true', as "
                "well."
            },
            {
                TransparencyInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                TransparencyInfo.description
            },
            {
                ScaleFactorInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                ScaleFactorInfo.description
            },
            {
                MinBillboardSizeInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                MinBillboardSizeInfo.description
            }
        }
    };
}

RenderableStars::RenderableStars(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _speckFile(SpeckFileInfo)
    , _pointSpreadFunctionTexturePath(PsfTextureInfo)
    , _colorTexturePath(ColorTextureInfo)
    , _colorOption(ColorOptionInfo, properties::OptionProperty::DisplayType::Dropdown)
    , _otherDataOption(
        OtherDataOptionInfo,
        properties::OptionProperty::DisplayType::Dropdown
    )
    , _otherDataColorMapPath(OtherDataColorMapInfo)
    , _otherDataRange(
        OtherDataValueRangeInfo,
        glm::vec2(0.f, 1.f),
        glm::vec2(-10.f, -10.f),
        glm::vec2(10.f, 10.f)
    )
    , _filterOutOfRange(FilterOutOfRangeInfo, false)
    , _alphaValue(TransparencyInfo, 1.f, 0.f, 1.f)
    , _scaleFactor(ScaleFactorInfo, 1.f, 0.f, 10.f)
    , _minBillboardSize(MinBillboardSizeInfo, 1.f, 1.f, 100.f)
{
    using File = ghoul::filesystem::File;

    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "RenderableStars"
    );

    _speckFile = absPath(dictionary.value<std::string>(KeyFile));
    _speckFile.onChange([&]() { _speckFileIsDirty = true; });
    addProperty(_speckFile);

    _pointSpreadFunctionTexturePath = absPath(dictionary.value<std::string>(
        PsfTextureInfo.identifier
    ));
    _pointSpreadFunctionFile = std::make_unique<File>(_pointSpreadFunctionTexturePath);

    _colorTexturePath = absPath(dictionary.value<std::string>(
        ColorTextureInfo.identifier
    ));

    if (dictionary.hasKey(OtherDataColorMapInfo.identifier)) {
        _otherDataColorMapPath = absPath(dictionary.value<std::string>(
            OtherDataColorMapInfo.identifier
        ));
    }
    _colorTextureFile = std::make_unique<File>(_colorTexturePath);


    _colorOption.addOptions({
        { ColorOption::Color, "Color" },
        { ColorOption::Velocity, "Velocity" },
        { ColorOption::Speed, "Speed" },
        { ColorOption::OtherData, "Other Data" }
    });
    if (dictionary.hasKey(ColorOptionInfo.identifier)) {
        const std::string colorOption = dictionary.value<std::string>(
            ColorOptionInfo.identifier
        );
        if (colorOption == "Color") {
            _colorOption = ColorOption::Color;
        }
        else if (colorOption == "Velocity") {
            _colorOption = ColorOption::Velocity;
        }
        else if (colorOption == "Speed") {
            _colorOption = ColorOption::Speed;
        }
        else {
            _colorOption = ColorOption::OtherData;
        }
    }
    _colorOption.onChange([&] { _dataIsDirty = true; });
    addProperty(_colorOption);

    _pointSpreadFunctionTexturePath.onChange([&] {
        _pointSpreadFunctionTextureIsDirty = true;
    });
    _pointSpreadFunctionFile->setCallback([&](const File&) {
        _pointSpreadFunctionTextureIsDirty = true;
    });
    addProperty(_pointSpreadFunctionTexturePath);

    _colorTexturePath.onChange([&] { _colorTextureIsDirty = true; });
    _colorTextureFile->setCallback([&](const File&) { _colorTextureIsDirty = true; }
    );
    addProperty(_colorTexturePath);

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

    if (dictionary.hasKey(MinBillboardSizeInfo.identifier)) {
        _minBillboardSize = static_cast<float>(
            dictionary.value<double>(MinBillboardSizeInfo.identifier)
        );
    }
    addProperty(_minBillboardSize);

    if (dictionary.hasKey(OtherDataOptionInfo.identifier)) {
        _queuedOtherData = dictionary.value<std::string>(OtherDataOptionInfo.identifier);
    }

    _otherDataOption.onChange([&]() { _dataIsDirty = true; });
    addProperty(_otherDataOption);

    addProperty(_otherDataRange);

    addProperty(_otherDataColorMapPath);
    _otherDataColorMapPath.onChange([&]() { _otherDataColorMapIsDirty = true; });

    if (dictionary.hasKey(KeyStaticFilterValue)) {
        _staticFilterValue = static_cast<float>(
            dictionary.value<double>(KeyStaticFilterValue)
        );
    }
    if (dictionary.hasKey(KeyStaticFilterReplacement)) {
        _staticFilterReplacementValue = static_cast<float>(
            dictionary.value<double>(KeyStaticFilterReplacement)
        );
    }

    addProperty(_filterOutOfRange);
}

RenderableStars::~RenderableStars() {} // NOLINT

bool RenderableStars::isReady() const {
    return _program != nullptr;
}

void RenderableStars::initializeGL() {
    _program = global::renderEngine.buildRenderProgram("Star",
        absPath("${MODULE_SPACE}/shaders/star_vs.glsl"),
        absPath("${MODULE_SPACE}/shaders/star_fs.glsl"),
        absPath("${MODULE_SPACE}/shaders/star_ge.glsl")
    );

    ghoul::opengl::updateUniformLocations(*_program, _uniformCache, UniformNames);

    loadData();

    if (!_queuedOtherData.empty()) {
        auto it = std::find(_dataNames.begin(), _dataNames.end(), _queuedOtherData);
        if (it == _dataNames.end()) {
            LERROR(fmt::format("Could not find other data column {}", _queuedOtherData));
        }
        else {
            _otherDataOption = static_cast<int>(std::distance(_dataNames.begin(), it));
            _queuedOtherData.clear();
        }
    }
    _speckFileIsDirty = false;
}

void RenderableStars::deinitializeGL() {
    glDeleteBuffers(1, &_vbo);
    _vbo = 0;
    glDeleteVertexArrays(1, &_vao);
    _vao = 0;

    _pointSpreadFunctionTexture = nullptr;
    _colorTexture = nullptr;

    if (_program) {
        global::renderEngine.removeRenderProgram(_program.get());
        _program = nullptr;
    }
}

void RenderableStars::render(const RenderData& data, RendererTasks&) {
    if (_fullData.empty()) {
        return;
    }

    glDepthMask(false);
    _program->activate();

    // @Check overwriting the scaling from the camera; error as parsec->meter conversion
    // is done twice? ---abock
    glm::vec2 scaling = glm::vec2(1, -19);

    _program->setUniform(_uniformCache.view, data.camera.viewMatrix());
    _program->setUniform(_uniformCache.projection, data.camera.projectionMatrix());

    _program->setUniform(_uniformCache.colorOption, _colorOption);
    _program->setUniform(_uniformCache.alphaValue, _alphaValue);
    _program->setUniform(_uniformCache.scaleFactor, _scaleFactor);
    _program->setUniform(_uniformCache.minBillboardSize, _minBillboardSize);
    _program->setUniform(
        _uniformCache.screenSize,
        glm::vec2(global::renderEngine.renderingResolution())
    );

    _program->setUniform("campos", glm::vec4(data.camera.positionVec3(), 1.f));
    _program->setUniform("objpos", glm::vec4(data.modelTransform.translation, 0.f));
    _program->setUniform("camrot", glm::mat4(data.camera.viewRotationMatrix()));
    _program->setUniform("scaling", glm::vec2(1.f, 0.f));
    _program->setUniform(_uniformCache.scaling, scaling);

    ghoul::opengl::TextureUnit psfUnit;
    psfUnit.activate();
    _pointSpreadFunctionTexture->bind();
    _program->setUniform(_uniformCache.psfTexture, psfUnit);

    ghoul::opengl::TextureUnit colorUnit;
    if (_colorTexture) {
        colorUnit.activate();
        _colorTexture->bind();
        _program->setUniform(_uniformCache.colorTexture, colorUnit);
    }

    ghoul::opengl::TextureUnit otherDataUnit;
    if (_colorOption == ColorOption::OtherData && _otherDataColorMapTexture) {
        otherDataUnit.activate();
        _otherDataColorMapTexture->bind();
        _program->setUniform(_uniformCache.otherDataTexture, otherDataUnit);
    }
    else {
        // We need to set the uniform to something, or the shader doesn't work
        _program->setUniform(_uniformCache.otherDataTexture, colorUnit);
    }
    // Same here, if we don't set this value, the rendering disappears even if we don't
    // use this color mode --- abock 2018-11-19
    _program->setUniform(_uniformCache.otherDataRange, _otherDataRange);
    _program->setUniform(_uniformCache.filterOutOfRange, _filterOutOfRange);

    glBindVertexArray(_vao);
    const GLsizei nStars = static_cast<GLsizei>(_fullData.size() / _nValuesPerStar);
    glDrawArrays(GL_POINTS, 0, nStars);

    glBindVertexArray(0);
    _program->deactivate();

    glDepthMask(true);
}

void RenderableStars::update(const UpdateData&) {
    if (_speckFileIsDirty) {
        loadData();
        _speckFileIsDirty = false;
        _dataIsDirty = true;
    }

    if (_fullData.empty()) {
        return;
    }

    if (_dataIsDirty) {
        const int value = _colorOption;
        LDEBUG("Regenerating data");

        createDataSlice(ColorOption(value));

        int size = static_cast<int>(_slicedData.size());

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
            size * sizeof(GLfloat),
            _slicedData.data(),
            GL_STATIC_DRAW
        );

        GLint positionAttrib = _program->attributeLocation("in_position");
        GLint brightnessDataAttrib = _program->attributeLocation("in_brightness");

        const size_t nStars = _fullData.size() / _nValuesPerStar;
        const size_t nValues = _slicedData.size() / nStars;

        GLsizei stride = static_cast<GLsizei>(sizeof(GLfloat) * nValues);

        glEnableVertexAttribArray(positionAttrib);
        glEnableVertexAttribArray(brightnessDataAttrib);
        const int colorOption = _colorOption;
        switch (colorOption) {
            case ColorOption::Color:
                glVertexAttribPointer(
                    positionAttrib,
                    4,
                    GL_FLOAT,
                    GL_FALSE,
                    stride,
                    nullptr // = offsetof(ColorVBOLayout, position)
                );
                glVertexAttribPointer(
                    brightnessDataAttrib,
                    3,
                    GL_FLOAT,
                    GL_FALSE,
                    stride,
                    reinterpret_cast<void*>(offsetof(ColorVBOLayout, value))
                );

                break;
            case ColorOption::Velocity:
            {
                glVertexAttribPointer(
                    positionAttrib,
                    4,
                    GL_FLOAT,
                    GL_FALSE,
                    stride,
                    nullptr // = offsetof(VelocityVBOLayout, position)
                );
                glVertexAttribPointer(
                    brightnessDataAttrib,
                    3,
                    GL_FLOAT,
                    GL_FALSE,
                    stride,
                    reinterpret_cast<void*>(offsetof(VelocityVBOLayout, value)) //NOLINT
                );

                GLint velocityAttrib = _program->attributeLocation("in_velocity");
                glEnableVertexAttribArray(velocityAttrib);
                glVertexAttribPointer(
                    velocityAttrib,
                    3,
                    GL_FLOAT,
                    GL_TRUE,
                    stride,
                    reinterpret_cast<void*>(offsetof(VelocityVBOLayout, vx)) // NOLINT
                );

                break;
            }
            case ColorOption::Speed:
            {
                glVertexAttribPointer(
                    positionAttrib,
                    4,
                    GL_FLOAT,
                    GL_FALSE,
                    stride,
                    nullptr // = offsetof(SpeedVBOLayout, position)
                );
                glVertexAttribPointer(
                    brightnessDataAttrib,
                    3,
                    GL_FLOAT,
                    GL_FALSE,
                    stride,
                    reinterpret_cast<void*>(offsetof(SpeedVBOLayout, value)) // NOLINT
                );

                GLint speedAttrib = _program->attributeLocation("in_speed");
                glEnableVertexAttribArray(speedAttrib);
                glVertexAttribPointer(
                    speedAttrib,
                    1,
                    GL_FLOAT,
                    GL_TRUE,
                    stride,
                    reinterpret_cast<void*>(offsetof(SpeedVBOLayout, speed)) // NOLINT
                );
                break;
            }
            case ColorOption::OtherData:
                glVertexAttribPointer(
                    positionAttrib,
                    4,
                    GL_FLOAT,
                    GL_FALSE,
                    stride,
                    nullptr // = offsetof(OtherDataLayout, position)
                );
                glVertexAttribPointer(
                    brightnessDataAttrib,
                    3,
                    GL_FLOAT,
                    GL_FALSE,
                    stride,
                    reinterpret_cast<void*>(offsetof(OtherDataLayout, value)) // NOLINT
                );
                break;
        }

        glBindBuffer(GL_ARRAY_BUFFER, 0);
        glBindVertexArray(0);

        _dataIsDirty = false;
    }

    if (_pointSpreadFunctionTextureIsDirty) {
        LDEBUG("Reloading Point Spread Function texture");
        _pointSpreadFunctionTexture = nullptr;
        if (!_pointSpreadFunctionTexturePath.value().empty()) {
            _pointSpreadFunctionTexture = ghoul::io::TextureReader::ref().loadTexture(
                absPath(_pointSpreadFunctionTexturePath)
            );

            if (_pointSpreadFunctionTexture) {
                LDEBUG(fmt::format(
                    "Loaded texture from '{}'",
                    absPath(_pointSpreadFunctionTexturePath)
                ));
                _pointSpreadFunctionTexture->uploadTexture();
            }
            _pointSpreadFunctionTexture->setFilter(
                ghoul::opengl::Texture::FilterMode::AnisotropicMipMap
            );

            _pointSpreadFunctionFile = std::make_unique<ghoul::filesystem::File>(
                _pointSpreadFunctionTexturePath
            );
            _pointSpreadFunctionFile->setCallback(
                [&](const ghoul::filesystem::File&) {
                    _pointSpreadFunctionTextureIsDirty = true;
                }
            );
        }
        _pointSpreadFunctionTextureIsDirty = false;
    }

    if (_colorTextureIsDirty) {
        LDEBUG("Reloading Color Texture");
        _colorTexture = nullptr;
        if (!_colorTexturePath.value().empty()) {
            _colorTexture = ghoul::io::TextureReader::ref().loadTexture(
                absPath(_colorTexturePath)
            );
            if (_colorTexture) {
                LDEBUG(fmt::format(
                    "Loaded texture from '{}'",
                    absPath(_colorTexturePath)
                ));
                _colorTexture->uploadTexture();
            }

            _colorTextureFile = std::make_unique<ghoul::filesystem::File>(
                _colorTexturePath
            );
            _colorTextureFile->setCallback(
                [&](const ghoul::filesystem::File&) { _colorTextureIsDirty = true; }
            );
        }
        _colorTextureIsDirty = false;
    }

    if (_otherDataColorMapIsDirty) {
        LDEBUG("Reloading Color Texture");
        _otherDataColorMapTexture = nullptr;
        if (!_otherDataColorMapPath.value().empty()) {
            _otherDataColorMapTexture = ghoul::io::TextureReader::ref().loadTexture(
                absPath(_otherDataColorMapPath)
            );
            if (_otherDataColorMapTexture) {
                LDEBUG(fmt::format(
                    "Loaded texture from '{}'",
                    absPath(_otherDataColorMapPath)
                ));
                _otherDataColorMapTexture->uploadTexture();
            }
        }
        _otherDataColorMapIsDirty = false;

    }

    if (_program->isDirty()) {
        _program->rebuildFromFile();
        ghoul::opengl::updateUniformLocations(*_program, _uniformCache, UniformNames);
    }
}

void RenderableStars::loadData() {
    std::string _file = _speckFile;
    if (!FileSys.fileExists(absPath(_file))) {
        return;
    }

    std::string cachedFile = FileSys.cacheManager()->cachedFilename(
        _file,
        ghoul::filesystem::CacheManager::Persistent::Yes
    );

    _nValuesPerStar = 0;
    _slicedData.clear();
    _fullData.clear();
    _dataNames.clear();

    bool hasCachedFile = FileSys.fileExists(cachedFile);
    if (hasCachedFile) {
        LINFO(fmt::format(
            "Cached file '{}' used for Speck file '{}'",
            cachedFile, _file
        ));

        bool success = loadCachedFile(cachedFile);
        if (success) {
            return;
        }
        else {
            FileSys.cacheManager()->removeCacheFile(_file);
            // Intentional fall-through to the 'else' computation to generate the cache
            // file for the next run
        }
    }
    else {
        LINFO(fmt::format("Cache for Speck file '{}' not found", _file));
    }
    LINFO(fmt::format("Loading Speck file '{}'", _file));

    readSpeckFile();

    LINFO("Saving cache");
    saveCachedFile(cachedFile);
}

void RenderableStars::readSpeckFile() {
    std::string _file = _speckFile;
    std::ifstream file(_file);
    if (!file.good()) {
        LERROR(fmt::format("Failed to open Speck file '{}'", _file));
        return;
    }

    // The beginning of the speck file has a header that either contains comments
    // (signaled by a preceding '#') or information about the structure of the file
    // (signaled by the keywords 'datavar', 'texturevar', and 'texture')
    std::string line;
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
            // we read a line that doesn't belong to the header, so we have to jump back
            // before the beginning of the current line
            file.seekg(position);
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
            str >> dummy;
            str >> _nValuesPerStar;

            std::string name;
            str >> name;

            _dataNames.push_back(name);
            _nValuesPerStar += 1; // We want the number, but the index is 0 based
        }
    }

    _otherDataOption.clearOptions();
    _otherDataOption.addOptions(_dataNames);

    _nValuesPerStar += 3; // X Y Z are not counted in the Speck file indices

    do {
        std::vector<float> values(_nValuesPerStar);

        std::getline(file, line);
        std::stringstream str(line);

        for (int i = 0; i < _nValuesPerStar; ++i) {
            str >> values[i];
        }
        bool nullArray = true;
        for (float v : values) {
            if (v != 0.0) {
                nullArray = false;
                break;
            }
        }
        if (!nullArray) {
            _fullData.insert(_fullData.end(), values.begin(), values.end());
        }
    } while (!file.eof());
}

bool RenderableStars::loadCachedFile(const std::string& file) {
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
        fileStream.read(reinterpret_cast<char*>(&_nValuesPerStar), sizeof(int32_t));

        for (int i = 0; i < _nValuesPerStar - 3; ++i) {
            uint16_t len;
            fileStream.read(reinterpret_cast<char*>(&len), sizeof(uint16_t));
            std::vector<char> buffer(len);
            fileStream.read(buffer.data(), len);
            std::string value(buffer.begin(), buffer.end());
            _dataNames.push_back(value);
        }
        _otherDataOption.addOptions(_dataNames);

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

void RenderableStars::saveCachedFile(const std::string& file) const {
    std::ofstream fileStream(file, std::ofstream::binary);
    if (!fileStream.good()) {
        LERROR(fmt::format("Error opening file '{}' for save cache file", file));
        return;
    }

    fileStream.write(reinterpret_cast<const char*>(&CurrentCacheVersion), sizeof(int8_t));

    int32_t nValues = static_cast<int32_t>(_fullData.size());
    if (nValues == 0) {
        throw ghoul::RuntimeError("Error writing cache: No values were loaded");
    }
    fileStream.write(reinterpret_cast<const char*>(&nValues), sizeof(int32_t));

    int32_t nValuesPerStar = static_cast<int32_t>(_nValuesPerStar);
    fileStream.write(reinterpret_cast<const char*>(&nValuesPerStar), sizeof(int32_t));

    // -3 as we don't want to save the xyz values that are in the beginning of the file
    for (int i = 0; i < _nValuesPerStar - 3; ++i) {
        uint16_t len = _dataNames[i].size();
        fileStream.write(reinterpret_cast<const char*>(&len), sizeof(uint16_t));
        fileStream.write(_dataNames[i].c_str(), len);
    }

    size_t nBytes = nValues * sizeof(_fullData[0]);
    fileStream.write(reinterpret_cast<const char*>(_fullData.data()), nBytes);
}

void RenderableStars::createDataSlice(ColorOption option) {
    _slicedData.clear();

    // This is only temporary until the scalegraph is in place ---abock
    float minDistance = std::numeric_limits<float>::max();
    float maxDistance = -std::numeric_limits<float>::max();

    for (size_t i = 0; i < _fullData.size(); i += _nValuesPerStar) {
        float distLy = _fullData[i + 6];
        //if (distLy < 20.f) {
        minDistance = std::min(minDistance, distLy);
        maxDistance = std::max(maxDistance, distLy);
        //}
    }

    _otherDataRange = glm::vec2(
        std::numeric_limits<float>::max(),
        -std::numeric_limits<float>::max()
    );

    for (size_t i = 0; i < _fullData.size(); i += _nValuesPerStar) {
        glm::vec3 p = glm::vec3(_fullData[i + 0], _fullData[i + 1], _fullData[i + 2]);

        // Convert parsecs -> meter
        psc position = psc(glm::vec4(p * 0.308567756f, 17));

        switch (option) {
            case ColorOption::Color:
            {
                union {
                    ColorVBOLayout value;
                    std::array<float, sizeof(ColorVBOLayout)> data;
                } layout = {};

                layout.value.position = { {
                        position[0], position[1], position[2], position[3]
                    } };

#ifdef USING_STELLAR_TEST_GRID
                layout.value.value = _fullData[i + 3];
                layout.value.luminance = _fullData[i + 3];
                layout.value.absoluteMagnitude = _fullData[i + 3];
#else
                layout.value.value = _fullData[i + 3];
                layout.value.luminance = _fullData[i + 4];
                layout.value.absoluteMagnitude = _fullData[i + 5];
#endif

                _slicedData.insert(_slicedData.end(),
                    layout.data.begin(),
                    layout.data.end());

                break;
            }
            case ColorOption::Velocity:
            {
                union {
                    VelocityVBOLayout value;
                    std::array<float, sizeof(VelocityVBOLayout)> data;
                } layout = {};

                layout.value.position = { {
                        position[0], position[1], position[2], position[3]
                    } };

                layout.value.value = _fullData[i + 3];
                layout.value.luminance = _fullData[i + 4];
                layout.value.absoluteMagnitude = _fullData[i + 5];

                layout.value.vx = _fullData[i + 12];
                layout.value.vy = _fullData[i + 13];
                layout.value.vz = _fullData[i + 14];

                _slicedData.insert(_slicedData.end(),
                    layout.data.begin(),
                    layout.data.end());
                break;
            }
            case ColorOption::Speed:
            {
                union {
                    SpeedVBOLayout value;
                    std::array<float, sizeof(SpeedVBOLayout)> data;
                } layout = {};

                layout.value.position = { {
                        position[0], position[1], position[2], position[3]
                    } };

                layout.value.value = _fullData[i + 3];
                layout.value.luminance = _fullData[i + 4];
                layout.value.absoluteMagnitude = _fullData[i + 5];

                layout.value.speed = _fullData[i + 15];

                _slicedData.insert(_slicedData.end(),
                    layout.data.begin(),
                    layout.data.end());
                break;
            }
            case ColorOption::OtherData:
            {
                union {
                    OtherDataLayout value;
                    std::array<float, sizeof(OtherDataLayout)> data;
                } layout = {};

                layout.value.position = {
                    { position[0], position[1], position[2], position[3] }
                };

                int index = _otherDataOption.value();
                layout.value.value = _fullData[i + index + 3];

                if (_staticFilterValue.has_value() &&
                    layout.value.value == _staticFilterValue)
                {
                    layout.value.value = _staticFilterReplacementValue;
                }

                glm::vec2 range = _otherDataRange.value();
                range.x = std::min(range.x, layout.value.value);
                range.y = std::max(range.y, layout.value.value);
                _otherDataRange = range;
                _otherDataRange.setMinValue(glm::vec2(range.x));
                _otherDataRange.setMaxValue(glm::vec2(range.y));

                layout.value.luminance = _fullData[i + 4];
                layout.value.absoluteMagnitude = _fullData[i + 5];

                _slicedData.insert(
                    _slicedData.end(),
                    layout.data.begin(),
                    layout.data.end()
                );

                break;
            }
        }
    }
}

} // namespace openspace
