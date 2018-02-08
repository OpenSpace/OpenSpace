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

#include <modules/gaiamission/rendering/renderablegaiastars.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/util/updatestructures.h>
#include <openspace/util/distanceconstants.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>

#include <modules/fitsfilereader/include/fitsfilereader.h>

#include <ghoul/filesystem/filesystem>
#include <ghoul/misc/templatefactory.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>

#include <array>
#include <fstream>
#include <stdint.h>

namespace {
    constexpr const char* _loggerCat = "RenderableGaiaStars";

    constexpr int8_t CurrentCacheVersion = 1;

    struct VBOLayout {
        std::array<float, 4> position; // (x,y,z) - TODO
        std::array<float, 3> velocity; // (x,y,z)
        float magnitude;
        float bvColor;
        float luminance;
        
    };


    static const openspace::properties::Property::PropertyInfo FitsFileInfo = {
        "File",
        "File Path",
        "The path to the fits file with data for the stars to be rendered."
    }; 
    
    static const openspace::properties::Property::PropertyInfo PsfTextureInfo = {
        "Texture",
        "Point Spread Function Texture",
        "The path to the texture that should be used as a point spread function for the "
        "stars."
    };

    static const openspace::properties::Property::PropertyInfo TransparencyInfo = {
        "Transparency",
        "Transparency",
        "This value is a multiplicative factor that is applied to the transparency of "
        "all stars."
    };

    static const openspace::properties::Property::PropertyInfo ScaleFactorInfo = {
        "ScaleFactor",
        "Scale Factor",
        "This value is used as a multiplicative factor that is applied to the apparent "
        "size of each star."
    };

    static const openspace::properties::Property::PropertyInfo MinBillboardSizeInfo = {
        "MinBillboardSize",
        "Min Billboard Size",
        "This value is used as a lower limit on the size of stars that are rendered. Any "
        "stars that have a smaller apparent size will be discarded entirely."
    };

    static const openspace::properties::Property::PropertyInfo ColorTextureInfo = {
        "ColorMap",
        "ColorBV Texture",
        "The path to the texture that is used to convert from the B-V value of the star "
        "to its color. The texture is used as a one dimensional lookup function."
    };

    static const openspace::properties::Property::PropertyInfo FirstRowInfo = {
        "FirstRow",
        "First Row to Read",
        "Defines the first row that will be read from the specified FITS file."
    };

    static const openspace::properties::Property::PropertyInfo LastRowInfo = {
        "LastRow",
        "Last Row to Read",
        "Defines the last row that will be read from the specified FITS file."
        "Has to be equal to or greater than FirstRow."
    };

    static const openspace::properties::Property::PropertyInfo ColumnNamesInfo = {
        "ColumnNames",
        "Column Names",
        "A list of strings with the names of all the columns that are to be "
        "read from the specified FITS file."
    };
}  // namespace

namespace openspace {

documentation::Documentation RenderableGaiaStars::Documentation() {
    using namespace documentation;
    return {
        "RenderableGaiaStars",
        "gaiamission_renderablegaiastars",
        {
            {
                "Type",
                new StringEqualVerifier("RenderableGaiaStars"),
                Optional::No
            },
            {
                FitsFileInfo.identifier,
                new StringVerifier,
                Optional::No,
                FitsFileInfo.description
            },
            {
                PsfTextureInfo.identifier,
                new StringVerifier,
                Optional::No,
                PsfTextureInfo.description
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
            },
            {
                ColorTextureInfo.identifier,
                new StringVerifier,
                Optional::No,
                ColorTextureInfo.description
            },
            {
                FirstRowInfo.identifier,
                new IntVerifier,
                Optional::No,
                FirstRowInfo.description
            },
            {
                LastRowInfo.identifier,
                new IntVerifier,
                Optional::No,
                LastRowInfo.description
            },
            {
                ColumnNamesInfo.identifier,
                new StringListVerifier,
                Optional::No,
                ColumnNamesInfo.description
            }
        }
    };
}

RenderableGaiaStars::RenderableGaiaStars(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _fitsFilePath(FitsFileInfo)
    , _fitsFile(nullptr)
    , _dataIsDirty(true)
    , _pointSpreadFunctionTexturePath(PsfTextureInfo)
    , _pointSpreadFunctionTexture(nullptr)
    , _pointSpreadFunctionTextureIsDirty(true)
    , _colorTexturePath(ColorTextureInfo)
    , _colorTexture(nullptr)
    , _colorTextureIsDirty(true)
    , _alphaValue(TransparencyInfo, 1.f, 0.f, 1.f)
    , _scaleFactor(ScaleFactorInfo, 1.f, 0.f, 10.f)
    , _minBillboardSize(MinBillboardSizeInfo, 1.f, 1.f, 100.f)
    , _firstRow(FirstRowInfo, 1, 1, 2539913)
    , _lastRow(LastRowInfo, 10000, 1, 2539913)
    , _columnNamesList(ColumnNamesInfo)
    , _program(nullptr)
    , _nValuesPerStar(0)
    , _vao(0)
    , _vbo(0)
{
    using File = ghoul::filesystem::File;

    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "RenderableGaiaStars"
    );

    _fitsFilePath = absPath(dictionary.value<std::string>(FitsFileInfo.identifier));
    _fitsFile = std::make_unique<File>(_fitsFilePath);

    _fitsFilePath.onChange(
        [&] { _dataIsDirty = true; }
    );
    _fitsFile->setCallback(
        [&](const File&) { _dataIsDirty = true; }
    );
    addProperty(_fitsFilePath);


    _pointSpreadFunctionTexturePath = absPath(dictionary.value<std::string>(
        PsfTextureInfo.identifier
    ));
    _pointSpreadFunctionFile = std::make_unique<File>(_pointSpreadFunctionTexturePath);

    _pointSpreadFunctionTexturePath.onChange(
        [&]{ _pointSpreadFunctionTextureIsDirty = true; }
    );
    _pointSpreadFunctionFile->setCallback(
        [&](const File&) { _pointSpreadFunctionTextureIsDirty = true; }
    );
    addProperty(_pointSpreadFunctionTexturePath);

    _colorTexturePath = absPath(dictionary.value<std::string>(
        ColorTextureInfo.identifier
        ));
    _colorTextureFile = std::make_unique<File>(_colorTexturePath);
    _colorTexturePath.onChange([&] { _colorTextureIsDirty = true; });
    _colorTextureFile->setCallback(
        [&](const File&) { _colorTextureIsDirty = true; }
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

    if (dictionary.hasKey(FirstRowInfo.identifier)) {
        _firstRow = static_cast<int>(
            dictionary.value<double>(FirstRowInfo.identifier)
        );
    }
    _firstRow.onChange([&] { _dataIsDirty = true; });
    addProperty(_firstRow);

    if (dictionary.hasKey(LastRowInfo.identifier)) {
        _lastRow = static_cast<int>(
            dictionary.value<double>(LastRowInfo.identifier)
        );
    }
    _lastRow.onChange([&] { _dataIsDirty = true; });
    addProperty(_lastRow);

    if (dictionary.hasKey(ColumnNamesInfo.identifier)) {        
        auto tmpDict = dictionary.value<ghoul::Dictionary>
            (ColumnNamesInfo.identifier);

        auto tmpKeys = tmpDict.keys();
        for (auto key : tmpKeys) {
            _columnNames.push_back(tmpDict.value<std::string>(key));
        }

        // Copy values to the StringListproperty to be shown in the Property list.
        _columnNamesList = _columnNames;
    }
    _columnNamesList.onChange([&] { _dataIsDirty = true; });
    addProperty(_columnNamesList);

    if (_firstRow > _lastRow) {
        throw ghoul::RuntimeError("User defined FirstRow is bigger than LastRow.");
    }

}

RenderableGaiaStars::~RenderableGaiaStars() {}

bool RenderableGaiaStars::isReady() const {
    return (_program != nullptr) && (!_fullData.empty());
}

void RenderableGaiaStars::initializeGL() {
    RenderEngine& renderEngine = OsEng.renderEngine();
    _program = renderEngine.buildRenderProgram("GaiaStar",
        //absPath("${MODULE_GAIAMISSION}/shaders/gaia_star_vs.glsl"),
        //absPath("${MODULE_GAIAMISSION}/shaders/gaia_star_fs.glsl"),
        //absPath("${MODULE_GAIAMISSION}/shaders/gaia_star_ge.glsl")
        absPath("${MODULE_GAIAMISSION}/shaders/star_vs.glsl"),
        absPath("${MODULE_GAIAMISSION}/shaders/star_fs.glsl"),
        absPath("${MODULE_GAIAMISSION}/shaders/star_ge.glsl")
        
    );

    _uniformCache.view = _program->uniformLocation("view");
    _uniformCache.projection = _program->uniformLocation("projection");
    _uniformCache.alphaValue = _program->uniformLocation("alphaValue");
    _uniformCache.scaleFactor = _program->uniformLocation("scaleFactor");
    _uniformCache.minBillboardSize = _program->uniformLocation("minBillboardSize");
    _uniformCache.screenSize = _program->uniformLocation("screenSize");
    _uniformCache.psfTexture = _program->uniformLocation("psfTexture");
    _uniformCache.time = _program->uniformLocation("time");
    _uniformCache.colorTexture = _program->uniformLocation("colorTexture");

    _uniformCache.scaling = _program->uniformLocation("scaling");

    bool success = loadData();
    if (!success) {
        throw ghoul::RuntimeError("Error loading FITS data");
    }
}

void RenderableGaiaStars::deinitializeGL() {
    glDeleteBuffers(1, &_vbo);
    _vbo = 0;
    glDeleteVertexArrays(1, &_vao);
    _vao = 0;

    _fitsFile = nullptr;
    _pointSpreadFunctionTexture = nullptr;
    _colorTexture = nullptr;

    RenderEngine& renderEngine = OsEng.renderEngine();
    if (_program) {
        renderEngine.removeRenderProgram(_program);
        _program = nullptr;
    }
}

void RenderableGaiaStars::render(const RenderData& data, RendererTasks&) {
    glDepthMask(false);
    _program->activate();

    // @Check overwriting the scaling from the camera; error as parsec->meter conversion
    // is done twice? ---abock
    glm::vec2 scaling = glm::vec2(1, -19);

    _program->setUniform(_uniformCache.view, data.camera.viewMatrix());
    _program->setUniform(_uniformCache.projection, data.camera.projectionMatrix());

    _program->setUniform(_uniformCache.alphaValue, _alphaValue);
    _program->setUniform(_uniformCache.scaleFactor, _scaleFactor);
    _program->setUniform(_uniformCache.minBillboardSize, _minBillboardSize);
    _program->setUniform(_uniformCache.screenSize,
        glm::vec2(OsEng.renderEngine().renderingResolution())
    );
    _program->setUniform(_uniformCache.time, static_cast<float>(data.time.j2000Seconds()));

    setPscUniforms(*_program.get(), data.camera, data.position);
    _program->setUniform(_uniformCache.scaling, scaling);

    ghoul::opengl::TextureUnit psfUnit;
    psfUnit.activate();
    _pointSpreadFunctionTexture->bind();
    _program->setUniform(_uniformCache.psfTexture, psfUnit);

    ghoul::opengl::TextureUnit colorUnit;
    colorUnit.activate();
    _colorTexture->bind();
    _program->setUniform(_uniformCache.colorTexture, colorUnit);

    glBindVertexArray(_vao);
    const GLsizei nStars = static_cast<GLsizei>(_fullData.size() / _nValuesPerStar);
    glDrawArrays(GL_POINTS, 0, nStars);

    //LINFO("Fulldata: " + std::to_string(_fullData.size()));
    //LINFO("_nValuesPerStar: " + std::to_string(_nValuesPerStar));
    //LINFO("Number of stars: " + std::to_string(nStars));

    glBindVertexArray(0);
    _program->deactivate();

    glDepthMask(true);
}

void RenderableGaiaStars::update(const UpdateData&) {
    if (_dataIsDirty) {
        LDEBUG("Regenerating data");

        // TODO (adaal): Reload fits file!

        createDataSlice();

        int size = static_cast<int>(_slicedData.size());

        if (_vao == 0) {
            glGenVertexArrays(1, &_vao);
            LDEBUG("Generating Vertex Array id '" << _vao << "'");
        }
        if (_vbo == 0) {
            glGenBuffers(1, &_vbo);
            LDEBUG("Generating Vertex Buffer Object id '" << _vbo << "'");
        }
        glBindVertexArray(_vao);
        glBindBuffer(GL_ARRAY_BUFFER, _vbo);
        glBufferData(
            GL_ARRAY_BUFFER,
            size * sizeof(GLfloat),
            &_slicedData[0],
            GL_STATIC_DRAW
        );

        GLint positionAttrib = _program->attributeLocation("in_position");
        GLint velocityAttrib = _program->attributeLocation("in_velocity");
        GLint brightnessDataAttrib = _program->attributeLocation("in_brightness");

        const size_t nStars = _fullData.size() / _nValuesPerStar;
        const size_t nValues = _slicedData.size() / nStars;

        LINFO("nStars: " + std::to_string(nStars) + " - nValues: " + std::to_string(nValues));

        GLsizei stride = static_cast<GLsizei>(sizeof(GLfloat) * nValues);

        glEnableVertexAttribArray(positionAttrib);
        glEnableVertexAttribArray(velocityAttrib);
        glEnableVertexAttribArray(brightnessDataAttrib);

        glVertexAttribPointer(
            positionAttrib,
            4, // TODO
            GL_FLOAT,
            GL_FALSE,
            stride,
            nullptr // = offsetof(VelocityVBOLayout, position)
        );
        glVertexAttribPointer(
            velocityAttrib,
            3,
            GL_FLOAT,
            GL_FALSE,
            stride,
            reinterpret_cast<void*>(offsetof(VBOLayout, velocity))
        );
        glVertexAttribPointer(
            brightnessDataAttrib,
            3, // TODO
            GL_FLOAT,
            GL_FALSE,
            stride,
            reinterpret_cast<void*>(offsetof(VBOLayout, magnitude))
        );

        glBindBuffer(GL_ARRAY_BUFFER, 0);
        glBindVertexArray(0);

        _dataIsDirty = false;
    }

    if (_pointSpreadFunctionTextureIsDirty) {
        LDEBUG("Reloading Point Spread Function texture");
        _pointSpreadFunctionTexture = nullptr;
        if (_pointSpreadFunctionTexturePath.value() != "") {
            _pointSpreadFunctionTexture = ghoul::io::TextureReader::ref().loadTexture(
                absPath(_pointSpreadFunctionTexturePath)
            );

            if (_pointSpreadFunctionTexture) {
                LDEBUG("Loaded texture from '" <<
                    absPath(_pointSpreadFunctionTexturePath) << "'"
               );
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
        if (_colorTexturePath.value() != "") {
            _colorTexture = ghoul::io::TextureReader::ref().loadTexture(
                absPath(_colorTexturePath)
            );
            if (_colorTexture) {
                LDEBUG("Loaded texture from '" << absPath(_colorTexturePath) << "'");
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

    if (_program->isDirty()) {
        _program->rebuildFromFile();

        _uniformCache.view = _program->uniformLocation("view");
        _uniformCache.projection = _program->uniformLocation("projection");
        _uniformCache.alphaValue = _program->uniformLocation("alphaValue");
        _uniformCache.scaleFactor = _program->uniformLocation("scaleFactor");
        _uniformCache.minBillboardSize = _program->uniformLocation("minBillboardSize");
        _uniformCache.screenSize = _program->uniformLocation("screenSize");
        _uniformCache.psfTexture = _program->uniformLocation("psfTexture");
        _uniformCache.time = _program->uniformLocation("time");
        _uniformCache.colorTexture = _program->uniformLocation("colorTexture");
        _uniformCache.scaling = _program->uniformLocation("scaling");
    }
}

bool RenderableGaiaStars::loadData() {
    std::string _file = _fitsFilePath;
    std::string cachedFile = FileSys.cacheManager()->cachedFilename(
        _file,
        ghoul::filesystem::CacheManager::Persistent::Yes
    );

    //bool hasCachedFile = FileSys.fileExists(cachedFile);
    //if (hasCachedFile) {
    //    LINFO("Cached file '" << cachedFile << "' used for Fits file '" << _file << "'");
    //
    //    bool success = loadCachedFile(cachedFile);
    //    if (success) {
    //        return true;
    //    }
    //    else {
    //        FileSys.cacheManager()->removeCacheFile(_file);
    //    }
    //}
    //else {
    //    LINFO("Cache for Fits file '" << _file << "' not found");
    //}
    //LINFO("Loading Fits file '" << _file << "'");

    bool success = readFitsFile();
    if (!success) {
        return false;
    }

    //LINFO("Saving cache");
    //success = saveCachedFile(cachedFile);

    return success;
}

bool RenderableGaiaStars::readFitsFile() {
    std::string _file = _fitsFilePath;

    int nStars = _lastRow - _firstRow + 1;

    FitsFileReader fitsInFile(false);
    std::shared_ptr<TableData<float>> table = fitsInFile.readTable<float>(_file, _columnNames,
        _firstRow, _lastRow);

    if (!table) {
        LERROR("Failed to open Fits file '" << _file << "'");
        return false;
    }

    _nValuesPerStar = _columnNames.size();
    int nNullArr = 0;

    std::unordered_map<string, std::vector<float>>& tableContent = table->contents;
    for (int i = 0; i < nStars; ++i) {
        std::vector<float> values(_nValuesPerStar);
        size_t idx = 0;
        for (std::string name : _columnNames) {
            std::vector<float> vecData = tableContent[name];
            values[idx] = vecData[i];
            idx++;
        }

        bool nullArray = true;
        for (size_t j = 0; j < values.size(); ++j) {
            if (values[j] != -999) {
                nullArray = false;
                break;
            }
        }
        if (i % 10000 == 0) {
            LINFO(std::to_string(i / 1000) + "k out of " + std::to_string(nStars) +
                " stars sorted!");
        }
        
        if (!nullArray) {
            _fullData.insert(_fullData.end(), values.begin(), values.end());
        } else {
            nNullArr++;
        }
    }
    LINFO(std::to_string(nNullArr) + " out of " + std::to_string(nStars) +
        " read stars were nullArrays");

    return true;
}

bool RenderableGaiaStars::loadCachedFile(const std::string& file) {
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

        _fullData.resize(nValues);
        fileStream.read(reinterpret_cast<char*>(&_fullData[0]),
            nValues * sizeof(_fullData[0]));

        bool success = fileStream.good();
        return success;
    }
    else {
        LERROR("Error opening file '" << file << "' for loading cache file");
        return false;
    }
}

bool RenderableGaiaStars::saveCachedFile(const std::string& file) const {
    std::ofstream fileStream(file, std::ofstream::binary);
    if (fileStream.good()) {
        fileStream.write(reinterpret_cast<const char*>(&CurrentCacheVersion),
            sizeof(int8_t));

        int32_t nValues = static_cast<int32_t>(_fullData.size());
        if (nValues == 0) {
            LERROR("Error writing cache: No values were loaded");
            return false;
        }
        fileStream.write(reinterpret_cast<const char*>(&nValues), sizeof(int32_t));

        int32_t nValuesPerStar = static_cast<int32_t>(_nValuesPerStar);
        fileStream.write(reinterpret_cast<const char*>(&nValuesPerStar), sizeof(int32_t));

        size_t nBytes = nValues * sizeof(_fullData[0]);
        fileStream.write(reinterpret_cast<const char*>(&_fullData[0]), nBytes);

        bool success = fileStream.good();
        return success;
    }
    else {
        LERROR("Error opening file '" << file << "' for save cache file");
        return false;
    }
}

void RenderableGaiaStars::createDataSlice() {
    _slicedData.clear();

    for (size_t i = 0; i < _fullData.size(); i += _nValuesPerStar) {
        glm::vec3 pos = glm::vec3(_fullData[i + 0], _fullData[i + 1], _fullData[i + 2]);
        //glm::vec3 position = glm::vec3(4662120063743.592773, 1263245003503.724854, -955413856565.788086);

        // Convert parsecs -> meter
        psc position = psc(glm::vec4(pos * 0.308567756f, 15));
        //glm::vec3 position = pos * static_cast<float>(distanceconstants::Parsec);
        /*LINFO("Pos for row: " + std::to_string(i/_nValuesPerStar) + " : ("
            + std::to_string(position.x) + ", "
            + std::to_string(position.y) + ", "
            + std::to_string(position.z) + ")");
        LINFO("Vel for row: " + std::to_string(i / _nValuesPerStar) + " : ("
            + std::to_string(_fullData[i + 3]) + ", "
            + std::to_string(_fullData[i + 4]) + ", "
            + std::to_string(_fullData[i + 5]) + ")");
        LINFO("Brighness for row: " + std::to_string(i / _nValuesPerStar) + " : ("
            + std::to_string(_fullData[i + 6]) + ", "
            + std::to_string(_fullData[i + 7]) + ")");*/

        union {
            VBOLayout value;
            std::array<float, sizeof(VBOLayout)> data;
        } layout;

        layout.value.position = { {
                position[0], position[1], position[2], position[3] // TODO
            } };
        layout.value.velocity = { {
                _fullData[i + 3], _fullData[i + 4], _fullData[i + 5]
            } };

        layout.value.magnitude = _fullData[i + 6];
        layout.value.bvColor = _fullData[i + 7];
        layout.value.luminance = 1.5;

        _slicedData.insert(_slicedData.end(), layout.data.begin(), layout.data.end());
    }
}

} // namespace openspace
