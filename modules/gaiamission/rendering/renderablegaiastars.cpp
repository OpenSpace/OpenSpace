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
#include <openspace/util/timeconversion.h>
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

    struct StaticVBOLayout {
        std::array<float, 3> position; // (x,y,z)
    };

    struct MotionVBOLayout {
        std::array<float, 3> position; // (x,y,z)
        std::array<float, 3> velocity; // (x,y,z)
    };

    struct ColorVBOLayout {
        std::array<float, 3> position; // (x,y,z)
        std::array<float, 3> velocity; // (x,y,z)
        float magnitude;
    };

    static const openspace::properties::Property::PropertyInfo FitsFileInfo = {
        "File",
        "File Path",
        "The path to the FITS or BIN file with data for the stars to be rendered."
    }; 

    static const openspace::properties::Property::PropertyInfo FilePreprocessedInfo = {
        "FilePreprocessed",
        "File Preprocessed",
        "If true then use a preprocessed BIN file. "
        "If false then a FITS file will be read."
    };

    static const openspace::properties::Property::PropertyInfo ColumnOptionInfo = {
        "ColumnOption",
        "Column Option",
        "This value determines which predefined columns to use. If 'Static' only the "
        "position of the stars is used. 'Motion' uses position + velocity and 'Color' "
        "uses pos, vel as well as magnitude for the color."
    };
    
    static const openspace::properties::Property::PropertyInfo PsfTextureInfo = {
        "Texture",
        "Point Spread Function Texture",
        "The path to the texture that should be used as a point spread function for the "
        "stars."
    };

    static const openspace::properties::Property::PropertyInfo MagnitudeExponentInfo = {
        "MagnitudeExponent",
        "MagnitudeExponent",
        "Adjust star magnitude by 10^MagnitudeExponent. "
        "Stars closer than this distance are given full opacity. "
        "Farther away, stars dim proportionally to the logarithm of their distance."
    };

    static const openspace::properties::Property::PropertyInfo SharpnessInfo = {
        "Sharpness",
        "Sharpness",
        "Adjust star sharpness"
    };

    static const openspace::properties::Property::PropertyInfo BillboardSizeInfo = {
        "BillboardSize",
        "Billboard Size",
        "Set the billboard size of all stars"
    };

    static const openspace::properties::Property::PropertyInfo CloseUpBoostDistInfo = {
        "CloseUpBoostDist",
        "Close-Up Boost Distance [pc]",
        "Set the distance where stars starts to increase in size. Unit is Parsec."
    };

    static const openspace::properties::Property::PropertyInfo ColorTextureInfo = {
        "ColorMap",
        "Color Texture",
        "The path to the texture that is used to convert from the magnitude of the star "
        "to its color. The texture is used as a one dimensional lookup function."
    };

    static const openspace::properties::Property::PropertyInfo FirstRowInfo = {
        "FirstRow",
        "First Row to Read",
        "Defines the first row that will be read from the specified FITS file."
        "No need to define if data already has been processed."
    };

    static const openspace::properties::Property::PropertyInfo LastRowInfo = {
        "LastRow",
        "Last Row to Read",
        "Defines the last row that will be read from the specified FITS file."
        "Has to be equal to or greater than FirstRow. No need to define if "
        "data already has been processed."
    };

    static const openspace::properties::Property::PropertyInfo ColumnNamesInfo = {
        "ColumnNames",
        "Column Names",
        "A list of strings with the names of all the columns that are to be "
        "read from the specified FITS file. No need to define if data already "
        "has been processed."
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
                FilePreprocessedInfo.identifier,
                new BoolVerifier,
                Optional::No,
                FilePreprocessedInfo.description
            },
            {
                ColumnOptionInfo.identifier,
                new StringInListVerifier({
                    "Static", "Motion", "Color"
                }),
                Optional::Yes,
                ColumnOptionInfo.description
            },
            {
                PsfTextureInfo.identifier,
                new StringVerifier,
                Optional::No,
                PsfTextureInfo.description
            },
            {
                MagnitudeExponentInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                MagnitudeExponentInfo.description
            },
            {
                SharpnessInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                SharpnessInfo.description
            },
            {
                BillboardSizeInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                BillboardSizeInfo.description
            },
            {
                CloseUpBoostDistInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                CloseUpBoostDistInfo.description
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
                Optional::Yes,
                FirstRowInfo.description
            },
            {
                LastRowInfo.identifier,
                new IntVerifier,
                Optional::Yes,
                LastRowInfo.description
            },
            {
                ColumnNamesInfo.identifier,
                new StringListVerifier,
                Optional::Yes,
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
    , _filePreprocessed(FilePreprocessedInfo, false)
    , _columnOption(ColumnOptionInfo, properties::OptionProperty::DisplayType::Dropdown)
    , _pointSpreadFunctionTexturePath(PsfTextureInfo)
    , _pointSpreadFunctionTexture(nullptr)
    , _pointSpreadFunctionTextureIsDirty(true)
    , _colorTexturePath(ColorTextureInfo)
    , _colorTexture(nullptr)
    , _colorTextureIsDirty(true)
    , _magnitudeExponent(MagnitudeExponentInfo, 19.f, 0.f, 50.f)
    , _sharpness(SharpnessInfo, 1.f, 0.f, 5.f)
    , _billboardSize(BillboardSizeInfo, 15.f, 1.f, 100.f)
    , _closeUpBoostDist(CloseUpBoostDistInfo, 10.f, 1.f, 1000.f)
    , _firstRow(FirstRowInfo, 1, 1, 2539913) // DR1-max: 2539913
    , _lastRow(LastRowInfo, 50000, 1, 2539913)
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

    _columnOption.addOptions({
        { ColumnOption::Static, "Static" },
        { ColumnOption::Motion, "Motion" },
        { ColumnOption::Color, "Color" }
    });
    if (dictionary.hasKey(ColumnOptionInfo.identifier)) {
        const std::string columnOption = dictionary.value<std::string>(
            ColumnOptionInfo.identifier
            );
        if (columnOption == "Static") {
            _columnOption = ColumnOption::Static;
        }
        else if (columnOption == "Motion") {
            _columnOption = ColumnOption::Motion;
        }
        else {
            _columnOption = ColumnOption::Color;
        }
    }
    _columnOption.onChange([&] { _dataIsDirty = true; });
    addProperty(_columnOption);

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
    
    if (dictionary.hasKey(MagnitudeExponentInfo.identifier)) {
        _magnitudeExponent = static_cast<float>(
            dictionary.value<double>(MagnitudeExponentInfo.identifier)
            );
    }
    addProperty(_magnitudeExponent);

    if (dictionary.hasKey(SharpnessInfo.identifier)) {
        _sharpness = static_cast<float>(
            dictionary.value<double>(SharpnessInfo.identifier)
            );
    }
    addProperty(_sharpness);

    if (dictionary.hasKey(BillboardSizeInfo.identifier)) {
        _billboardSize = static_cast<float>(
            dictionary.value<double>(BillboardSizeInfo.identifier)
            );
    }
    addProperty(_billboardSize);

    if (dictionary.hasKey(CloseUpBoostDistInfo.identifier)) {
        _closeUpBoostDist = static_cast<float>(
            dictionary.value<double>(CloseUpBoostDistInfo.identifier)
            );
    }
    addProperty(_closeUpBoostDist);

    if (dictionary.hasKey(FilePreprocessedInfo.identifier)) {
        _filePreprocessed = dictionary.value<bool>(FilePreprocessedInfo.identifier);
    }

    // No need to add properties if file has been preprocessed.
    if (!_filePreprocessed) {
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
        // There's not any point in exposing this property atm --adaal
        // _columnNamesList.onChange([&] { 
        //     _dataIsDirty = true; 
        //     _columnNames = _columnNamesList;
        // });
        // addProperty(_columnNamesList);

        if (_firstRow > _lastRow) {
            throw ghoul::RuntimeError("User defined FirstRow is bigger than LastRow.");
        }
    }
}

RenderableGaiaStars::~RenderableGaiaStars() {}

bool RenderableGaiaStars::isReady() const {
    return (_program != nullptr) && (!_fullData.empty());
}

void RenderableGaiaStars::initializeGL() {
    RenderEngine& renderEngine = OsEng.renderEngine();
    _program = renderEngine.buildRenderProgram("GaiaStar",
        absPath("${MODULE_GAIAMISSION}/shaders/gaia_star_vs.glsl"),
        absPath("${MODULE_GAIAMISSION}/shaders/gaia_star_fs.glsl"),
        absPath("${MODULE_GAIAMISSION}/shaders/gaia_star_ge.glsl")
        //absPath("${MODULE_GAIAMISSION}/shaders/gaia_point_vs.glsl"),
        //absPath("${MODULE_GAIAMISSION}/shaders/gaia_point_fs.glsl"),
        //absPath("${MODULE_GAIAMISSION}/shaders/gaia_point_ge.glsl")
        
    );
    //using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;
    //_program->setIgnoreUniformLocationError(IgnoreError::Yes);

    _uniformCache.model = _program->uniformLocation("model");
    _uniformCache.view = _program->uniformLocation("view");
    _uniformCache.viewScaling = _program->uniformLocation("viewScaling");
    _uniformCache.projection = _program->uniformLocation("projection");
    _uniformCache.columnOption = _program->uniformLocation("columnOption");
    _uniformCache.magnitudeExponent = _program->uniformLocation("magnitudeExponent");
    _uniformCache.sharpness = _program->uniformLocation("sharpness");
    _uniformCache.billboardSize = _program->uniformLocation("billboardSize");
    _uniformCache.closeUpBoostDist = _program->uniformLocation("closeUpBoostDist");
    _uniformCache.screenSize = _program->uniformLocation("screenSize");
    _uniformCache.psfTexture = _program->uniformLocation("psfTexture");
    _uniformCache.time = _program->uniformLocation("time");
    _uniformCache.colorTexture = _program->uniformLocation("colorTexture");

    bool success = readFitsFile();
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
    glBlendFunc(GL_SRC_ALPHA, GL_ONE);
    glDepthMask(false);
    _program->activate();

    glm::mat4 model =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
        glm::dmat4(data.modelTransform.rotation) *
        glm::dmat4(glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale)));

    float viewScaling = 1.0f; //data.camera.scaling();
    glm::mat4 view = data.camera.combinedViewMatrix();
    glm::mat4 projection = data.camera.projectionMatrix();

    _program->setUniform(_uniformCache.model, model);
    _program->setUniform(_uniformCache.view, view);
    _program->setUniform(_uniformCache.viewScaling, viewScaling);
    _program->setUniform(_uniformCache.projection, projection);
    _program->setUniform(_uniformCache.columnOption, _columnOption);
    _program->setUniform(_uniformCache.magnitudeExponent, _magnitudeExponent);
    _program->setUniform(_uniformCache.sharpness, _sharpness);
    _program->setUniform(_uniformCache.billboardSize, _billboardSize);
    _program->setUniform(_uniformCache.closeUpBoostDist, 
        _closeUpBoostDist * static_cast<float>(distanceconstants::Parsec)
    );
    _program->setUniform(_uniformCache.screenSize,
        glm::vec2(OsEng.renderEngine().renderingResolution())
    );
    _program->setUniform(_uniformCache.time, static_cast<float>(data.time.j2000Seconds()));


    ghoul::opengl::TextureUnit psfUnit;
    psfUnit.activate();
    _pointSpreadFunctionTexture->bind();
    _program->setUniform(_uniformCache.psfTexture, psfUnit);

    ghoul::opengl::TextureUnit colorUnit;
    colorUnit.activate();
    _colorTexture->bind();
    _program->setUniform(_uniformCache.colorTexture, colorUnit);

    glEnable(GL_PROGRAM_POINT_SIZE);
    glBindVertexArray(_vao);
    const GLsizei nStars = static_cast<GLsizei>(_fullData.size() / _nValuesPerStar);
    glDrawArrays(GL_POINTS, 0, nStars);

    glDisable(GL_PROGRAM_POINT_SIZE);
    glBindVertexArray(0);
    _program->deactivate();

    glDepthMask(true);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
}

void RenderableGaiaStars::update(const UpdateData&) {
    if (_dataIsDirty) {
        LDEBUG("Regenerating data");
        const int option = _columnOption;

        // Reload fits file (as long as it's not the first initialization)!
        if (_vao != 0) {
            // Bypass cached files and go directly to read function.
            bool success = readFitsFile();
            if (!success) {
                throw ghoul::RuntimeError("Error loading FITS data");
            }
        }

        createDataSlice(ColumnOption(option));

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

        const size_t nStars = _fullData.size() / _nValuesPerStar;
        const size_t nValues = _slicedData.size() / nStars;

        GLsizei stride = static_cast<GLsizei>(sizeof(GLfloat) * nValues);

        switch (option) {
        case ColumnOption::Static: {
            GLint positionAttrib = _program->attributeLocation("in_position");

            glEnableVertexAttribArray(positionAttrib);

            glVertexAttribPointer(
                positionAttrib,
                3,
                GL_FLOAT,
                GL_FALSE,
                stride,
                nullptr // = offsetof(StaticVBOLayout, position)
            );

            break;
        } 
        case ColumnOption::Motion: {
            GLint positionAttrib = _program->attributeLocation("in_position");
            GLint velocityAttrib = _program->attributeLocation("in_velocity");

            glEnableVertexAttribArray(positionAttrib);
            glEnableVertexAttribArray(velocityAttrib);

            glVertexAttribPointer(
                positionAttrib,
                3,
                GL_FLOAT,
                GL_FALSE,
                stride,
                nullptr // = offsetof(MotionVBOLayout, position)
            );
            glVertexAttribPointer(
                velocityAttrib,
                3,
                GL_FLOAT,
                GL_FALSE,
                stride,
                reinterpret_cast<void*>(offsetof(MotionVBOLayout, velocity))
            );

            break;
        }
        case ColumnOption::Color: {
            GLint positionAttrib = _program->attributeLocation("in_position");
            GLint velocityAttrib = _program->attributeLocation("in_velocity");
            GLint brightnessDataAttrib = _program->attributeLocation("in_brightness");

            glEnableVertexAttribArray(positionAttrib);
            glEnableVertexAttribArray(velocityAttrib);
            glEnableVertexAttribArray(brightnessDataAttrib);

            glVertexAttribPointer(
                positionAttrib,
                3,
                GL_FLOAT,
                GL_FALSE,
                stride,
                nullptr // = offsetof(ColorVBOLayout, position)
            );
            glVertexAttribPointer(
                velocityAttrib,
                3,
                GL_FLOAT,
                GL_FALSE,
                stride,
                reinterpret_cast<void*>(offsetof(ColorVBOLayout, velocity))
            );
            glVertexAttribPointer(
                brightnessDataAttrib,
                1,
                GL_FLOAT,
                GL_FALSE,
                stride,
                reinterpret_cast<void*>(offsetof(ColorVBOLayout, magnitude))
            );
            break;
        }
        }

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

        _uniformCache.model = _program->uniformLocation("model");
        _uniformCache.view = _program->uniformLocation("view");
        _uniformCache.viewScaling = _program->uniformLocation("viewScaling");
        _uniformCache.projection = _program->uniformLocation("projection");
        _uniformCache.columnOption = _program->uniformLocation("columnOption");
        _uniformCache.magnitudeExponent = _program->uniformLocation("magnitudeExponent");
        _uniformCache.sharpness = _program->uniformLocation("sharpness");
        _uniformCache.billboardSize = _program->uniformLocation("billboardSize");
        _uniformCache.closeUpBoostDist = _program->uniformLocation("closeUpBoostDist");
        _uniformCache.screenSize = _program->uniformLocation("screenSize");
        _uniformCache.psfTexture = _program->uniformLocation("psfTexture");
        _uniformCache.time = _program->uniformLocation("time");
        _uniformCache.colorTexture = _program->uniformLocation("colorTexture");
    }
}

bool RenderableGaiaStars::readFitsFile() {
    std::string _file = _fitsFilePath;
    _fullData.clear();

    LINFO("Loading FITS file: " + _file);

    // Read from binary if file has been preprocessed, else read from FITS file.
    if (_filePreprocessed) {
        std::ifstream fileStream(_file, std::ifstream::binary);
        if (fileStream.good()) {

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
            LERROR("Error opening file '" << _file << "' for loading preprocessed file!");
            return false;
        }
    }
    else {
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
        std::vector<float> posXcol = tableContent[_columnNames[0]];
        std::vector<float> posYcol = tableContent[_columnNames[1]];
        std::vector<float> posZcol = tableContent[_columnNames[2]];
        std::vector<float> velXcol = tableContent[_columnNames[3]];
        std::vector<float> velYcol = tableContent[_columnNames[4]];
        std::vector<float> velZcol = tableContent[_columnNames[5]];
        std::vector<float> magCol = tableContent[_columnNames[6]];

        for (int i = 0; i < nStars; ++i) {
            std::vector<float> values(_nValuesPerStar);
            size_t idx = 0;

            // Read default values.
            values[idx++] = posXcol[i];
            values[idx++] = posYcol[i];
            values[idx++] = posZcol[i];
            values[idx++] = velXcol[i];
            values[idx++] = velYcol[i];
            values[idx++] = velZcol[i];
            values[idx++] = magCol[i];

            // Read extra columns, if any. This will slow down the sorting tremendously!
            for (size_t col = 7; col < _nValuesPerStar; ++col) {
                std::vector<float> vecData = tableContent[_columnNames[col]];
                values[idx++] = vecData[i];
            }

            bool nullArray = true;
            for (size_t j = 0; j < _nValuesPerStar; ++j) {
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
            }
            else {
                nNullArr++;
            }
        }
        LINFO(std::to_string(nNullArr) + " out of " + std::to_string(nStars) +
            " read stars were nullArrays");
    }
   
    return true;
}

void RenderableGaiaStars::createDataSlice(ColumnOption option) {
    _slicedData.clear();

    for (size_t i = 0; i < _fullData.size(); i += _nValuesPerStar) {
        float parallax = _fullData[i + 7];

        // Convert kiloparsecs -> meter
        glm::vec3 position = glm::vec3(_fullData[i + 0], _fullData[i + 1], _fullData[i + 2]);
        position *= 1000 * static_cast<float>(distanceconstants::Parsec);

        // Convert milliarcseconds/year to m/s
        glm::vec3 velocity = glm::vec3(
            convertMasPerYearToMeterPerSecond(_fullData[i + 3], parallax),
            convertMasPerYearToMeterPerSecond(_fullData[i + 4], parallax),
            convertMasPerYearToMeterPerSecond(_fullData[i + 5], parallax));

        switch (option) {
        case ColumnOption::Static: {
            union {
                StaticVBOLayout value;
                std::array<float, sizeof(StaticVBOLayout) / sizeof(float)> data;
            } layout;

            layout.value.position = { {
                    position[0], position[1], position[2]
                } };

            _slicedData.insert(_slicedData.end(), layout.data.begin(), layout.data.end());

            break;
        }
            
        case ColumnOption::Motion: {
            union {
                MotionVBOLayout value;
                std::array<float, sizeof(MotionVBOLayout) / sizeof(float)> data;
            } layout;

            layout.value.position = { {
                    position[0], position[1], position[2]
                } };
            layout.value.velocity = { {
                    velocity[0], velocity[1], velocity[2]
                } };

            _slicedData.insert(_slicedData.end(), layout.data.begin(), layout.data.end());
            break;
        }
            
        case ColumnOption::Color: {
            union {
                ColorVBOLayout value;
                std::array<float, sizeof(ColorVBOLayout) / sizeof(float)> data;
            } layout;

            layout.value.position = { {
                    position[0], position[1], position[2]
                } };
            layout.value.velocity = { {
                    velocity[0], velocity[1], velocity[2]
                } };

            layout.value.magnitude = _fullData[i + 6];

            _slicedData.insert(_slicedData.end(), layout.data.begin(), layout.data.end());
            break;
        }
        }
            
    }
}

float RenderableGaiaStars::convertMasPerYearToMeterPerSecond(float masPerYear, 
    float parallax) {

    float degreeFromMas = 1 / 3600000.0;
    float radiusInMeter = ( static_cast<float>(distanceconstants::Parsec) * 1000 ) 
        / parallax;
    float perYearToPerSecond = 1 / SecondsPerYear;

    float meterPerSecond = masPerYear * degreeFromMas * radiusInMeter * perYearToPerSecond;
    return meterPerSecond;

}

} // namespace openspace
