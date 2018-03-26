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
#include <modules/gaiamission/rendering/octreemanager.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/templatefactory.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/fmt.h>

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
        float bvColor;
    };

    static const openspace::properties::Property::PropertyInfo FitsFileInfo = {
        "File",
        "File Path",
        "The path to the FITS or BIN file with data for the stars to be rendered."
    }; 

    static const openspace::properties::Property::PropertyInfo FileTypeOriginInfo = {
        "FileTypeOrigin",
        "File Type Origin",
        "Specifies if preprocessed file is generated from a speck or a fits file."
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

    static const openspace::properties::Property::PropertyInfo LuminosityMultiplierInfo = {
        "LuminosityMultiplier",
        "Luminosity Multiplier",
        "Factor by which to multiply the luminosity with. [Works only in Color mode!]"
    };

    static const openspace::properties::Property::PropertyInfo MagnitudeBoostInfo = {
        "MagnitudeBoost",
        "Magnitude Boost",
        "Sets what percent of the star magnitude that will be used as boost to star size. "
        "[Works only in Color mode!]"
    };

    static const openspace::properties::Property::PropertyInfo CutOffThresholdInfo = {
        "CutOffThreshold",
        "Cut Off Threshold",
        "Set threshold for when to cut off star rendering. "
        "Stars closer than this threshold are given full opacity. "
        "Farther away, stars dim proportionally to the 4-logarithm of their distance."
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

    static const openspace::properties::Property::PropertyInfo NumRenderedStarsInfo = {
        "NumRenderedStars",
        "Rendered Stars",
        "The number of rendered stars in the current frame."
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
                FileTypeOriginInfo.identifier,
                new StringVerifier,
                Optional::No,
                FileTypeOriginInfo.description
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
                LuminosityMultiplierInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                LuminosityMultiplierInfo.description
            },
            {
                MagnitudeBoostInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                MagnitudeBoostInfo.description
            },
            {
                CutOffThresholdInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                CutOffThresholdInfo.description
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
    , _fileTypeOrigin(FileTypeOriginInfo)
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
    , _luminosityMultiplier(LuminosityMultiplierInfo, 100.f, 1.f, 1000.f)
    , _magnitudeBoost(MagnitudeBoostInfo, 25.f, 0.f, 100.f)
    , _cutOffThreshold(CutOffThresholdInfo, 38.f, 0.f, 50.f)
    , _sharpness(SharpnessInfo, 1.f, 0.f, 5.f)
    , _billboardSize(BillboardSizeInfo, 15.f, 1.f, 100.f)
    , _closeUpBoostDist(CloseUpBoostDistInfo, 10.f, 1.f, 1000.f)
    , _firstRow(FirstRowInfo, 1, 1, 2539913) // DR1-max: 2539913
    , _lastRow(LastRowInfo, 50000, 1, 2539913)
    , _columnNamesList(ColumnNamesInfo)
    , _nRenderedStars(NumRenderedStarsInfo, 0, 0, 2539913)
    , _program(nullptr)
    , _nValuesPerStar(0)
    , _nValuesInSlice(0)
    , _vao(0)
    , _vbo(0)
{
    using File = ghoul::filesystem::File;

    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "RenderableGaiaStars"
    );

    _octreeManager = std::make_shared<OctreeManager>();

    _fitsFilePath = absPath(dictionary.value<std::string>(FitsFileInfo.identifier));
    _fitsFile = std::make_unique<File>(_fitsFilePath);

    _fitsFilePath.onChange(
        [&] { _dataIsDirty = true; }
    );
    _fitsFile->setCallback(
        [&](const File&) { _dataIsDirty = true; }
    );
    addProperty(_fitsFilePath);

    _fileTypeOrigin = dictionary.value<std::string>(FileTypeOriginInfo.identifier);

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
    
    if (dictionary.hasKey(LuminosityMultiplierInfo.identifier)) {
        _luminosityMultiplier = static_cast<float>(
            dictionary.value<double>(LuminosityMultiplierInfo.identifier)
            );
    }
    addProperty(_luminosityMultiplier);

    if (dictionary.hasKey(MagnitudeBoostInfo.identifier)) {
        _magnitudeBoost = static_cast<float>(
            dictionary.value<double>(MagnitudeBoostInfo.identifier)
            );
    }
    addProperty(_magnitudeBoost);

    if (dictionary.hasKey(CutOffThresholdInfo.identifier)) {
        _cutOffThreshold = static_cast<float>(
            dictionary.value<double>(CutOffThresholdInfo.identifier)
            );
    }
    addProperty(_cutOffThreshold);

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

            auto stringKeys = tmpDict.keys();
            auto intKeys = std::set<int>();

            // Ugly fix for ASCII sorting when there are more columns read than 10.
            for (auto key : stringKeys) {
                intKeys.insert(std::stoi(key));
            }

            for (auto key : intKeys) {
                _columnNames.push_back(tmpDict.value<std::string>(std::to_string(key)));
            }

            // Copy values to the StringListproperty to be shown in the Property list.
            _columnNamesList = _columnNames;
        }

        if (_firstRow > _lastRow) {
            throw ghoul::RuntimeError("User defined FirstRow is bigger than LastRow.");
        }
    }

    // Add a read-only property for the number of rendered stars per frame.
    _nRenderedStars.setReadOnly(true);
    addProperty(_nRenderedStars);
}

RenderableGaiaStars::~RenderableGaiaStars() {}

bool RenderableGaiaStars::isReady() const {
    return (_program != nullptr) && (_octreeManager != nullptr);
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
    _uniformCache.luminosityMultiplier = _program->uniformLocation("luminosityMultiplier");
    _uniformCache.magnitudeBoost = _program->uniformLocation("magnitudeBoost");
    _uniformCache.cutOffThreshold = _program->uniformLocation("cutOffThreshold");
    _uniformCache.sharpness = _program->uniformLocation("sharpness");
    _uniformCache.billboardSize = _program->uniformLocation("billboardSize");
    _uniformCache.closeUpBoostDist = _program->uniformLocation("closeUpBoostDist");
    _uniformCache.screenSize = _program->uniformLocation("screenSize");
    _uniformCache.psfTexture = _program->uniformLocation("psfTexture");
    _uniformCache.time = _program->uniformLocation("time");
    _uniformCache.colorTexture = _program->uniformLocation("colorTexture");
    
    bool success = readFitsFile(ColumnOption(static_cast<int>(_columnOption)));
    if (!success) {
        throw ghoul::RuntimeError("Error loading file data");
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
    
    glm::mat4 model =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
        glm::dmat4(data.modelTransform.rotation) *
        glm::dmat4(glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale)));

    float viewScaling = 1.0f; //data.camera.scaling();
    glm::mat4 view = data.camera.combinedViewMatrix();
    glm::mat4 projection = data.camera.projectionMatrix();

    glm::mat4 modelViewProjMat = projection * view * model;
    glm::vec2 screenSize = glm::vec2(OsEng.renderEngine().renderingResolution());


    // Traverse Octree and build a map with new nodes to render, uses mvp matrix to decide.
    int deltaStars = 0;
    auto updateData = _octreeManager->traverseData(modelViewProjMat, screenSize, deltaStars);
    deltaStars /= static_cast<int>(_nValuesInSlice);

    // Update VBO with new nodes. 
    // This will overwrite old data that's not visible anymore as well.
    GLsizei maxStarsToRender = _octreeManager->maxStarsPerNode() *
        _octreeManager->biggestChunkIndexInUse();

    glBindVertexArray(_vao);
    glBindBuffer(GL_ARRAY_BUFFER, _vbo);

    // Use buffer orphaning to update a subset of total data.
    glBufferData(
        GL_ARRAY_BUFFER,
        _streamingBudget * sizeof(GLfloat),
        nullptr,
        GL_STREAM_DRAW
    );

    // Update buffer with one insert per chunk/node. The key in map holds the offset index.
    for (auto & [offset, subData] : updateData) {
        // Fill chunk by appending zeroes to data so we overwrite possible earlier values.
        subData.resize(_chunkSize, 0.f);
        glBufferSubData(
            GL_ARRAY_BUFFER,
            offset * _chunkSize * sizeof(GLfloat),
            _chunkSize * sizeof(GLfloat),
            subData.data()
        );
    }

    int nStars = static_cast<int>(_nRenderedStars) + deltaStars;
    _nRenderedStars.set(nStars);
    
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);
    
    // Activate shader program and send uniforms.
    glBlendFunc(GL_SRC_ALPHA, GL_ONE);
    glDepthMask(false);
    _program->activate();

    _program->setUniform(_uniformCache.model, model);
    _program->setUniform(_uniformCache.view, view);
    _program->setUniform(_uniformCache.viewScaling, viewScaling);
    _program->setUniform(_uniformCache.projection, projection);
    _program->setUniform(_uniformCache.columnOption, _columnOption);
    _program->setUniform(_uniformCache.luminosityMultiplier, _luminosityMultiplier);
    _program->setUniform(_uniformCache.magnitudeBoost, _magnitudeBoost);
    _program->setUniform(_uniformCache.cutOffThreshold, _cutOffThreshold);
    _program->setUniform(_uniformCache.sharpness, _sharpness);
    _program->setUniform(_uniformCache.billboardSize, _billboardSize);
    _program->setUniform(_uniformCache.closeUpBoostDist, 
        _closeUpBoostDist * static_cast<float>(distanceconstants::Parsec)
    );
    _program->setUniform(_uniformCache.screenSize, screenSize);
    _program->setUniform(_uniformCache.time, static_cast<float>(data.time.j2000Seconds()));


    ghoul::opengl::TextureUnit psfUnit;
    psfUnit.activate();
    _pointSpreadFunctionTexture->bind();
    _program->setUniform(_uniformCache.psfTexture, psfUnit);

    ghoul::opengl::TextureUnit colorUnit;
    colorUnit.activate();
    _colorTexture->bind();
    _program->setUniform(_uniformCache.colorTexture, colorUnit);


    // Draw call.
    //glEnable(GL_PROGRAM_POINT_SIZE);
    glBindVertexArray(_vao);
    glDrawArrays(GL_POINTS, 0, maxStarsToRender);
    glBindVertexArray(0);
    //glDisable(GL_PROGRAM_POINT_SIZE);
    _program->deactivate();

    glDepthMask(true);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
}

void RenderableGaiaStars::update(const UpdateData&) {
    if (_dataIsDirty) {
        LDEBUG("Regenerating data");
        const int option = _columnOption;

        // Reload data file (as long as it's not the first initialization)!
        if (_vao != 0) { 
            // This will reconstruct the Octree as well!
            bool success = readFitsFile(ColumnOption(option));
            if (!success) {
                throw ghoul::RuntimeError("Error loading FITS data");
            }
        }

        if (_vao == 0) {
            glGenVertexArrays(1, &_vao);
            LDEBUG(fmt::format("Generating Vertex Array id '{}'",  _vao));
        }
        if (_vbo == 0) {
            glGenBuffers(1, &_vbo);
            LDEBUG(fmt::format("Generating Vertex Buffer Object id '{}'", _vbo));
        }
        
        // Bind the VBO to our vertex array layout. 
        glBindVertexArray(_vao);
        glBindBuffer(GL_ARRAY_BUFFER, _vbo);

        // Initialize buffer memory to zeros. Not really needed because it will be overwritten
        // by glBufferSubData when a chunk uses the specified slot. 
        std::vector<float> dummyData(_streamingBudget, 0.f);

        glBufferData(
            GL_ARRAY_BUFFER,
            _streamingBudget * sizeof(GLfloat),
            dummyData.data(),
            GL_STREAM_DRAW
        );
        
        GLsizei stride = static_cast<GLsizei>(sizeof(GLfloat) * _nValuesInSlice);

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
                2,
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
                LDEBUG(fmt::format("Loaded texture from '{}'",
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
        if (_colorTexturePath.value() != "") {
            _colorTexture = ghoul::io::TextureReader::ref().loadTexture(
                absPath(_colorTexturePath)
            );
            if (_colorTexture) {
                LDEBUG(fmt::format("Loaded texture from '{}'", absPath(_colorTexturePath)));
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
        _uniformCache.luminosityMultiplier = _program->uniformLocation("luminosityMultiplier");
        _uniformCache.magnitudeBoost = _program->uniformLocation("magnitudeBoost");
        _uniformCache.cutOffThreshold = _program->uniformLocation("cutOffThreshold");
        _uniformCache.sharpness = _program->uniformLocation("sharpness");
        _uniformCache.billboardSize = _program->uniformLocation("billboardSize");
        _uniformCache.closeUpBoostDist = _program->uniformLocation("closeUpBoostDist");
        _uniformCache.screenSize = _program->uniformLocation("screenSize");
        _uniformCache.psfTexture = _program->uniformLocation("psfTexture");
        _uniformCache.time = _program->uniformLocation("time");
        _uniformCache.colorTexture = _program->uniformLocation("colorTexture");
    }
}

bool RenderableGaiaStars::readFitsFile(ColumnOption option) {
    std::string _file = _fitsFilePath;
    _fullData.clear();
    _octreeManager->initOctree();

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

            // Slice star data and insert star into octree.
            for (size_t i = 0; i < _fullData.size(); i += _nValuesPerStar) {
                auto first = _fullData.begin() + i;
                auto last = _fullData.begin() + i + _nValuesPerStar;
                std::vector<float> values(first, last);

                // Data needs to be sliced differently depending on file origin.
                auto slicedValues = std::vector<float>();
                if (fitsOrigin.compare(_fileTypeOrigin)) {
                    slicedValues = sliceStarValues(option, values);
                }
                else if (speckOrigin.compare(_fileTypeOrigin)) {
                    slicedValues = sliceSpeckStars(option, values);
                }
                else {
                    LERROR("User did not specify correct origin of preprocessed file.");
                }
                
                _nValuesInSlice = slicedValues.size(); // Unnecessary to do for every star.
                _octreeManager->insert(slicedValues);
            }

        }
        else {
            LERROR(fmt::format("Error opening file '{}' for loading preprocessed file!"
                , _file));
            return false;
        }
    }
    else {
        int nStars = _lastRow - _firstRow + 1;

        FitsFileReader fitsInFile(false);
        std::shared_ptr<TableData<float>> table = fitsInFile.readTable<float>(_file, 
            _columnNames, _firstRow, _lastRow);

        if (!table) {
            LERROR(fmt::format("Failed to open Fits file '{}'", _file));
            return false;
        }

        _nValuesPerStar = _columnNames.size();
        int nNullArr = 0;
        size_t defaultCols = 17; // Default: 8, Full: 17

        std::unordered_map<string, std::vector<float>>& tableContent = table->contents;
        std::vector<float> posXcol = tableContent[_columnNames[0]];
        std::vector<float> posYcol = tableContent[_columnNames[1]];
        std::vector<float> posZcol = tableContent[_columnNames[2]];
        std::vector<float> velXcol = tableContent[_columnNames[3]];
        std::vector<float> velYcol = tableContent[_columnNames[4]];
        std::vector<float> velZcol = tableContent[_columnNames[5]];
        std::vector<float> magCol = tableContent[_columnNames[6]];
        std::vector<float> parallax = tableContent[_columnNames[7]];

        std::vector<float> parallax_err = tableContent[_columnNames[8]];
        std::vector<float> pr_mot_ra = tableContent[_columnNames[9]];
        std::vector<float> pr_mot_ra_err = tableContent[_columnNames[10]];
        std::vector<float> pr_mot_dec = tableContent[_columnNames[11]];
        std::vector<float> pr_mot_dec_err = tableContent[_columnNames[12]];
        std::vector<float> tycho_b = tableContent[_columnNames[13]];
        std::vector<float> tycho_b_err = tableContent[_columnNames[14]];
        std::vector<float> tycho_v = tableContent[_columnNames[15]];
        std::vector<float> tycho_v_err = tableContent[_columnNames[16]];

        for (int i = 0; i < nStars; ++i) {
            std::vector<float> values(_nValuesPerStar);
            size_t idx = 0;

            // Read positions.
            values[idx++] = posXcol[i];
            values[idx++] = posYcol[i];
            values[idx++] = posZcol[i];

            // Return early if star doesn't have a measured position.
            if (values[0] == -999 && values[1] == -999 && values[2] == -999) {
                nNullArr++;
                continue;
            }

            // Read the rest of the default values.
            values[idx++] = velXcol[i];
            values[idx++] = velYcol[i];
            values[idx++] = velZcol[i];
            values[idx++] = magCol[i];
            values[idx++] = parallax[i];

            values[idx++] = parallax_err[i];
            values[idx++] = pr_mot_ra[i];
            values[idx++] = pr_mot_ra_err[i];
            values[idx++] = pr_mot_dec[i];
            values[idx++] = pr_mot_dec_err[i];
            values[idx++] = tycho_b[i];
            values[idx++] = tycho_b_err[i];
            values[idx++] = tycho_v[i];
            values[idx++] = tycho_v_err[i];

            // Read extra columns, if any. This will slow down the sorting tremendously!
            for (size_t col = defaultCols; col < _nValuesPerStar; ++col) {
                std::vector<float> vecData = tableContent[_columnNames[col]];
                values[idx++] = vecData[i];
            }

            for (size_t j = 0; j < _nValuesPerStar; ++j) {
                // The astronomers in Vienna use -999 as default value. Change it to 0.
                if (values[j] == -999) {
                    values[j] = 0.f;
                }
            }

            // Slice star data and insert star into octree.
            // TODO: Do this earlier so I don't read values to just throw them away!? 
            auto slicedValues = sliceStarValues(option, values); 
            _nValuesInSlice = slicedValues.size(); // Unnecessary to do for every star.

            // TODO: Insert into correct subfile & sort in Morton order (z-order)!?
            _octreeManager->insert(slicedValues);
        }
        LINFO(std::to_string(nNullArr) + " out of " + std::to_string(nStars) +
            " read stars were nullArrays");

    }

    // Init rendering info and VBO stack in Octree. 
    _chunkSize = _octreeManager->maxStarsPerNode() * _nValuesInSlice;
    _streamingBudget = _octreeManager->totalNodes() * _chunkSize;
    _streamingBudget = std::min(_streamingBudget, _memoryBudgetInValues);
    int maxNodesInStream = static_cast<int>(_streamingBudget / _chunkSize);

    LINFO("Chunk size: " + std::to_string(_chunkSize) +
        " Streaming budget: " + std::to_string(_streamingBudget) +
        " Max Nodes in stream: " + std::to_string(maxNodesInStream));

    _octreeManager->initVBOIndexStack(maxNodesInStream);

    //_octreeManager->printStarsPerNode();
   
    return true;
}

// Slices every star seperately, before they are inserted into Octree. 
// Conversion of position is done in vertex shader to simplify calculations in Octree.
std::vector<float> RenderableGaiaStars::sliceStarValues(ColumnOption option, 
    std::vector<float> starValues) {

    auto tmpData = std::vector<float>();

    // Conversion kiloparsecs -> meter is done in vertex shader.
    glm::vec3 position = glm::vec3(starValues[0], starValues[1], starValues[2]);
    //position *= 1000 * static_cast<float>(distanceconstants::Parsec);

    // Convert milliarcseconds/year to m/s
    float parallax = starValues[7];
    glm::vec3 velocity = glm::vec3(
        convertMasPerYearToMeterPerSecond(starValues[3], parallax),
        convertMasPerYearToMeterPerSecond(starValues[4], parallax),
        convertMasPerYearToMeterPerSecond(starValues[5], parallax));

    switch (option) {
    case ColumnOption::Static: {
        union {
            StaticVBOLayout value;
            std::array<float, sizeof(StaticVBOLayout) / sizeof(float)> data;
        } layout;

        layout.value.position = { {
                position[0], position[1], position[2]
            } };

        tmpData.insert(tmpData.end(), layout.data.begin(), layout.data.end());
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

        tmpData.insert(tmpData.end(), layout.data.begin(), layout.data.end());
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
        layout.value.magnitude = starValues[6];

        // B-V color is Blue minus Visible filter magnitudes
        layout.value.bvColor = starValues[13] - starValues[15];

        tmpData.insert(tmpData.end(), layout.data.begin(), layout.data.end());
        break;
    }
    }
    return tmpData;
}

// Slices every star seperately, from SPECK file, before they are inserted into Octree. 
// Conversion of position is done in vertex shader to simplify calculations in Octree.
std::vector<float> RenderableGaiaStars::sliceSpeckStars(ColumnOption option,
    std::vector<float> starValues) {

    auto tmpData = std::vector<float>();

    // Conversion kiloparsecs -> meter is done in vertex shader.
    glm::vec3 position = glm::vec3(starValues[0], starValues[1], starValues[2]);
    //position *= 1000 * static_cast<float>(distanceconstants::Parsec);

    // TODO: Unclear which columns are velocity (and what unit in that case)!'
    // Right now we're using [U,V,W] from speck file. 
    glm::vec3 velocity = glm::vec3(starValues[9], starValues[10], starValues[11]);

    switch (option) {
    case ColumnOption::Static: {
        union {
            StaticVBOLayout value;
            std::array<float, sizeof(StaticVBOLayout) / sizeof(float)> data;
        } layout;

        layout.value.position = { {
                position[0], position[1], position[2]
            } };

        tmpData.insert(tmpData.end(), layout.data.begin(), layout.data.end());
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

        tmpData.insert(tmpData.end(), layout.data.begin(), layout.data.end());
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
        layout.value.magnitude = starValues[5];
        // Luminosity is in starValues[4].
        // However, we're already doing those computations in the shader from magnitude.

        // B-V color is Blue minus Visible filter magnitudes.
        // Already computed in speck file. 
        layout.value.bvColor = starValues[3];

        tmpData.insert(tmpData.end(), layout.data.begin(), layout.data.end());
        break;
    }
    }
    return tmpData;
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
