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
#include <openspace/util/distanceconversion.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/wrapper/windowwrapper.h>
#include <openspace/rendering/renderengine.h>

#include <modules/fitsfilereader/include/fitsfilereader.h>
#include <modules/gaiamission/rendering/octreemanager.h>
#include <modules/gaiamission/rendering/renderoption.h>

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

    struct ColorVBOLayout {
        std::array<float, 3> position; // (x,y,z)
        float magnitude;
        float bvColor;
    };

    struct MotionVBOLayout {
        std::array<float, 3> position; // (x,y,z)
        float magnitude;
        float bvColor;
        std::array<float, 3> velocity; // (x,y,z)
    };

    

    static const openspace::properties::Property::PropertyInfo FilePathInfo = {
        "File",
        "File Path",
        "The path to the FITS, SPECK or BIN file with data for the stars to be rendered."
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

    static const openspace::properties::Property::PropertyInfo RenderOptionInfo = {
        "RenderOption",
        "Render Option",
        "This value determines which predefined columns to use in rendering. If 'Static' "
        "only the position of the stars is used. 'Color' uses position + color parameters "
        "and 'Motion' uses pos, color as well as velocity for the stars."
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
                FilePathInfo.identifier,
                new StringVerifier,
                Optional::No,
                FilePathInfo.description
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
                RenderOptionInfo.identifier,
                new StringInListVerifier({
                    "Static", "Color", "Motion"
                }),
                Optional::Yes,
                RenderOptionInfo.description
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
    , _filePath(FilePathInfo)
    , _fileTypeOrigin("")
    , _dataFile(nullptr)
    , _dataIsDirty(true)
    , _filePreprocessed(FilePreprocessedInfo, false)
    , _renderOption(RenderOptionInfo, properties::OptionProperty::DisplayType::Dropdown)
    , _pointSpreadFunctionTexturePath(PsfTextureInfo)
    , _pointSpreadFunctionTexture(nullptr)
    , _pointSpreadFunctionTextureIsDirty(true)
    , _colorTexturePath(ColorTextureInfo)
    , _colorTexture(nullptr)
    , _colorTextureIsDirty(true)
    , _luminosityMultiplier(LuminosityMultiplierInfo, 100.f, 1.f, 100000.f)
    , _magnitudeBoost(MagnitudeBoostInfo, 25.f, 0.f, 100.f)
    , _cutOffThreshold(CutOffThresholdInfo, 38.f, 0.f, 50.f)
    , _sharpness(SharpnessInfo, 1.45f, 0.f, 5.f)
    , _billboardSize(BillboardSizeInfo, 10.f, 1.f, 100.f)
    , _closeUpBoostDist(CloseUpBoostDistInfo, 300.f, 1.f, 1000.f)
    , _firstRow(FirstRowInfo, 1, 1, 2539913) // DR1-max: 2539913
    , _lastRow(LastRowInfo, 50000, 1, 2539913)
    , _columnNamesList(ColumnNamesInfo)
    , _nRenderedStars(NumRenderedStarsInfo, 0, 0, 2539913)
    , _program(nullptr)
    , _programTM(nullptr)
    , _fboTexture(nullptr)
    , _nValuesPerStar(0)
    , _nValuesInSlice(0)
    , _vao(0)
    , _vboPos(0)
    , _vboCol(0)
    , _vboVel(0)
    , _ssboIdx(0)
    , _ssboData(0)
    , _vaoQuad(0)
    , _vboQuad(0)
    , _fbo(0)
{
    using File = ghoul::filesystem::File;

    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "RenderableGaiaStars"
    );

    _octreeManager = std::make_shared<OctreeManager>();
    _fullData = std::vector<float>();
    _accumulatedIndices = std::vector<int>(1, 0);

    _filePath = absPath(dictionary.value<std::string>(FilePathInfo.identifier));
    _dataFile = std::make_unique<File>(_filePath);

    _filePath.onChange(
        [&] { _dataIsDirty = true; }
    );
    _dataFile->setCallback(
        [&](const File&) { _dataIsDirty = true; }
    );
    addProperty(_filePath);

    _fileTypeOrigin = dictionary.value<std::string>(FileTypeOriginInfo.identifier);

    _renderOption.addOptions({
        { gaiamission::RenderOption::Static, "Static" },
        { gaiamission::RenderOption::Color, "Color" },
        { gaiamission::RenderOption::Motion, "Motion" }
    });
    if (dictionary.hasKey(RenderOptionInfo.identifier)) {
        const std::string renderOption = dictionary.value<std::string>(
            RenderOptionInfo.identifier
            );
        if (renderOption == "Static") {
            _renderOption = gaiamission::RenderOption::Static;
        }
        else if (renderOption == "Color") {
            _renderOption = gaiamission::RenderOption::Color;
        }
        else {
            _renderOption = gaiamission::RenderOption::Motion;
        }
    }
    _renderOption.onChange([&] { _dataIsDirty = true; });
    addProperty(_renderOption);

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
    return (_program != nullptr) && (_programTM != nullptr) && (_octreeManager != nullptr);
}

void RenderableGaiaStars::initializeGL() {
    RenderEngine& renderEngine = OsEng.renderEngine();
    _program = ghoul::opengl::ProgramObject::Build(
        "GaiaStar",
        absPath("${MODULE_GAIAMISSION}/shaders/gaia_point_vs.glsl"),
        absPath("${MODULE_GAIAMISSION}/shaders/gaia_point_fs.glsl"),
        absPath("${MODULE_GAIAMISSION}/shaders/gaia_point_ge.glsl")
    );
    //using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;
    //_program->setIgnoreUniformLocationError(IgnoreError::Yes);

    _uniformCache.model = _program->uniformLocation("model");
    _uniformCache.view = _program->uniformLocation("view");
    _uniformCache.viewScaling = _program->uniformLocation("viewScaling");
    _uniformCache.projection = _program->uniformLocation("projection");
    _uniformCache.renderOption = _program->uniformLocation("renderOption");
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
    _uniformCache.maxStarsPerNode = _program->uniformLocation("maxStarsPerNode");
    _uniformCache.valuesPerStar = _program->uniformLocation("valuesPerStar");
    _uniformCache.nChunksToRender = _program->uniformLocation("nChunksToRender");

    _programTM = renderEngine.buildRenderProgram("ToneMapping",
        absPath("${MODULE_GAIAMISSION}/shaders/gaia_tonemapping_vs.glsl"),
        absPath("${MODULE_GAIAMISSION}/shaders/gaia_tonemapping_fs.glsl")
    );
    _uniformCacheTM.renderedTexture = _programTM->uniformLocation("renderedTexture");
    _uniformCacheTM.screenSize = _programTM->uniformLocation("screenSize");

    // Find out how much GPU memory this computer has (Nvidia cards).
    // TODO: use for streaming budget!
    GLint nTotalMemoryInKB = 0;
    glGetIntegerv(GL_GPU_MEMORY_INFO_TOTAL_AVAILABLE_MEMORY_NVX, &nTotalMemoryInKB);

    GLint nCurrentAvailMemoryInKB = 0;
    glGetIntegerv(GL_GPU_MEMORY_INFO_CURRENT_AVAILABLE_VIDMEM_NVX, &nCurrentAvailMemoryInKB);
    LINFO("nTotalMemoryInKB: " + std::to_string(nTotalMemoryInKB) +
        " - nCurrentAvailMemoryInKB: " + std::to_string(nCurrentAvailMemoryInKB));
    
    // Read data file. 
    bool success = readDataFile(gaiamission::RenderOption(static_cast<int>(_renderOption)));
    if (!success) {
        throw ghoul::RuntimeError("Error loading file data");
    }
}

void RenderableGaiaStars::deinitializeGL() {
    glDeleteBuffers(1, &_vboPos);
    _vboPos = 0;
    glDeleteBuffers(1, &_vboCol);
    _vboCol = 0;
    glDeleteBuffers(1, &_vboVel);
    _vboVel = 0;
    glDeleteBuffers(1, &_ssboIdx);
    _ssboIdx = 0;
    glDeleteBuffers(1, &_ssboData);
    _ssboData = 0;
    glDeleteVertexArrays(1, &_vao);
    _vao = 0;
    glDeleteBuffers(1, &_vboQuad);
    _vboQuad = 0;
    glDeleteVertexArrays(1, &_vaoQuad);
    _vaoQuad = 0;
    glDeleteFramebuffers(1, &_fbo);
    _fbo = 0;

    _dataFile = nullptr;
    _pointSpreadFunctionTexture = nullptr;
    _colorTexture = nullptr;
    _fboTexture = nullptr;

    RenderEngine& renderEngine = OsEng.renderEngine();
    if (_program) {
        renderEngine.removeRenderProgram(_program);
        _program = nullptr;
    }
    if (_programTM) {
        renderEngine.removeRenderProgram(_programTM);
        _programTM = nullptr;
    }
}

void RenderableGaiaStars::render(const RenderData& data, RendererTasks&) {
    
    // Save current FBO.
    GLint defaultFbo;
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFbo);

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
    const int option = _renderOption;
    int deltaStars = 0;
    auto updateData = _octreeManager->traverseData(modelViewProjMat, screenSize, deltaStars, 
        gaiamission::RenderOption(option));

    // Update number of rendered stars.
    int nStars = static_cast<int>(_nRenderedStars) + deltaStars;
    _nRenderedStars.set(nStars);
    
    //-------------------- FUNCTIONING RENDERING WITH SSBOS ---------------------------
    // Update SSBO Index array with accumulated stars in all chunks.
    glBindBuffer(GL_SHADER_STORAGE_BUFFER, _ssboIdx);
    int nChunksToRender = _octreeManager->biggestChunkIndexInUse();
    int maxStarsPerNode = _octreeManager->maxStarsPerNode();
    int lastValue = _accumulatedIndices.back();
    _accumulatedIndices.resize(nChunksToRender + 1, lastValue);
    
    // Update vector with accumulated indices.
    for (auto &[offset, subData] : updateData) {
        int newValue = (subData.size() / _nValuesInSlice) + _accumulatedIndices[offset];
        int changeInValue = newValue - _accumulatedIndices[offset + 1];
        _accumulatedIndices[offset + 1] = newValue;
        // Propagate change.
        for (int i = offset + 1; i < nChunksToRender; ++i) {
            _accumulatedIndices[i + 1] += changeInValue;
        }
    }

    // Fix number of stars rendered if it doesn't correspond to our buffers.
    if (_accumulatedIndices.back() != nStars) {
        _nRenderedStars.set(_accumulatedIndices.back());
    }

    // Update SSBO Index (stars per chunk).
    glBufferData(
        GL_SHADER_STORAGE_BUFFER,
        nChunksToRender * sizeof(GLint),
        _accumulatedIndices.data(),
        GL_STREAM_DRAW
    );

    // Use orphaning strategy for data SSBO.
    glBindBuffer(GL_SHADER_STORAGE_BUFFER, _ssboData);

    glBufferData(
        GL_SHADER_STORAGE_BUFFER,
        _streamingBudget * sizeof(GLfloat),
        nullptr,
        GL_STREAM_DRAW
    );

    // Update SSBO with one insert per chunk/node. The key in map holds the offset index.
    for (auto &[offset, subData] : updateData) {
        // We don't need to fill chunk with zeros anymore! Just check if we have any values to update.
        if (!subData.empty()) {
            std::vector<float> vectorData(subData.begin(), subData.end());
            int dataSize = vectorData.size();
            glBufferSubData(
                GL_SHADER_STORAGE_BUFFER,
                offset * _chunkSize * sizeof(GLfloat),
                dataSize * sizeof(GLfloat),
                vectorData.data()
            );
        }
    }

    glBindBuffer(GL_SHADER_STORAGE_BUFFER, 0);


    //------------------FUNCTIONING RENDERING WITH VBOS---------------------------
    // Update VBOs with new nodes. 
    // This will overwrite old data that's not visible anymore as well.
    /*glBindVertexArray(_vao);

    // Always update Position VBO.
    glBindBuffer(GL_ARRAY_BUFFER, _vboPos);
    size_t posChunkSize = _octreeManager->maxStarsPerNode() * _posSize;
    size_t posStreamingBudget = _octreeManager->totalNodes() * posChunkSize;

    // Use buffer orphaning to update a subset of total data.
    glBufferData(
        GL_ARRAY_BUFFER,
        posStreamingBudget * sizeof(GLfloat),
        nullptr,
        GL_STREAM_DRAW
    );

    // Update buffer with one insert per chunk/node. The key in map holds the offset index.
    for (auto & [offset, subData] : updateData) {
        // Fill chunk by appending zeroes to data so we overwrite possible earlier values.
        // Only required when removing nodes because chunks are filled up in octree fetch on add.
        std::vector<float> vectorData(subData.begin(), subData.end());
        vectorData.resize(posChunkSize, 0.f);
        glBufferSubData(
            GL_ARRAY_BUFFER,
            offset * posChunkSize * sizeof(GLfloat),
            posChunkSize * sizeof(GLfloat),
            vectorData.data()
        );
    }

    // Update Color VBO if render option is 'Color' or 'Motion'.
    if (option != gaiamission::RenderOption::Static) {
        glBindBuffer(GL_ARRAY_BUFFER, _vboCol);
        size_t colChunkSize = _octreeManager->maxStarsPerNode() * _colSize;
        size_t colStreamingBudget = _octreeManager->totalNodes() * colChunkSize;
        
        // Use buffer orphaning to update a subset of total data.
        glBufferData(
            GL_ARRAY_BUFFER,
            colStreamingBudget * sizeof(GLfloat),
            nullptr,
            GL_STREAM_DRAW
        );

        // Update buffer with one insert per chunk/node. The key in map holds the offset index.
        for (auto &[offset, subData] : updateData) {
            // Fill chunk by appending zeroes to data so we overwrite possible earlier values.
            std::vector<float> vectorData(subData.begin(), subData.end());
            vectorData.resize(posChunkSize + colChunkSize, 0.f);
            glBufferSubData(
                GL_ARRAY_BUFFER,
                offset * colChunkSize * sizeof(GLfloat),
                colChunkSize * sizeof(GLfloat),
                vectorData.data() + posChunkSize
            );
        }

        // Update Velocity VBO if specified.
        if (option == gaiamission::RenderOption::Motion) {
            glBindBuffer(GL_ARRAY_BUFFER, _vboVel);
            size_t velChunkSize = _octreeManager->maxStarsPerNode() * _velSize;
            size_t velStreamingBudget = _octreeManager->totalNodes() * velChunkSize;

            // Use buffer orphaning to update a subset of total data.
            glBufferData(
                GL_ARRAY_BUFFER,
                velStreamingBudget * sizeof(GLfloat),
                nullptr,
                GL_STREAM_DRAW
            );

            // Update buffer with one insert per chunk/node. The key in map holds the offset index.
            for (auto &[offset, subData] : updateData) {
                // Fill chunk by appending zeroes to data so we overwrite possible earlier values.
                std::vector<float> vectorData(subData.begin(), subData.end());
                vectorData.resize(_chunkSize, 0.f);
                glBufferSubData(
                    GL_ARRAY_BUFFER,
                    offset * velChunkSize * sizeof(GLfloat),
                    velChunkSize * sizeof(GLfloat),
                    vectorData.data() + posChunkSize + colChunkSize
                );
            }
        }
    }
    
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);*/

    GLenum error = glGetError();
    if (error != GL_NO_ERROR) {
        switch (error) {
        case GL_INVALID_ENUM:
            LINFO("1 - GL_INVALID_ENUM");
            break;
        case GL_INVALID_VALUE:
            LINFO("1 - GL_INVALID_VALUE");
            break;
        case GL_INVALID_OPERATION:
            LINFO("1 - GL_INVALID_OPERATION");
            break;
        case GL_INVALID_FRAMEBUFFER_OPERATION:
            LINFO("1 - GL_INVALID_FRAMEBUFFER_OPERATION");
            break;
        case GL_OUT_OF_MEMORY:
            LINFO("1 - GL_OUT_OF_MEMORY");
            break;
        default:
            LINFO("1 - Unknown error");
            break;
        }
    }
    
    // Activate shader program and send uniforms.
    glBlendFunc(GL_SRC_ALPHA, GL_ONE);
    glDepthMask(false);
    _program->activate();

    _program->setUniform(_uniformCache.model, model);
    _program->setUniform(_uniformCache.view, view);
    _program->setUniform(_uniformCache.viewScaling, viewScaling);
    _program->setUniform(_uniformCache.projection, projection);
    _program->setUniform(_uniformCache.renderOption, _renderOption);
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

    // Specify how many potential stars we have to render.
    GLsizei nStarsToRender = _nRenderedStars;
    int valuesPerStar = _nValuesInSlice;
    //GLsizei maxStarsToRender = maxStarsPerNode * nChunksToRender;
    _program->setUniform(_uniformCache.maxStarsPerNode, maxStarsPerNode);
    _program->setUniform(_uniformCache.valuesPerStar, valuesPerStar);
    _program->setUniform(_uniformCache.nChunksToRender, nChunksToRender);

    // Render to FBO.
    glBindFramebuffer(GL_FRAMEBUFFER, _fbo);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    
    //glEnable(GL_PROGRAM_POINT_SIZE);
    // A non-zero named vao MUST be bound!
    glBindVertexArray(_vao);
    glDrawArrays(GL_POINTS, 0, nStarsToRender); //maxStarsToRender
    glBindVertexArray(0);
    //glDisable(GL_PROGRAM_POINT_SIZE);
    _program->deactivate();

    // Use ToneMapping shaders and render to default FBO again!
    _programTM->activate();
    
    glBindFramebuffer(GL_FRAMEBUFFER, defaultFbo);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    ghoul::opengl::TextureUnit fboTexUnit;
    fboTexUnit.activate();
    _fboTexture->bind();
    _programTM->setUniform(_uniformCacheTM.renderedTexture, fboTexUnit);
    _programTM->setUniform(_uniformCacheTM.screenSize, screenSize);

    glBindVertexArray(_vaoQuad);
    glDrawArrays(GL_TRIANGLES, 0, 6); // 2 triangles
    glBindVertexArray(0);

    _programTM->deactivate();

    glDepthMask(true);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    error = glGetError();
    if (error != GL_NO_ERROR) {
        switch (error) {
        case GL_INVALID_ENUM:
            LINFO("4 - GL_INVALID_ENUM");
            break;
        case GL_INVALID_VALUE:
            LINFO("4 - GL_INVALID_VALUE");
            break;
        case GL_INVALID_OPERATION:
            LINFO("4 - GL_INVALID_OPERATION");
            break;
        case GL_INVALID_FRAMEBUFFER_OPERATION:
            LINFO("4 - GL_INVALID_FRAMEBUFFER_OPERATION");
            break;
        case GL_OUT_OF_MEMORY:
            LINFO("4 - GL_OUT_OF_MEMORY");
            break;
        default:
            LINFO("4 - Unknown error");
            break;
        }
    }
}

void RenderableGaiaStars::update(const UpdateData&) {
    if (_dataIsDirty) {
        LDEBUG("Regenerating data");
        const int option = _renderOption;

        // Reload data file (as long as it's not the first initialization)!
        if (_vao != 0) { 
            // This will reconstruct the Octree as well!
            bool success = readDataFile(gaiamission::RenderOption(option));
            if (!success) {
                throw ghoul::RuntimeError("Error loading FITS data");
            }
        }

        if (_vao == 0) {
            glGenVertexArrays(1, &_vao);
            LDEBUG(fmt::format("Generating Vertex Array id '{}'",  _vao));
        }
        if (_vboPos == 0) {
            glGenBuffers(1, &_vboPos);
            LDEBUG(fmt::format("Generating Position Vertex Buffer Object id '{}'", _vboPos));
        }
        if (_vboCol == 0) {
            glGenBuffers(1, &_vboCol);
            LDEBUG(fmt::format("Generating Color Vertex Buffer Object id '{}'", _vboCol));
        }
        if (_vboVel == 0) {
            glGenBuffers(1, &_vboVel);
            LDEBUG(fmt::format("Generating Velocity Vertex Buffer Object id '{}'", _vboVel));
        }
        if (_ssboIdx == 0) {
            glGenBuffers(1, &_ssboIdx);
            LDEBUG(fmt::format("Generating Index Shader Storage Buffer Object id '{}'", _ssboIdx));
        }
        if (_ssboData == 0) {
            glGenBuffers(1, &_ssboData);
            LDEBUG(fmt::format("Generating Data Shader Storage Buffer Object id '{}'", _ssboData));
        }
        if (_vaoQuad == 0) {
            glGenVertexArrays(1, &_vaoQuad);
            LDEBUG(fmt::format("Generating Quad Vertex Array id '{}'", _vaoQuad));
        }
        if (_vboQuad == 0) {
            glGenBuffers(1, &_vboQuad);
            LDEBUG(fmt::format("Generating Quad Vertex Buffer Object id '{}'", _vboQuad));
        }
        if (_fbo == 0) {
            glGenFramebuffers(1, &_fbo);
            LDEBUG(fmt::format("Generating Framebuffer Object id '{}'", _fbo));
        }
        if (!_fboTexture) {
            // Generate a new texture and attach it to our FBO. 
            glm::vec2 screenSize = glm::vec2(OsEng.renderEngine().renderingResolution());
            _fboTexture = std::make_unique<ghoul::opengl::Texture>(
                glm::uvec3(screenSize, 1), ghoul::opengl::Texture::Format::RGBA, GL_RGBA32F,
                GL_FLOAT
            );
            _fboTexture->uploadTexture();
            LDEBUG("Generating Framebuffer Texture!");
        }

        // Bind SSBO blocks to our shader positions.
        // Number of stars per chunk (a.k.a. Index).
        glBindBuffer(GL_SHADER_STORAGE_BUFFER, _ssboIdx);
        
        _ssboIdxBinding = std::make_unique<ghoul::opengl::BufferBinding<
            ghoul::opengl::bufferbinding::Buffer::ShaderStorage>>();
        glBindBufferBase(GL_SHADER_STORAGE_BUFFER, _ssboIdxBinding->bindingNumber(), _ssboIdx);
        _program->setSsboBinding("ssbo_idx_data", _ssboIdxBinding->bindingNumber());

        // Combined SSBO with all data.
        glBindBuffer(GL_SHADER_STORAGE_BUFFER, _ssboData);

        _ssboDataBinding = std::make_unique<ghoul::opengl::BufferBinding<
            ghoul::opengl::bufferbinding::Buffer::ShaderStorage>>();
        glBindBufferBase(GL_SHADER_STORAGE_BUFFER, _ssboDataBinding->bindingNumber(), _ssboData);
        _program->setSsboBinding("ssbo_comb_data", _ssboDataBinding->bindingNumber());
        
        glBindBuffer(GL_SHADER_STORAGE_BUFFER, 0);

        // Bind our different VBOs to our vertex array layout. 
        /*glBindVertexArray(_vao);

        switch (option) {
        case gaiamission::RenderOption::Static: {
            glBindBuffer(GL_ARRAY_BUFFER, _vboPos);
            GLint positionAttrib = _program->attributeLocation("in_position");
            glEnableVertexAttribArray(positionAttrib);

            glVertexAttribPointer(
                positionAttrib,
                _posSize,
                GL_FLOAT,
                GL_FALSE,
                0,
                nullptr
            );

            break;
        } 
        case gaiamission::RenderOption::Color: {
            glBindBuffer(GL_ARRAY_BUFFER, _vboPos);
            GLint positionAttrib = _program->attributeLocation("in_position");
            glEnableVertexAttribArray(positionAttrib);

            glVertexAttribPointer(
                positionAttrib,
                _posSize,
                GL_FLOAT,
                GL_FALSE,
                0,
                nullptr
            );

            glBindBuffer(GL_ARRAY_BUFFER, _vboCol);
            GLint brightnessDataAttrib = _program->attributeLocation("in_brightness");
            glEnableVertexAttribArray(brightnessDataAttrib);

            glVertexAttribPointer(
                brightnessDataAttrib,
                _colSize,
                GL_FLOAT,
                GL_FALSE,
                0,
                nullptr
            );
            break;
        }
        case gaiamission::RenderOption::Motion: {
            glBindBuffer(GL_ARRAY_BUFFER, _vboPos);
            GLint positionAttrib = _program->attributeLocation("in_position");
            glEnableVertexAttribArray(positionAttrib);

            glVertexAttribPointer(
                positionAttrib,
                _posSize,
                GL_FLOAT,
                GL_FALSE,
                0,
                nullptr
            );

            glBindBuffer(GL_ARRAY_BUFFER, _vboCol);
            GLint brightnessDataAttrib = _program->attributeLocation("in_brightness");
            glEnableVertexAttribArray(brightnessDataAttrib);

            glVertexAttribPointer(
                brightnessDataAttrib,
                _colSize,
                GL_FLOAT,
                GL_FALSE,
                0,
                nullptr
            );

            glBindBuffer(GL_ARRAY_BUFFER, _vboVel);
            GLint velocityAttrib = _program->attributeLocation("in_velocity");
            glEnableVertexAttribArray(velocityAttrib);

            glVertexAttribPointer(
                velocityAttrib,
                _velSize,
                GL_FLOAT,
                GL_FALSE,
                0,
                nullptr 
            );
            break;
        }
        }

        glBindBuffer(GL_ARRAY_BUFFER, 0);
        glBindVertexArray(0);
        */

        // Bind VBO and VAO for Quad rendering.
        glBindVertexArray(_vaoQuad);
        glBindBuffer(GL_ARRAY_BUFFER, _vboQuad);

        // Quad for fullscreen.
        static const GLfloat vbo_quad_data[] = {
            -1.0f, -1.0f, 0.0f,
            1.0f, -1.0f, 0.0f,
            -1.0f,  1.0f, 0.0f,
            -1.0f,  1.0f, 0.0f,
            1.0f, -1.0f, 0.0f,
            1.0f,  1.0f, 0.0f,
        };

        glBufferData(
            GL_ARRAY_BUFFER,
            sizeof(vbo_quad_data),
            vbo_quad_data,
            GL_STATIC_DRAW
        );

        GLint tmPositionAttrib = _programTM->attributeLocation("in_position");
        glEnableVertexAttribArray(tmPositionAttrib);
        glVertexAttribPointer(
            tmPositionAttrib,
            3,
            GL_FLOAT,
            GL_FALSE,
            0,
            nullptr
        );

        glEnableVertexAttribArray(0);
        glBindBuffer(GL_ARRAY_BUFFER, 0);
        glBindVertexArray(0);

        // Bind render texture to FBO. 
        // TODO: Should I have a depth buffer as well for Fragment depth in 2nd pass!?
        glBindFramebuffer(GL_FRAMEBUFFER, _fbo);
        glBindTexture(GL_TEXTURE_2D, *_fboTexture);
        glFramebufferTexture(
            GL_FRAMEBUFFER,
            GL_COLOR_ATTACHMENT0,
            *_fboTexture,
            0);
        GLenum textureBuffers[1] = { GL_COLOR_ATTACHMENT0 };
        glDrawBuffers(1, textureBuffers);

        // Check that our framebuffer is ok.
        if (glCheckFramebufferStatus(GL_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE) {
            LERROR("Error when generating GaiaStar Framebuffer.");
        }
        glBindFramebuffer(GL_FRAMEBUFFER, 0);

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
        _uniformCache.renderOption = _program->uniformLocation("renderOption");
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
        _uniformCache.maxStarsPerNode = _program->uniformLocation("maxStarsPerNode");
        _uniformCache.valuesPerStar = _program->uniformLocation("valuesPerStar");
        _uniformCache.nChunksToRender = _program->uniformLocation("nChunksToRender");

        _program->setSsboBinding("ssbo_idx_data", _ssboIdxBinding->bindingNumber());
        _program->setSsboBinding("ssbo_comb_data", _ssboDataBinding->bindingNumber());
    }

    if (_programTM->isDirty()) {
        _programTM->rebuildFromFile();
        _uniformCacheTM.renderedTexture = _programTM->uniformLocation("renderedTexture");
        _uniformCacheTM.screenSize = _programTM->uniformLocation("screenSize");
    }

    if (OsEng.windowWrapper().windowHasResized()) {
        // Update FBO texture resolution if we haven't already.
        glm::vec2 screenSize = glm::vec2(OsEng.renderEngine().renderingResolution());
        if ( glm::any(glm::notEqual(_fboTexture->dimensions(), glm::uvec3(screenSize, 1.0)))) {
            _fboTexture = std::make_unique<ghoul::opengl::Texture>(
                glm::uvec3(screenSize, 1), ghoul::opengl::Texture::Format::RGBA, GL_RGBA32F,
                GL_FLOAT
                );
            _fboTexture->uploadTexture();
            LDEBUG("Re-Generating Gaia Framebuffer Texture!");

            glBindFramebuffer(GL_FRAMEBUFFER, _fbo);
            glBindTexture(GL_TEXTURE_2D, *_fboTexture);
            glFramebufferTexture(
                GL_FRAMEBUFFER,
                GL_COLOR_ATTACHMENT0,
                *_fboTexture,
                0);
            GLenum textureBuffers[1] = { GL_COLOR_ATTACHMENT0 };
            glDrawBuffers(1, textureBuffers);

            // Check that our framebuffer is ok.
            if (glCheckFramebufferStatus(GL_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE) {
                LERROR("Error when re-generating GaiaStar Framebuffer.");
            }
            glBindFramebuffer(GL_FRAMEBUFFER, 0);
        }
    }
}

bool RenderableGaiaStars::readDataFile(gaiamission::RenderOption option) {
    std::string _file = _filePath;
    _fullData.clear();
    _octreeManager->initOctree();

    LINFO("Loading data file: " + _file);

    // Read from binary if file has been preprocessed, else read from FITS file.
    if (_filePreprocessed) {
        std::ifstream fileStream(_file, std::ifstream::binary);
        if (fileStream.good()) {

            // Let's assume we've already contructed an Octree!
            _octreeManager->readFromFile(fileStream);

            if (option == gaiamission::RenderOption::Static) {
                _nValuesInSlice = 3;
            }
            else if (option == gaiamission::RenderOption::Color) {
                _nValuesInSlice = 5;
            }
            else { // (option == gaiamission::RenderOption::Motion)
                _nValuesInSlice = 8;
            }

            // Else we're reading from a BIN file as before.
            /*else {
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

                    // TODO: This doesn't work anymore!!!! (Order already different!)
                    // Data needs to be sliced differently depending on file origin.
                    auto slicedValues = std::vector<float>();
                    if (_fileTypeOrigin == FITS) {
                        slicedValues = sliceFitsValues(option, values);
                    }
                    else if (_fileTypeOrigin == SPECK) {
                        slicedValues = sliceSpeckStars(option, values);
                    }
                    else {
                        LERROR("User did not specify correct origin of preprocessed file.");
                    }

                    _nValuesInSlice = slicedValues.size(); // Unnecessary to do for every star.
                    _octreeManager->insert(slicedValues);
                }
            }*/
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

        _nValuesPerStar = _columnNames.size() + 1; // +1 for B-V color value.
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

            // Store positions.
            values[idx++] = posXcol[i];
            values[idx++] = posYcol[i];
            values[idx++] = posZcol[i];

            // Return early if star doesn't have a measured position.
            if (values[0] == -999 && values[1] == -999 && values[2] == -999) {
                nNullArr++;
                continue;
            }

            // Store color values.
            values[idx++] = magCol[i];
            values[idx++] = tycho_b[i] - tycho_v[i];

            // Store velocity convert it with parallax.
            values[idx++] = convertMasPerYearToMeterPerSecond(velXcol[i], parallax[i]);
            values[idx++] = convertMasPerYearToMeterPerSecond(velYcol[i], parallax[i]);
            values[idx++] = convertMasPerYearToMeterPerSecond(velZcol[i], parallax[i]);

            // Store additional parameters to filter by.
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
            auto slicedValues = sliceFitsValues(option, values); 
            _nValuesInSlice = slicedValues.size(); // Unnecessary to do for every star.

            // TODO: Insert into correct subfile & sort in Morton order (z-order)!?
            _octreeManager->insert(slicedValues);
        }
        LINFO(std::to_string(nNullArr) + " out of " + std::to_string(nStars) +
            " read stars were nullArrays");
    }

    // TODO: Improve streaming budget!
    // Init rendering info and VBO stack in Octree. 
    _chunkSize = _octreeManager->maxStarsPerNode() * _nValuesInSlice;
    _streamingBudget = _octreeManager->totalNodes() * _chunkSize;
    _streamingBudget = std::min(_streamingBudget, _memoryBudgetInValues);
    int maxNodesInStream = static_cast<int>(_streamingBudget / _chunkSize);

    LINFO("Chunk size: " + std::to_string(_chunkSize) +
        " Streaming budget: " + std::to_string(_streamingBudget) +
        " Max Nodes in stream: " + std::to_string(maxNodesInStream));

    _octreeManager->initVBOIndexStack(maxNodesInStream);
    _nRenderedStars.set(0);

    //_octreeManager->printStarsPerNode();
   
    return true;
}

// Slices every star seperately, from FITS file, before they are inserted into Octree. 
// Conversion of position is done in vertex shader to simplify calculations in Octree.
std::vector<float> RenderableGaiaStars::sliceFitsValues(gaiamission::RenderOption option,
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
    case gaiamission::RenderOption::Static: {
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
    case gaiamission::RenderOption::Color: {
        union {
            ColorVBOLayout value;
            std::array<float, sizeof(ColorVBOLayout) / sizeof(float)> data;
        } layout;

        layout.value.position = { {
                position[0], position[1], position[2]
            } };
        layout.value.magnitude = starValues[6];

        // B-V color is Blue minus Visible filter magnitudes
        layout.value.bvColor = starValues[13] - starValues[15];

        tmpData.insert(tmpData.end(), layout.data.begin(), layout.data.end());
        break;
    }
    case gaiamission::RenderOption::Motion: {
        union {
            MotionVBOLayout value;
            std::array<float, sizeof(MotionVBOLayout) / sizeof(float)> data;
        } layout;

        layout.value.position = { {
                position[0], position[1], position[2]
            } };

        layout.value.magnitude = starValues[6];

        // B-V color is Blue minus Visible filter magnitudes
        layout.value.bvColor = starValues[13] - starValues[15];

        layout.value.velocity = { {
                velocity[0], velocity[1], velocity[2]
            } };

        tmpData.insert(tmpData.end(), layout.data.begin(), layout.data.end());
        break;
    }
    }
    return tmpData;
}

// Slices every star seperately, from SPECK file, before they are inserted into Octree. 
// Conversion of position is done in vertex shader to simplify calculations in Octree.
std::vector<float> RenderableGaiaStars::sliceSpeckStars(gaiamission::RenderOption option,
    std::vector<float> starValues) {

    auto tmpData = std::vector<float>();

    // Conversion kiloparsecs -> meter is done in vertex shader.
    glm::vec3 position = glm::vec3(starValues[0], starValues[1], starValues[2]);
    //position *= 1000 * static_cast<float>(distanceconstants::Parsec);

    // TODO: Unclear which columns are velocity (and what unit in that case)!'
    // Right now we're using [U,V,W] from speck file. 
    glm::vec3 velocity = glm::vec3(starValues[9], starValues[10], starValues[11]);

    switch (option) {
    case gaiamission::RenderOption::Static: {
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
    case gaiamission::RenderOption::Color: {
        union {
            ColorVBOLayout value;
            std::array<float, sizeof(ColorVBOLayout) / sizeof(float)> data;
        } layout;

        layout.value.position = { {
                position[0], position[1], position[2]
            } };
        
        // Luminosity is in starValues[4].
        // However, we're already doing those computations in the shader from magnitude.
        layout.value.magnitude = starValues[5];
        // B-V color is Blue minus Visible filter magnitudes.
        // Already computed in speck file. 
        layout.value.bvColor = starValues[3];

        tmpData.insert(tmpData.end(), layout.data.begin(), layout.data.end());
        break;
    }
    case gaiamission::RenderOption::Motion: {
        union {
            MotionVBOLayout value;
            std::array<float, sizeof(MotionVBOLayout) / sizeof(float)> data;
        } layout;

        layout.value.position = { {
                position[0], position[1], position[2]
            } };

        // Luminosity is in starValues[4].
        // However, we're already doing those computations in the shader from magnitude.
        layout.value.magnitude = starValues[5];
        // B-V color is Blue minus Visible filter magnitudes.
        // Already computed in speck file. 
        layout.value.bvColor = starValues[3];

        layout.value.velocity = { {
                velocity[0], velocity[1], velocity[2]
            } };

        tmpData.insert(tmpData.end(), layout.data.begin(), layout.data.end());
        break;
    }
    }
    return tmpData;
}

} // namespace openspace
