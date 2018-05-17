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
#include <ghoul/systemcapabilities/generalcapabilitiescomponent.h>

#include <array>
#include <fstream>
#include <stdint.h>

namespace {
    constexpr const char* _loggerCat = "RenderableGaiaStars";

    static const openspace::properties::Property::PropertyInfo FilePathInfo = {
        "File",
        "File Path",
        "The path to the file with data for the stars to be rendered."
    }; 

    static const openspace::properties::Property::PropertyInfo FileReaderOptionInfo = {
        "FileReaderOption",
        "File Reader Option",
        "This value tells the renderable what format the input data file has. "
        "'Fits' will read a FITS file, construct an Octree from it and render full data. "
        "'Speck' will read a SPECK file, construct an Octree from it and render full data. "
        "'BinaryRaw' will read a preprocessed binary file with ordered star data, construct "
        "and Octree and render it. "
        "'BinaryOctree' will read a constructed Octree from binary file and render full data. "
        "'StreamOctree' will read an index file with full Octree structure and them stream nodes "
        "during runtime. Suited for bigger datasets."
    };

    static const openspace::properties::Property::PropertyInfo RenderOptionInfo = {
        "RenderOption",
        "Render Option",
        "This value determines which predefined columns to use in rendering. If 'Static' "
        "only the position of the stars is used. 'Color' uses position + color parameters "
        "and 'Motion' uses pos, color as well as velocity for the stars."
    };

    static const openspace::properties::Property::PropertyInfo ShaderOptionInfo = {
        "ShaderOption",
        "Shader Option",
        "This value determines which shaders to use while rendering. If 'Point_*' is chosen "
        "then gl_Points will be rendered and then spread out with a bloom filter. If 'Billboard_*' "
        "is chosen then the geometry shaders will generate screen-faced billboards for all stars. "
        "For '*_SSBO' the data will be stored in Shader Storage Buffer Objects will '*_VBO' uses "
        "Vertex Buffer Objects for the streaming."
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

    static const openspace::properties::Property::PropertyInfo CpuRamBudgetInfo = {
        "CpuRamBudget",
        "CPU RAM Budget",
        "Current remaining budget [bytes] on the CPU RAM for loading more node data files."
    };

    static const openspace::properties::Property::PropertyInfo SsboStreamBudgetInfo = {
        "SsboStreamBudget",
        "SSBO Star Stream Budget",
        "Current remaining memory budget [in number of stars] on the GPU for streaming "
        "additional stars."
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
                FileReaderOptionInfo.identifier,
                new StringInListVerifier({
                    "Fits", "Speck", "BinaryRaw", "BinaryOctree", "StreamOctree"
                }),
                Optional::No,
                FileReaderOptionInfo.description
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
                ShaderOptionInfo.identifier,
                new StringInListVerifier({
                    "Point_SSBO", "Point_VBO", "Billboard_SSBO", "Billboard_VBO"
                }),
                Optional::Yes,
                ShaderOptionInfo.description
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
    , _dataFile(nullptr)
    , _dataIsDirty(true)
    , _buffersAreDirty(true)
    , _shadersAreDirty(false)
    , _fileReaderOption(FileReaderOptionInfo, properties::OptionProperty::DisplayType::Dropdown)
    , _renderOption(RenderOptionInfo, properties::OptionProperty::DisplayType::Dropdown)
    , _shaderOption(ShaderOptionInfo, properties::OptionProperty::DisplayType::Dropdown)
    , _pointSpreadFunctionTexturePath(PsfTextureInfo)
    , _pointSpreadFunctionTexture(nullptr)
    , _pointSpreadFunctionTextureIsDirty(true)
    , _colorTexturePath(ColorTextureInfo)
    , _colorTexture(nullptr)
    , _colorTextureIsDirty(true)
    , _luminosityMultiplier(LuminosityMultiplierInfo, 1200.f, 1.f, 100000.f)
    , _magnitudeBoost(MagnitudeBoostInfo, 25.f, 0.f, 100.f)
    , _cutOffThreshold(CutOffThresholdInfo, 38.f, 0.f, 50.f)
    , _sharpness(SharpnessInfo, 1.45f, 0.f, 5.f)
    , _billboardSize(BillboardSizeInfo, 10.f, 1.f, 100.f)
    , _closeUpBoostDist(CloseUpBoostDistInfo, 300.f, 1.f, 1000.f)
    , _firstRow(FirstRowInfo, 0, 0, 2539913) // DR1-max: 2539913
    , _lastRow(LastRowInfo, 0, 0, 2539913)
    , _columnNamesList(ColumnNamesInfo)
    , _nRenderedStars(NumRenderedStarsInfo, 0, 0, 2000000000) // 2 Billion stars
    , _cpuRamBudgetProperty(CpuRamBudgetInfo, 0, 0, 1)
    , _ssboStreamBudgetProperty(SsboStreamBudgetInfo, 0, 0, 1)
    , _nStarsToRender(0)
    , _program(nullptr)
    , _programTM(nullptr)
    , _fboTexture(nullptr)
    , _nRenderValuesPerStar(0)
    , _firstDrawCalls(true)
    , _initialDataFilesLoaded(true)
    , _useVBO(false)
    , _cpuRamBudgetInBytes(0)
    , _totalDatasetSizeInBytes(0)
    , _gpuMemoryBudgetInBytes(0)
    , _maxStreamingBudgetInBytes(0)
    , _chunkSize(0)
    , _vao(0)
    , _vaoEmpty(0)
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
    _accumulatedIndices = std::vector<int>(1, 0);
    _previousCameraRotation = glm::dquat();

    _filePath = absPath(dictionary.value<std::string>(FilePathInfo.identifier));
    _dataFile = std::make_unique<File>(_filePath);

    _filePath.onChange(
        [&] { _dataIsDirty = true; }
    );
    _dataFile->setCallback(
        [&](const File&) { _dataIsDirty = true; }
    );
    addProperty(_filePath);

    _fileReaderOption.addOptions({
        { FileReaderOption::Fits, "Fits" },
        { FileReaderOption::Speck, "Speck" },
        { FileReaderOption::BinaryRaw, "BinaryRaw" },
        { FileReaderOption::BinaryOctree, "BinaryOctree" },
        { FileReaderOption::StreamOctree, "StreamOctree" }
        });
    if (dictionary.hasKey(FileReaderOptionInfo.identifier)) {
        const std::string fileReaderOption = dictionary.value<std::string>(FileReaderOptionInfo.identifier);
        if (fileReaderOption == "Fits") {
            _fileReaderOption = FileReaderOption::Fits;
        }
        else if (fileReaderOption == "Speck") {
            _fileReaderOption = FileReaderOption::Speck;
        }
        else if (fileReaderOption == "BinaryRaw") {
            _fileReaderOption = FileReaderOption::BinaryRaw;
        }
        else if (fileReaderOption == "BinaryOctree") {
            _fileReaderOption = FileReaderOption::BinaryOctree;
        }
        else {
            _fileReaderOption = FileReaderOption::StreamOctree;
        }
    }

    _renderOption.addOptions({
        { gaiamission::RenderOption::Static, "Static" },
        { gaiamission::RenderOption::Color, "Color" },
        { gaiamission::RenderOption::Motion, "Motion" }
    });
    if (dictionary.hasKey(RenderOptionInfo.identifier)) {
        const std::string renderOption = dictionary.value<std::string>(RenderOptionInfo.identifier);
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
    _renderOption.onChange([&] { _buffersAreDirty = true; });
    addProperty(_renderOption);

    _shaderOption.addOptions({
        { ShaderOption::Point_SSBO, "Point_SSBO" },
        { ShaderOption::Point_VBO, "Point_VBO" },
        { ShaderOption::Billboard_SSBO, "Billboard_SSBO" },
        { ShaderOption::Billboard_VBO, "Billboard_VBO" }
        });
    if (dictionary.hasKey(ShaderOptionInfo.identifier)) {
        const std::string shaderOption = dictionary.value<std::string>(ShaderOptionInfo.identifier);
        if (shaderOption == "Point_SSBO") {
            _shaderOption = ShaderOption::Point_SSBO;
        }
        else if (shaderOption == "Point_VBO") {
            _shaderOption = ShaderOption::Point_VBO;
        }
        else if (shaderOption == "Billboard_SSBO") {
            _shaderOption = ShaderOption::Billboard_SSBO;
        }
        else {
            _shaderOption = ShaderOption::Billboard_VBO;
        }
    }
    _shaderOption.onChange([&] { _buffersAreDirty = true; _shadersAreDirty = true; });
    addProperty(_shaderOption);

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

    // Only add properties correlated to fits files if we're actually reading from a fits file.
    if (_fileReaderOption == FileReaderOption::Fits) {
        if (dictionary.hasKey(FirstRowInfo.identifier)) {
            _firstRow = static_cast<int>(dictionary.value<double>(FirstRowInfo.identifier));
        }
        _firstRow.onChange([&] { _dataIsDirty = true; });
        addProperty(_firstRow);
        
        if (dictionary.hasKey(LastRowInfo.identifier)) {
            _lastRow = static_cast<int>(dictionary.value<double>(LastRowInfo.identifier));
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
            // OBS - This is not used atm!
        }

        if (_firstRow > _lastRow) {
            throw ghoul::RuntimeError("User defined FirstRow is bigger than LastRow.");
        }
    }

    // Add a read-only property for the number of rendered stars per frame.
    _nRenderedStars.setReadOnly(true);
    addProperty(_nRenderedStars);

    // Add CPU RAM Budget Property and SSBO Star Stream Property to menu.
    _cpuRamBudgetProperty.setReadOnly(true);
    addProperty(_cpuRamBudgetProperty);
    _ssboStreamBudgetProperty.setReadOnly(true);
    addProperty(_ssboStreamBudgetProperty);
}

RenderableGaiaStars::~RenderableGaiaStars() {}

bool RenderableGaiaStars::isReady() const {
    return _program && _programTM && _octreeManager && _initialDataFilesLoaded;
}

void RenderableGaiaStars::initializeGL() {
    RenderEngine& renderEngine = OsEng.renderEngine();
    //using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;
    //_program->setIgnoreUniformLocationError(IgnoreError::Yes);

    // Construct shader program depending on user-defined shader option.
    const int option = _shaderOption;
    switch (option) {
    case ShaderOption::Point_SSBO: {
        _program = ghoul::opengl::ProgramObject::Build(
            "GaiaStar",
            absPath("${MODULE_GAIAMISSION}/shaders/gaia_ssbo_vs.glsl"),
            absPath("${MODULE_GAIAMISSION}/shaders/gaia_point_fs.glsl"),
            absPath("${MODULE_GAIAMISSION}/shaders/gaia_point_ge.glsl")
        );
        _uniformCache.maxStarsPerNode = _program->uniformLocation("maxStarsPerNode");
        _uniformCache.valuesPerStar = _program->uniformLocation("valuesPerStar");
        _uniformCache.nChunksToRender = _program->uniformLocation("nChunksToRender");

        _programTM = renderEngine.buildRenderProgram("ToneMapping",
            absPath("${MODULE_GAIAMISSION}/shaders/gaia_tonemapping_vs.glsl"),
            absPath("${MODULE_GAIAMISSION}/shaders/gaia_tonemapping_point_fs.glsl")
        );
        _uniformCacheTM.screenSize = _programTM->uniformLocation("screenSize");
        break;
    }
    case ShaderOption::Point_VBO: {
        _program = ghoul::opengl::ProgramObject::Build(
            "GaiaStar",
            absPath("${MODULE_GAIAMISSION}/shaders/gaia_vbo_vs.glsl"),
            absPath("${MODULE_GAIAMISSION}/shaders/gaia_point_fs.glsl"),
            absPath("${MODULE_GAIAMISSION}/shaders/gaia_point_ge.glsl")
        );

        _programTM = renderEngine.buildRenderProgram("ToneMapping",
            absPath("${MODULE_GAIAMISSION}/shaders/gaia_tonemapping_vs.glsl"),
            absPath("${MODULE_GAIAMISSION}/shaders/gaia_tonemapping_point_fs.glsl")
        );
        _uniformCacheTM.screenSize = _programTM->uniformLocation("screenSize");
        break;
    }
    case ShaderOption::Billboard_SSBO: {
        _program = ghoul::opengl::ProgramObject::Build(
            "GaiaStar",
            absPath("${MODULE_GAIAMISSION}/shaders/gaia_ssbo_vs.glsl"),
            absPath("${MODULE_GAIAMISSION}/shaders/gaia_billboard_fs.glsl"),
            absPath("${MODULE_GAIAMISSION}/shaders/gaia_billboard_ge.glsl")
        );
        _uniformCache.magnitudeBoost = _program->uniformLocation("magnitudeBoost");
        _uniformCache.sharpness = _program->uniformLocation("sharpness");
        _uniformCache.billboardSize = _program->uniformLocation("billboardSize");
        _uniformCache.closeUpBoostDist = _program->uniformLocation("closeUpBoostDist");
        _uniformCache.screenSize = _program->uniformLocation("screenSize");
        _uniformCache.psfTexture = _program->uniformLocation("psfTexture");

        _uniformCache.maxStarsPerNode = _program->uniformLocation("maxStarsPerNode");
        _uniformCache.valuesPerStar = _program->uniformLocation("valuesPerStar");
        _uniformCache.nChunksToRender = _program->uniformLocation("nChunksToRender");

        _programTM = renderEngine.buildRenderProgram("ToneMapping",
            absPath("${MODULE_GAIAMISSION}/shaders/gaia_tonemapping_vs.glsl"),
            absPath("${MODULE_GAIAMISSION}/shaders/gaia_tonemapping_billboard_fs.glsl")
        );
        break;
    }
    case ShaderOption::Billboard_VBO: {
        _program = ghoul::opengl::ProgramObject::Build(
            "GaiaStar",
            absPath("${MODULE_GAIAMISSION}/shaders/gaia_vbo_vs.glsl"),
            absPath("${MODULE_GAIAMISSION}/shaders/gaia_billboard_fs.glsl"),
            absPath("${MODULE_GAIAMISSION}/shaders/gaia_billboard_ge.glsl")
        );
        _uniformCache.magnitudeBoost = _program->uniformLocation("magnitudeBoost");
        _uniformCache.sharpness = _program->uniformLocation("sharpness");
        _uniformCache.billboardSize = _program->uniformLocation("billboardSize");
        _uniformCache.closeUpBoostDist = _program->uniformLocation("closeUpBoostDist");
        _uniformCache.screenSize = _program->uniformLocation("screenSize");
        _uniformCache.psfTexture = _program->uniformLocation("psfTexture");

        _programTM = renderEngine.buildRenderProgram("ToneMapping",
            absPath("${MODULE_GAIAMISSION}/shaders/gaia_tonemapping_vs.glsl"),
            absPath("${MODULE_GAIAMISSION}/shaders/gaia_tonemapping_billboard_fs.glsl")
        );
        break;
    }
    }

    // Common uniforms for all shaders:
    _uniformCache.model = _program->uniformLocation("model");
    _uniformCache.view = _program->uniformLocation("view");
    _uniformCache.projection = _program->uniformLocation("projection");
    _uniformCache.time = _program->uniformLocation("time");
    _uniformCache.renderOption = _program->uniformLocation("renderOption");
    _uniformCache.viewScaling = _program->uniformLocation("viewScaling");
    _uniformCache.cutOffThreshold = _program->uniformLocation("cutOffThreshold");
    _uniformCache.luminosityMultiplier = _program->uniformLocation("luminosityMultiplier");
    _uniformCache.colorTexture = _program->uniformLocation("colorTexture");

    _uniformCacheTM.renderedTexture = _programTM->uniformLocation("renderedTexture");


    // Find out how much GPU memory this computer has (Nvidia cards).
    GLint nDedicatedVidMemoryInKB = 0;
    glGetIntegerv(GL_GPU_MEMORY_INFO_DEDICATED_VIDMEM_NVX, &nDedicatedVidMemoryInKB);
    GLint nTotalMemoryInKB = 0;
    glGetIntegerv(GL_GPU_MEMORY_INFO_TOTAL_AVAILABLE_MEMORY_NVX, &nTotalMemoryInKB);
    GLint nCurrentAvailMemoryInKB = 0;
    glGetIntegerv(GL_GPU_MEMORY_INFO_CURRENT_AVAILABLE_VIDMEM_NVX, &nCurrentAvailMemoryInKB);

    LINFO("nDedicatedVidMemoryInKB: " + std::to_string(nDedicatedVidMemoryInKB) + 
        " - nTotalMemoryInKB: " + std::to_string(nTotalMemoryInKB) +
        " - nCurrentAvailMemoryInKB: " + std::to_string(nCurrentAvailMemoryInKB));
    
    // Set ceiling for video memory to use in streaming.
    float currentVidMem = static_cast<float>(static_cast<long long>(nCurrentAvailMemoryInKB) * 1024);
    _gpuMemoryBudgetInBytes = static_cast<long long>(currentVidMem * MAX_GPU_MEMORY_PERCENT);

    // Set ceiling for how much of the installed CPU RAM to use for streaming. 
    long long installedRam = static_cast<long long>(CpuCap.installedMainMemory()) * 1024 * 1024;
    _cpuRamBudgetInBytes = static_cast<long long>(
        static_cast<float>(installedRam) * MAX_CPU_RAM_PERCENT);
    _cpuRamBudgetProperty.setMaxValue(static_cast<float>(_cpuRamBudgetInBytes));

    LINFO("GPU Memory Budget {bytes}: " + std::to_string(_gpuMemoryBudgetInBytes) +
        " - CPU RAM Budget {bytes}: " + std::to_string(_cpuRamBudgetInBytes));
}

void RenderableGaiaStars::deinitializeGL() {
    if (_vboPos != 0) {
        glDeleteBuffers(1, &_vboPos);
        _vboPos = 0;
    }
    if (_vboCol != 0) {
        glDeleteBuffers(1, &_vboCol);
        _vboCol = 0;
    }
    if (_vboVel != 0) {
        glDeleteBuffers(1, &_vboVel);
        _vboVel = 0;
    }
    if (_ssboIdx != 0) {
        glDeleteBuffers(1, &_ssboIdx);
        _ssboIdx = 0;
        glDeleteBuffers(1, &_ssboData);
        _ssboData = 0;
    }
    if (_vao != 0) {
        glDeleteVertexArrays(1, &_vao);
        _vao = 0;
    }
    if (_vaoEmpty != 0) {
        glDeleteVertexArrays(1, &_vaoEmpty);
        _vaoEmpty = 0;
    }

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
        renderEngine.removeRenderProgram(_program.get());
        _program = nullptr;
    }
    if (_programTM) {
        renderEngine.removeRenderProgram(_programTM.get());
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

    float viewScaling = data.camera.scaling();
    glm::mat4 view = data.camera.combinedViewMatrix();
    glm::mat4 projection = data.camera.projectionMatrix();

    glm::mat4 modelViewProjMat = projection * view * model;
    glm::vec2 screenSize = glm::vec2(OsEng.renderEngine().renderingResolution());

    // Wait until camera has stabilized before we traverse the Octree/stream from files.
    float rotationDiff = abs(length(_previousCameraRotation) - length(data.camera.rotationQuaternion()));
    if (_firstDrawCalls && rotationDiff > 1e-10) {
        _previousCameraRotation = data.camera.rotationQuaternion();
        return;
    } 
    else _firstDrawCalls = false;

    // Update which nodes that are stored in memory as the camera moves around (if streaming).
    if (_fileReaderOption == FileReaderOption::StreamOctree) {
        glm::dvec3 cameraPos = data.camera.positionVec3();
        glm::dvec3 cameraViewDir = data.camera.viewDirectionWorldSpace();
        _octreeManager->fetchSurroundingNodes(cameraPos, cameraViewDir);

        // Update CPU Budget property.
        _cpuRamBudgetProperty.set(static_cast<float>(_octreeManager->cpuRamBudget()));
    }

    // Traverse Octree and build a map with new nodes to render, uses mvp matrix to decide.
    const int renderOption = _renderOption;
    int deltaStars = 0;
    auto updateData = _octreeManager->traverseData(modelViewProjMat, screenSize, deltaStars, 
        gaiamission::RenderOption(renderOption));

    // Update number of rendered stars.
    _nStarsToRender += deltaStars;
    _nRenderedStars.set(_nStarsToRender);

    int nChunksToRender = _octreeManager->biggestChunkIndexInUse();
    int maxStarsPerNode = _octreeManager->maxStarsPerNode();
    int valuesPerStar = _nRenderValuesPerStar;

    // Switch rendering technique depending on user-defined shader option.
    const int shaderOption = _shaderOption;
    if (shaderOption == ShaderOption::Billboard_SSBO || shaderOption == ShaderOption::Point_SSBO) {

        //------------------------ RENDER WITH SSBO ---------------------------
        // Update SSBO Index array with accumulated stars in all chunks.
        glBindBuffer(GL_SHADER_STORAGE_BUFFER, _ssboIdx);
        int lastValue = _accumulatedIndices.back();
        _accumulatedIndices.resize(nChunksToRender + 1, lastValue);

        // Update vector with accumulated indices.
        for (auto &[offset, subData] : updateData) {
            int newValue = (subData.size() / _nRenderValuesPerStar) + _accumulatedIndices[offset];
            int changeInValue = newValue - _accumulatedIndices[offset + 1];
            _accumulatedIndices[offset + 1] = newValue;
            // Propagate change.
            for (int i = offset + 1; i < nChunksToRender; ++i) {
                _accumulatedIndices[i + 1] += changeInValue;
            }
        }

        // Fix number of stars rendered if it doesn't correspond to our buffers.
        if (_accumulatedIndices.back() != _nStarsToRender) {
            _nStarsToRender = _accumulatedIndices.back();
            _nRenderedStars.set(_nStarsToRender);
        }

        size_t indexBufferSize = nChunksToRender * sizeof(GLint);

        // Update SSBO Index (stars per chunk).
        glBufferData(
            GL_SHADER_STORAGE_BUFFER,
            indexBufferSize,
            _accumulatedIndices.data(),
            GL_STREAM_DRAW
        );

        // Use orphaning strategy for data SSBO.
        glBindBuffer(GL_SHADER_STORAGE_BUFFER, _ssboData);

        // Update SSBO Star Stream Budget property.
        //_ssboStreamBudgetProperty.set(static_cast<float>(_octreeManager->ssboStarStreamBudget()));
        _ssboStreamBudgetProperty.set(static_cast<float>(_octreeManager->numFreeSpotsInBuffer()));

        // Keep streaming memeory size to a minimum.
        //long long memoryQuery = nChunksToRender * maxStarsPerNode * _nRenderValuesPerStar * sizeof(GLfloat);
        //long long streamingBudgetInBytes = std::min(memoryQuery, _maxStreamingBudgetInBytes - indexBufferSize);
        long long streamingBudgetInBytes = _maxStreamingBudgetInBytes;

        glBufferData(
            GL_SHADER_STORAGE_BUFFER,
            streamingBudgetInBytes,
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
    }
    else {
        //---------------------- RENDER WITH VBO -----------------------------
        // Update VBOs with new nodes. 
        // This will overwrite old data that's not visible anymore as well.
        glBindVertexArray(_vao);

        // Always update Position VBO.
        glBindBuffer(GL_ARRAY_BUFFER, _vboPos);
        float posMemoryShare = static_cast<float>(POS_SIZE) / _nRenderValuesPerStar;
        int posChunkSize = maxStarsPerNode * POS_SIZE;
        //long long posMemoryQuery = nChunksToRender * posChunkSize * sizeof(GLfloat);
        long long posStreamingBudget = static_cast<long long>(_maxStreamingBudgetInBytes * posMemoryShare);

        // Use buffer orphaning to update a subset of total data.
        glBufferData(
            GL_ARRAY_BUFFER,
            posStreamingBudget,
            nullptr,
            GL_STREAM_DRAW
        );

        // Update buffer with one insert per chunk/node. The key in map holds the offset index.
        for (auto &[offset, subData] : updateData) {
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
        if (renderOption != gaiamission::RenderOption::Static) {
            glBindBuffer(GL_ARRAY_BUFFER, _vboCol);
            float colMemoryShare = static_cast<float>(COL_SIZE) / _nRenderValuesPerStar;
            int colChunkSize = maxStarsPerNode * COL_SIZE;
            //long long colMemoryQuery = nChunksToRender * colChunkSize * sizeof(GLfloat);
            long long colStreamingBudget = static_cast<long long>(
                _maxStreamingBudgetInBytes * colMemoryShare);

            // Use buffer orphaning to update a subset of total data.
            glBufferData(
                GL_ARRAY_BUFFER,
                colStreamingBudget,
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
            if (renderOption == gaiamission::RenderOption::Motion) {
                glBindBuffer(GL_ARRAY_BUFFER, _vboVel);
                float velMemoryShare = static_cast<float>(VEL_SIZE) / _nRenderValuesPerStar;
                int velChunkSize = maxStarsPerNode * VEL_SIZE;
                //long long velMemoryQuery = nChunksToRender * velChunkSize * sizeof(GLfloat);
                long long velStreamingBudget = static_cast<long long>(
                    _maxStreamingBudgetInBytes * velMemoryShare);

                // Use buffer orphaning to update a subset of total data.
                glBufferData(
                    GL_ARRAY_BUFFER,
                    velStreamingBudget,
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
        glBindVertexArray(0);
    }

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
    _program->setUniform(_uniformCache.projection, projection);
    _program->setUniform(_uniformCache.time, static_cast<float>(data.time.j2000Seconds()));
    _program->setUniform(_uniformCache.renderOption, _renderOption);
    _program->setUniform(_uniformCache.viewScaling, viewScaling);
    _program->setUniform(_uniformCache.cutOffThreshold, _cutOffThreshold);
    _program->setUniform(_uniformCache.luminosityMultiplier, _luminosityMultiplier);

    ghoul::opengl::TextureUnit colorUnit;
    colorUnit.activate();
    _colorTexture->bind();
    _program->setUniform(_uniformCache.colorTexture, colorUnit);

    // Specify how many stars we will render. (Will be overwritten if rendering billboards.)
    GLsizei nShaderCalls = _nStarsToRender;

    switch (shaderOption) {
    case ShaderOption::Point_SSBO: {
        _program->setUniform(_uniformCache.maxStarsPerNode, maxStarsPerNode);
        _program->setUniform(_uniformCache.valuesPerStar, valuesPerStar);
        _program->setUniform(_uniformCache.nChunksToRender, nChunksToRender);
        break;
    }
    case ShaderOption::Point_VBO: {
        // Specify how many potential stars we have to render.
        nShaderCalls = maxStarsPerNode * nChunksToRender;
        break;
    }
    case ShaderOption::Billboard_SSBO: {
        _program->setUniform(_uniformCache.maxStarsPerNode, maxStarsPerNode);
        _program->setUniform(_uniformCache.valuesPerStar, valuesPerStar);
        _program->setUniform(_uniformCache.nChunksToRender, nChunksToRender);

        _program->setUniform(_uniformCache.closeUpBoostDist,
            _closeUpBoostDist * static_cast<float>(distanceconstants::Parsec)
        );
        _program->setUniform(_uniformCache.billboardSize, _billboardSize);
        _program->setUniform(_uniformCache.screenSize, screenSize);
        _program->setUniform(_uniformCache.magnitudeBoost, _magnitudeBoost);
        _program->setUniform(_uniformCache.sharpness, _sharpness);

        ghoul::opengl::TextureUnit psfUnit;
        psfUnit.activate();
        _pointSpreadFunctionTexture->bind();
        _program->setUniform(_uniformCache.psfTexture, psfUnit);
        break;
    }
    case ShaderOption::Billboard_VBO: {
        _program->setUniform(_uniformCache.closeUpBoostDist,
            _closeUpBoostDist * static_cast<float>(distanceconstants::Parsec)
        );
        _program->setUniform(_uniformCache.billboardSize, _billboardSize);
        _program->setUniform(_uniformCache.screenSize, screenSize);
        _program->setUniform(_uniformCache.magnitudeBoost, _magnitudeBoost);
        _program->setUniform(_uniformCache.sharpness, _sharpness);

        ghoul::opengl::TextureUnit psfUnit;
        psfUnit.activate();
        _pointSpreadFunctionTexture->bind();
        _program->setUniform(_uniformCache.psfTexture, psfUnit);

        // Specify how many potential stars we have to render.
        nShaderCalls = maxStarsPerNode * nChunksToRender;
        break;
    }
    }

    // Render to FBO.
    glBindFramebuffer(GL_FRAMEBUFFER, _fbo);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    
    //glEnable(GL_PROGRAM_POINT_SIZE);
    // A non-zero named vao MUST ALWAYS be bound!
    if (_useVBO) {
        glBindVertexArray(_vao);
    }
    else {
        glBindVertexArray(_vaoEmpty);
    }
    
    glDrawArrays(GL_POINTS, 0, nShaderCalls);
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

    if (shaderOption == ShaderOption::Point_SSBO || shaderOption == ShaderOption::Point_VBO) {
        _programTM->setUniform(_uniformCacheTM.screenSize, screenSize);
    }

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
    const int shaderOption = _shaderOption;
    const int renderOption = _renderOption;

    if (_dataIsDirty) {
        LDEBUG("Regenerating data");
        // Reload data file. This may reconstruct the Octree as well.
        bool success = readDataFile();
        if (!success) {
            throw ghoul::RuntimeError("Error loading Gaia Star data");
        }
        _dataIsDirty = false;
        // Make sure we regenerate buffers if data has reloaded!
        _buffersAreDirty = true;
    }

    if (_program->isDirty() || _shadersAreDirty) {
        RenderEngine& renderEngine = OsEng.renderEngine();
        if (_program) {
            renderEngine.removeRenderProgram(_program.get());
            _program = nullptr;
        }
        switch (shaderOption) {
        case ShaderOption::Point_SSBO: {
            _program = ghoul::opengl::ProgramObject::Build(
                "GaiaStar",
                absPath("${MODULE_GAIAMISSION}/shaders/gaia_ssbo_vs.glsl"),
                absPath("${MODULE_GAIAMISSION}/shaders/gaia_point_fs.glsl"),
                absPath("${MODULE_GAIAMISSION}/shaders/gaia_point_ge.glsl")
            );
            _program->rebuildFromFile();

            _uniformCache.maxStarsPerNode = _program->uniformLocation("maxStarsPerNode");
            _uniformCache.valuesPerStar = _program->uniformLocation("valuesPerStar");
            _uniformCache.nChunksToRender = _program->uniformLocation("nChunksToRender");

            // If rebuild was triggered by switching ShaderOption then ssboBinding may not have 
            //been initialized yet. Binding will happen in later in buffersAredirty.
            if (!_shadersAreDirty) {
                _program->setSsboBinding("ssbo_idx_data", _ssboIdxBinding->bindingNumber());
                _program->setSsboBinding("ssbo_comb_data", _ssboDataBinding->bindingNumber());
            }
            break;
        }
        case ShaderOption::Point_VBO: {
            _program = ghoul::opengl::ProgramObject::Build(
                "GaiaStar",
                absPath("${MODULE_GAIAMISSION}/shaders/gaia_vbo_vs.glsl"),
                absPath("${MODULE_GAIAMISSION}/shaders/gaia_point_fs.glsl"),
                absPath("${MODULE_GAIAMISSION}/shaders/gaia_point_ge.glsl")
            );
            _program->rebuildFromFile();
            break;
        }
        case ShaderOption::Billboard_SSBO: {
            _program = ghoul::opengl::ProgramObject::Build(
                "GaiaStar",
                absPath("${MODULE_GAIAMISSION}/shaders/gaia_ssbo_vs.glsl"),
                absPath("${MODULE_GAIAMISSION}/shaders/gaia_billboard_fs.glsl"),
                absPath("${MODULE_GAIAMISSION}/shaders/gaia_billboard_ge.glsl")
            );
            _program->rebuildFromFile();

            _uniformCache.magnitudeBoost = _program->uniformLocation("magnitudeBoost");
            _uniformCache.sharpness = _program->uniformLocation("sharpness");
            _uniformCache.billboardSize = _program->uniformLocation("billboardSize");
            _uniformCache.closeUpBoostDist = _program->uniformLocation("closeUpBoostDist");
            _uniformCache.screenSize = _program->uniformLocation("screenSize");
            _uniformCache.psfTexture = _program->uniformLocation("psfTexture");

            _uniformCache.maxStarsPerNode = _program->uniformLocation("maxStarsPerNode");
            _uniformCache.valuesPerStar = _program->uniformLocation("valuesPerStar");
            _uniformCache.nChunksToRender = _program->uniformLocation("nChunksToRender");

            // If rebuild was triggered by switching ShaderOption then ssboBinding may not have 
            //been initialized yet. Binding will happen in later in buffersAredirty.
            if (!_shadersAreDirty) {
                _program->setSsboBinding("ssbo_idx_data", _ssboIdxBinding->bindingNumber());
                _program->setSsboBinding("ssbo_comb_data", _ssboDataBinding->bindingNumber());
            }
            break;
        }
        case ShaderOption::Billboard_VBO: {
            _program = ghoul::opengl::ProgramObject::Build(
                "GaiaStar",
                absPath("${MODULE_GAIAMISSION}/shaders/gaia_vbo_vs.glsl"),
                absPath("${MODULE_GAIAMISSION}/shaders/gaia_billboard_fs.glsl"),
                absPath("${MODULE_GAIAMISSION}/shaders/gaia_billboard_ge.glsl")
            );
            _program->rebuildFromFile();

            _uniformCache.magnitudeBoost = _program->uniformLocation("magnitudeBoost");
            _uniformCache.sharpness = _program->uniformLocation("sharpness");
            _uniformCache.billboardSize = _program->uniformLocation("billboardSize");
            _uniformCache.closeUpBoostDist = _program->uniformLocation("closeUpBoostDist");
            _uniformCache.screenSize = _program->uniformLocation("screenSize");
            _uniformCache.psfTexture = _program->uniformLocation("psfTexture");
            break;
        }
        }

        // Common uniforms for all shaders:
        _uniformCache.model = _program->uniformLocation("model");
        _uniformCache.view = _program->uniformLocation("view");
        _uniformCache.projection = _program->uniformLocation("projection");
        _uniformCache.time = _program->uniformLocation("time");
        _uniformCache.renderOption = _program->uniformLocation("renderOption");
        _uniformCache.viewScaling = _program->uniformLocation("viewScaling");
        _uniformCache.cutOffThreshold = _program->uniformLocation("cutOffThreshold");
        _uniformCache.luminosityMultiplier = _program->uniformLocation("luminosityMultiplier");
        _uniformCache.colorTexture = _program->uniformLocation("colorTexture");
    }

    if (_programTM->isDirty() || _shadersAreDirty) {
        RenderEngine& renderEngine = OsEng.renderEngine();
        if (_programTM) {
            renderEngine.removeRenderProgram(_programTM.get());
            _programTM = nullptr;
        }
        switch (shaderOption) {
        case ShaderOption::Point_SSBO:
        case ShaderOption::Point_VBO: {
            _programTM = renderEngine.buildRenderProgram("ToneMapping",
                absPath("${MODULE_GAIAMISSION}/shaders/gaia_tonemapping_vs.glsl"),
                absPath("${MODULE_GAIAMISSION}/shaders/gaia_tonemapping_point_fs.glsl")
            );
            _programTM->rebuildFromFile();

            _uniformCacheTM.screenSize = _programTM->uniformLocation("screenSize");
            break;
        }
        case ShaderOption::Billboard_SSBO:
        case ShaderOption::Billboard_VBO: {
            _programTM = renderEngine.buildRenderProgram("ToneMapping",
                absPath("${MODULE_GAIAMISSION}/shaders/gaia_tonemapping_vs.glsl"),
                absPath("${MODULE_GAIAMISSION}/shaders/gaia_tonemapping_billboard_fs.glsl")
            );
            _programTM->rebuildFromFile();
            break;
        }
        }
        // Common uniforms:
        _uniformCacheTM.renderedTexture = _programTM->uniformLocation("renderedTexture");

        _shadersAreDirty = false;
    }
    
    if (_buffersAreDirty) {
        LDEBUG("Regenerating buffers");

        // Set values per star slice depending on render option.
        if (renderOption == gaiamission::RenderOption::Static) {
            _nRenderValuesPerStar = POS_SIZE;
        }
        else if (renderOption == gaiamission::RenderOption::Color) {
            _nRenderValuesPerStar = POS_SIZE + COL_SIZE;
        }
        else { // (renderOption == gaiamission::RenderOption::Motion)
            _nRenderValuesPerStar = POS_SIZE + COL_SIZE + VEL_SIZE;
        }

        // Calculate memory budgets. 
        _chunkSize = _octreeManager->maxStarsPerNode() * _nRenderValuesPerStar;
        long long totalChunkSizeInBytes = _octreeManager->totalNodes() * _chunkSize * sizeof(GLfloat);
        _maxStreamingBudgetInBytes = std::min(totalChunkSizeInBytes, _gpuMemoryBudgetInBytes);
        long long maxNodesInStream = _maxStreamingBudgetInBytes / (_chunkSize * sizeof(GLfloat));
        
        // TODO: Figure out how to use properly! (Re-building fucked up)
        //long long maxStarsInStream = _gpuMemoryBudgetInBytes / (_nRenderValuesPerStar * sizeof(GLfloat));
        long long maxStarsInStream = maxNodesInStream;
        //_ssboStreamBudgetProperty.setMaxValue(static_cast<float>(_maxStreamingBudgetInBytes));
        _ssboStreamBudgetProperty.setMaxValue(static_cast<float>(maxStarsInStream));

        bool datasetFitInMemory = (_totalDatasetSizeInBytes < _cpuRamBudgetInBytes);

        LINFO("Chunk size: " + std::to_string(_chunkSize) +
            " - Max streaming budget (in bytes): " + std::to_string(_maxStreamingBudgetInBytes) +
            " - Max stars in stream: " + std::to_string(maxStarsInStream) +
            " - Max nodes in stream: " + std::to_string(maxNodesInStream));

        
        // ------------------ RENDER WITH SSBO -----------------------
        if (shaderOption == ShaderOption::Billboard_SSBO || shaderOption == ShaderOption::Point_SSBO) {
            _useVBO = false;

            // Trigger a rebuild of buffer data from octree. With VBO we will fill the chunks.
            _octreeManager->initBufferIndexStack(maxStarsInStream, _useVBO, datasetFitInMemory);
            _nStarsToRender = 0;

            // Generate SSBO Buffers and bind them. 
            if (_vaoEmpty == 0) {
                glGenVertexArrays(1, &_vaoEmpty);
                LDEBUG(fmt::format("Generating Empty Vertex Array id '{}'", _vaoEmpty));
            }
            if (_ssboIdx == 0) {
                glGenBuffers(1, &_ssboIdx);
                LDEBUG(fmt::format("Generating Index Shader Storage Buffer Object id '{}'", _ssboIdx));
            }
            if (_ssboData == 0) {
                glGenBuffers(1, &_ssboData);
                LDEBUG(fmt::format("Generating Data Shader Storage Buffer Object id '{}'", _ssboData));
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
        }
        else { // ------------------ RENDER WITH VBO -----------------------
            _useVBO = true;

            // Trigger a rebuild of buffer data from octree. With VBO we will fill the chunks.
            _octreeManager->initBufferIndexStack(maxNodesInStream, _useVBO, datasetFitInMemory);
            _nStarsToRender = 0;

            // Generate VAO and VBOs
            if (_vao == 0) {
                glGenVertexArrays(1, &_vao);
                LDEBUG(fmt::format("Generating Vertex Array id '{}'", _vao));
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

            // Bind our different VBOs to our vertex array layout. 
            glBindVertexArray(_vao);

            switch (renderOption) {
            case gaiamission::RenderOption::Static: {
                glBindBuffer(GL_ARRAY_BUFFER, _vboPos);
                GLint positionAttrib = _program->attributeLocation("in_position");
                glEnableVertexAttribArray(positionAttrib);

                glVertexAttribPointer(
                    positionAttrib,
                    POS_SIZE,
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
                    POS_SIZE,
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
                    COL_SIZE,
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
                    POS_SIZE,
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
                    COL_SIZE,
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
                    VEL_SIZE,
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
        }
        
        // Generate VAO and VBO for Quad.
        if (_vaoQuad == 0) {
            glGenVertexArrays(1, &_vaoQuad);
            LDEBUG(fmt::format("Generating Quad Vertex Array id '{}'", _vaoQuad));
        }
        if (_vboQuad == 0) {
            glGenBuffers(1, &_vboQuad);
            LDEBUG(fmt::format("Generating Quad Vertex Buffer Object id '{}'", _vboQuad));
        }

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

        // Generate Framebuffer Object and Texture.
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

        _buffersAreDirty = false;
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

bool RenderableGaiaStars::readDataFile() {

    const int fileReaderOption = _fileReaderOption;
    int nReadStars = 0;
    _initialDataFilesLoaded = false;

    std::string _file = _filePath;
    _octreeManager->initOctree(_cpuRamBudgetInBytes);

    LINFO("Loading data file: " + _file);

    switch (fileReaderOption) {
    case FileReaderOption::Fits: {

        // Read raw fits file and construct Octree.  
        nReadStars = readFitsFile(_file);
        break;
    }
    case FileReaderOption::Speck: {

        // Read raw speck file and construct Octree. 
        nReadStars = readSpeckFile(_file);
        break;
    }
    case FileReaderOption::BinaryRaw: {

        // Stars are stored in an ordered binary file.
        nReadStars = readBinaryRawFile(_file);
        break;
    }
    case FileReaderOption::BinaryOctree: {

        // Octree already constructed and stored as a binary file. 
        nReadStars = readBinaryOctreeFile(_file);
        break;
    }
    case FileReaderOption::StreamOctree: {

        // Read Octree structure from file, without data.
        nReadStars = readBinaryOctreeStructureFile(_file);
        break;
    }
    default:
        LERROR("Wrong FileReaderOption - no data file loaded!");
        break;
    }

    //_octreeManager->printStarsPerNode();
    _nRenderedStars.setMaxValue(nReadStars);
    LINFO("Dataset contains a total of " + std::to_string(nReadStars) + " stars.");
    _totalDatasetSizeInBytes = nReadStars * (POS_SIZE + COL_SIZE + VEL_SIZE) * 4;
    
    if (nReadStars > 0) _initialDataFilesLoaded = true;
    return _initialDataFilesLoaded;
}

int RenderableGaiaStars::readFitsFile(const std::string& filePath) {
    int nReadValuesPerStar = 0;

    FitsFileReader fitsFileReader(false);
    std::vector<float> fullData = fitsFileReader.readFitsFile(filePath, nReadValuesPerStar, _firstRow, 
        _lastRow, _columnNames);

    // Insert stars into octree.
    for (size_t i = 0; i < fullData.size(); i += nReadValuesPerStar) {
        auto first = fullData.begin() + i;
        auto last = fullData.begin() + i + nReadValuesPerStar;
        std::vector<float> starValues(first, last);

        _octreeManager->insert(starValues);
    }
    _octreeManager->sliceLodData();
    return fullData.size() / nReadValuesPerStar;
}

int RenderableGaiaStars::readSpeckFile(const std::string& filePath) {
    int nReadValuesPerStar = 0;

    FitsFileReader fileReader(false);
    std::vector<float> fullData = fileReader.readSpeckFile(filePath, nReadValuesPerStar);

    // Insert stars into octree.
    for (size_t i = 0; i < fullData.size(); i += nReadValuesPerStar) {
        auto first = fullData.begin() + i;
        auto last = fullData.begin() + i + nReadValuesPerStar;
        std::vector<float> starValues(first, last);

        _octreeManager->insert(starValues);
    }
    _octreeManager->sliceLodData();
    return fullData.size() / nReadValuesPerStar;
}

int RenderableGaiaStars::readBinaryRawFile(const std::string& filePath) {

    std::vector<float> fullData;
    int nReadStars = 0;

    std::ifstream fileStream(filePath, std::ifstream::binary);
    if (fileStream.good()) {
        
        int32_t nValues = 0;
        int32_t nReadValuesPerStar = 0;
        int renderValues = 8;
        fileStream.read(reinterpret_cast<char*>(&nValues), sizeof(int32_t));
        fileStream.read(reinterpret_cast<char*>(&nReadValuesPerStar), sizeof(int32_t));

        fullData.resize(nValues);
        fileStream.read(reinterpret_cast<char*>(&fullData[0]), nValues * sizeof(fullData[0]));

        // Insert stars into octree.
        for (size_t i = 0; i < fullData.size(); i += nReadValuesPerStar) {
            auto first = fullData.begin() + i;
            auto last = fullData.begin() + i + renderValues;
            std::vector<float> starValues(first, last);

            //TODO: Filter?
            _octreeManager->insert(starValues);
        }
        _octreeManager->sliceLodData();

        nReadStars = nValues / nReadValuesPerStar;
        fileStream.close();
    }
    else {
        LERROR(fmt::format("Error opening file '{}' for loading raw binary file!", filePath));
        return nReadStars;
    }
    return nReadStars;
}

int RenderableGaiaStars::readBinaryOctreeFile(const std::string& filePath) {
    int nReadStars = 0;

    std::ifstream fileStream(filePath, std::ifstream::binary);
    if (fileStream.good()) {
        nReadStars = _octreeManager->readFromFile(fileStream, true);

        fileStream.close();
    }
    else {
        LERROR(fmt::format("Error opening file '{}' for loading binary Octree file!", filePath));
        return nReadStars;
    }
    return nReadStars;
}

int RenderableGaiaStars::readBinaryOctreeStructureFile(const std::string& folderPath) {
    int nReadStars = 0;
    std::string indexFile = folderPath + "index.bin";

    std::ifstream fileStream(indexFile, std::ifstream::binary);
    if (fileStream.good()) {
        nReadStars = _octreeManager->readFromFile(fileStream, false, folderPath);

        fileStream.close();
    }
    else {
        LERROR(fmt::format("Error opening file '{}' for loading binary Octree file!", indexFile));
        return nReadStars;
    }
    return nReadStars;
}

} // namespace openspace
