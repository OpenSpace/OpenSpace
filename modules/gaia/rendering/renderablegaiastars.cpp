/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include <modules/gaia/rendering/renderablegaiastars.h>

#include <modules/fitsfilereader/include/fitsfilereader.h>
#include <modules/gaia/rendering/gaiaoptions.h>
#include <modules/gaia/rendering/octreeculler.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/distanceconversion.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/format.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/misc/templatefactory.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/systemcapabilities/generalcapabilitiescomponent.h>
#include <array>
#include <fstream>
#include <cstdint>

namespace {
    constexpr std::string_view _loggerCat = "RenderableGaiaStars";

    constexpr size_t PositionSize = 3;
    constexpr size_t ColorSize = 2;
    constexpr size_t VelocitySize = 3;

    constexpr openspace::properties::Property::PropertyInfo FilePathInfo = {
        "File",
        "File Path",
        "The path to the file with data for the stars to be rendered",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo FileReaderOptionInfo = {
        "FileReaderOption",
        "File Reader Option",
        "This value tells the renderable what format the input data file has. "
        "'Fits' will read a FITS file, construct an Octree from it and render full "
        "data. 'Speck' will read a SPECK file, construct an Octree from it and render "
        "full data. 'BinaryRaw' will read a preprocessed binary file with ordered star "
        "data, construct an Octree and render it. 'BinaryOctree' will read a constructed "
        "Octree from binary file and render full data. 'StreamOctree' will read an index "
        "file with full Octree structure and then stream nodes during runtime. (This "
        "option is suited for bigger datasets)",
        // @VISIBILITY(3.67)
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo RenderModeInfo = {
        "RenderMode",
        "Render Mode",
        "This value determines which predefined columns to use in rendering. If "
        "'Static' only the position of the stars is used. 'Color' uses position + color "
        "parameters and 'Motion' uses pos, color as well as velocity for the stars",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ShaderOptionInfo = {
        "ShaderOption",
        "Shader Option",
        "This value determines which shaders to use while rendering. If 'Point_*' is "
        "chosen then gl_Points will be rendered and then spread out with a bloom "
        "filter. If 'Billboard_*' is chosen then the geometry shaders will generate "
        "screen-faced billboards for all stars. For '*_SSBO' the data will be stored in "
        "Shader Storage Buffer Objects while '*_VBO' uses Vertex Buffer Objects for the "
        "streaming. OBS! SSBO won't work on Apple",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo PsfTextureInfo = {
        "Texture",
        "Point Spread Function Texture",
        "The path to the texture that should be used as a point spread function for the "
        "stars",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo LuminosityMultiplierInfo = {
        "LuminosityMultiplier",
        "Luminosity Multiplier",
        "Factor by which to multiply the luminosity with. [Works in Color and Motion "
        "modes]",
        // @VISIBILITY(2.33)
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo MagnitudeBoostInfo = {
        "MagnitudeBoost",
        "Magnitude Boost",
        "Sets what percent of the star magnitude that will be used as boost to star "
        "size. [Works only with billboards in Color and Motion modes]",
        // @VISIBILITY(2.33)
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo CutOffThresholdInfo = {
        "CutOffThreshold",
        "Cut Off Threshold",
        "Set threshold for when to cut off star rendering. Stars closer than this "
        "threshold are given full opacity. Farther away, stars dim proportionally to the "
        "4-logarithm of their distance",
        // @VISIBILITY(2.33)
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo SharpnessInfo = {
        "Sharpness",
        "Sharpness",
        "Adjust star sharpness. [Works only with billboards]",
        // @VISIBILITY(?)
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo BillboardSizeInfo = {
        "BillboardSize",
        "Billboard Size",
        "Set the billboard size of all stars. [Works only with billboards]",
        // @VISIBILITY(?)
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo CloseUpBoostDistInfo = {
        "CloseUpBoostDist",
        "Close-Up Boost Distance [pc]",
        "Set the distance where stars starts to increase in size. Unit is Parsec [Works "
        "only with billboards]",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo TmPointFilterSizeInfo = {
        "FilterSize",
        "Filter Size [px]",
        "Set the filter size in pixels used in tonemapping for point splatting rendering"
        "[Works only with points]",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo TmPointSigmaInfo = {
        "Sigma",
        "Normal Distribution Sigma",
        "Set the normal distribution sigma used in tonemapping for point splatting "
        "rendering. [Works only with points]",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo AdditionalNodesInfo = {
        "AdditionalNodes",
        "Additional Nodes",
        "Determines how many additional nodes around the camera that will be fetched "
        "from disk. The first value determines how many additional layers of parents "
        "that will be fetched. The second value determines how many layers of descendant "
        "that will be fetched from the found parents",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo TmPointPxThresholdInfo = {
        "PixelWeightThreshold",
        "Pixel Weight Threshold",
        "Set the threshold for how big the elliptic weight of a pixel has to be to "
        "contribute to the final elliptic shape. A smaller value gives a more visually "
        "pleasing result while a bigger value will speed up the rendering on skewed "
        "frustums (aka Domes)",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ColorTextureInfo = {
        "ColorMap",
        "Color Texture",
        "The path to the texture that is used to convert from the magnitude of the star "
        "to its color. The texture is used as a one dimensional lookup function",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo FirstRowInfo = {
        "FirstRow",
        "First Row to Read",
        "Defines the first row that will be read from the specified FITS file No need to "
        "define if data already has been processed. [Works only with "
        "FileReaderOption::Fits]",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo LastRowInfo = {
        "LastRow",
        "Last Row to Read",
        "Defines the last row that will be read from the specified FITS file; has to be "
        "equal to or greater than FirstRow. No need to define if data already has been "
        "processed. [Works only with FileReaderOption::Fits]",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ColumnNamesInfo = {
        "ColumnNames",
        "Column Names",
        "A list of strings with the names of all the columns that are to be read from "
        "the specified FITS file. No need to define if data already has been processed. "
        "[Works only with FileReaderOption::Fits]",
        // @VISIBILITY(3.67)
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo NumRenderedStarsInfo = {
        "NumRenderedStars",
        "Rendered Stars",
        "The number of rendered stars in the current frame",
        // @VISIBILITY(3.67)
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo CpuRamBudgetInfo = {
        "CpuRamBudget",
        "CPU RAM Budget",
        "Current remaining budget (bytes) on the CPU RAM for loading more node data "
        "files",
        // @VISIBILITY(3.33)
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo GpuStreamBudgetInfo = {
        "GpuStreamBudget",
        "GPU Stream Budget",
        "Current remaining memory budget [in number of chunks] on the GPU for streaming "
        "additional stars",
        // @VISIBILITY(3.67)
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo LodPixelThresholdInfo = {
        "LodPixelThreshold",
        "LOD Pixel Threshold",
        "The number of total pixels a nodes AABB can have in clipping space before its "
        "parent is fetched as LOD cache",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo MaxGpuMemoryPercentInfo = {
        "MaxGpuMemoryPercent",
        "Max GPU Memory",
        "Sets the max percent of existing GPU memory budget that the streaming will use",
        // @VISIBILITY(3.33)
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo MaxCpuMemoryPercentInfo = {
        "MaxCpuMemoryPercent",
        "Max CPU Memory",
        "Sets the max percent of existing CPU memory budget that the streaming of files "
        "will use",
        // @VISIBILITY(3.33)
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo FilterPosXInfo = {
        "FilterPosX",
        "PosX Threshold",
        "If defined then only stars with Position X values between [min, max] will be "
        "rendered (if min is set to 0.0 it is read as -Inf, if max is set to 0.0 it is "
        "read as +Inf). Measured in kiloParsec",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo FilterPosYInfo = {
        "FilterPosY",
        "PosY Threshold",
        "If defined then only stars with Position Y values between [min, max] will be "
        "rendered (if min is set to 0.0 it is read as -Inf, if max is set to 0.0 it is "
        "read as +Inf). Measured in kiloParsec",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo FilterPosZInfo = {
        "FilterPosZ",
        "PosZ Threshold",
        "If defined then only stars with Position Z values between [min, max] will be "
        "rendered (if min is set to 0.0 it is read as -Inf, if max is set to 0.0 it is "
        "read as +Inf). Measured in kiloParsec",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo FilterGMagInfo = {
        "FilterGMag",
        "GMag Threshold",
        "If defined then only stars with G mean magnitude values between [min, max] will "
        "be rendered (if min is set to 20.0 it is read as -Inf, if max is set to 20.0 it "
        "is read as +Inf). If min = max then all values equal min|max will be filtered "
        "away",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo FilterBpRpInfo = {
        "FilterBpRp",
        "Bp-Rp Threshold",
        "If defined then only stars with Bp-Rp color values between [min, max] will be "
        "rendered (if min is set to 0.0 it is read as -Inf, if max is set to 0.0 it is "
        "read as +Inf). If min = max then all values equal min|max will be filtered away",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo FilterDistInfo = {
        "FilterDist",
        "Dist Threshold",
        "If defined then only stars with Distances values between [min, max] will be "
        "rendered (if min is set to 0.0 it is read as -Inf, if max is set to 0.0 it is "
        "read as +Inf). Measured in kParsec",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ReportGlErrorsInfo = {
        "ReportGlErrors",
        "Report GL Errors",
        "If set to true, any OpenGL errors will be reported if encountered",
        // @VISIBILITY(3.67)
        openspace::properties::Property::Visibility::AdvancedUser
    };

    struct [[codegen::Dictionary(RenderableGaiaStars)]] Parameters {
        // [[codegen::verbatim(FilePathInfo.description)]]
        std::string file;

        enum class [[codegen::map(openspace::gaia::FileReaderOption)]] FileReader {
            Fits,
            Speck,
            BinaryRaw,
            BinaryOctree,
            StreamOctree
        };
        // [[codegen::verbatim(FileReaderOptionInfo.description)]]
        FileReader fileReaderOption;

        enum class [[codegen::map(openspace::gaia::RenderMode)]] RenderMode {
            Static,
            Color,
            Motion
        };
        // [[codegen::verbatim(RenderModeInfo.description)]]
        std::optional<RenderMode> renderMode;

        enum class [[codegen::map(openspace::gaia::ShaderOption)]] ShaderOption {
            PointSSBO [[codegen::key("Point_SSBO")]],
            PointVBO [[codegen::key("Point_VBO")]],
            BillboardSSBO [[codegen::key("Billboard_SSBO")]],
            BillboardVBO [[codegen::key("Billboard_VBO")]],
            BillboardSSBONoFBO [[codegen::key("Billboard_SSBO_noFBO")]]
        };
        // [codegen::verbatim(ShaderOptionInfo.description)]]
        std::optional<ShaderOption> shaderOption;

        // [codegen::verbatim(PsfTextureInfo.description)]]
        std::string texture;

        // [codegen::verbatim(ColorTextureInfo.description)]]
        std::string colorMap;

        // [codegen::verbatim(LuminosityMultiplierInfo.description)]]
        std::optional<float> luminosityMultiplier;

        // [codegen::verbatim(MagnitudeBoostInfo.description)]]
        std::optional<float> magnitudeBoost;

        // [codegen::verbatim(CutOffThresholdInfo.description)]]
        std::optional<float> cutOffThreshold;

        // [codegen::verbatim(SharpnessInfo.description)]]
        std::optional<float> sharpness;

        // [codegen::verbatim(BillboardSizeInfo.description)]]
        std::optional<float> billboardSize;

        // [codegen::verbatim(CloseUpBoostDistInfo.description)]]
        std::optional<float> closeUpBoostDist;

        // [codegen::verbatim(TmPointFilterSizeInfo.description)]]
        std::optional<int> filterSize;

        // [codegen::verbatim(TmPointSigmaInfo.description)]]
        std::optional<float> sigma;

        // [codegen::verbatim(AdditionalNodesInfo.description)]]
        std::optional<glm::ivec2> additionalNodes;

        // [codegen::verbatim(TmPointPxThresholdInfo.description)]]
        std::optional<float> pixelWeightThreshold;

        // [codegen::verbatim(FirstRowInfo.description)]]
        std::optional<int> firstRow;

        // [codegen::verbatim(LastRowInfo.description)]]
        std::optional<int> lastRow;

        // [codegen::verbatim(ColumnNamesInfo.description)]]
        std::optional<std::vector<std::string>> columnNames;

        // [codegen::verbatim(LodPixelThresholdInfo.description)]]
        std::optional<float> lodPixelThreshold;

        // [codegen::verbatim(MaxGpuMemoryPercentInfo.description)]]
        std::optional<float> maxGpuMemoryPercent;

        // [codegen::verbatim(MaxCpuMemoryPercentInfo.description)]]
        std::optional<float> maxCpuMemoryPercent;

        // [codegen::verbatim(FilterPosXInfo.description)]]
        std::optional<glm::vec2> filterPosX;

        // [codegen::verbatim(FilterPosYInfo.description)]]
        std::optional<glm::vec2> filterPosY;

        // [codegen::verbatim(FilterPosZInfo.description)]]
        std::optional<glm::vec2> filterPosZ;

        // [codegen::verbatim(FilterGMagInfo.description)]]
        std::optional<glm::vec2> filterGMag;

        // [codegen::verbatim(FilterBpRpInfo.description)]]
        std::optional<glm::vec2> filterBpRp;

        // [codegen::verbatim(FilterDistInfo.description)]]
        std::optional<glm::vec2> filterDist;

        // [codegen::verbatim(ReportGlErrorsInfo.description)]]
        std::optional<bool> reportGlErrors;
    };
#include "renderablegaiastars_codegen.cpp"
}  // namespace

namespace openspace {

documentation::Documentation RenderableGaiaStars::Documentation() {
    return codegen::doc<Parameters>("gaiamission_renderablegaiastars");
}

RenderableGaiaStars::RenderableGaiaStars(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _filePath(FilePathInfo)
    , _pointSpreadFunctionTexturePath(PsfTextureInfo)
    , _colorTexturePath(ColorTextureInfo)
    , _luminosityMultiplier(LuminosityMultiplierInfo, 35.f, 1.f, 250.f)
    , _magnitudeBoost(MagnitudeBoostInfo, 25.f, 0.f, 100.f)
    , _cutOffThreshold(CutOffThresholdInfo, 38.f, 0.f, 50.f)
    , _sharpness(SharpnessInfo, 1.45f, 0.f, 5.f)
    , _billboardSize(BillboardSizeInfo, 10.f, 1.f, 100.f)
    , _closeUpBoostDist(CloseUpBoostDistInfo, 300.f, 1.f, 1000.f)
    , _tmPointFilterSize(TmPointFilterSizeInfo, 7, 1, 19)
    , _tmPointSigma(TmPointSigmaInfo, 0.7f, 0.1f, 3.f)
    , _additionalNodes(AdditionalNodesInfo, glm::ivec2(1), glm::ivec2(0), glm::ivec2(4))
    , _tmPointPixelWeightThreshold(TmPointPxThresholdInfo, 0.001f, 0.000001f, 0.01f)
    , _lodPixelThreshold(LodPixelThresholdInfo, 250.f, 0.f, 5000.f)
    , _posXThreshold(FilterPosXInfo, glm::vec2(0.f), glm::vec2(-10.f), glm::vec2(10.f))
    , _posYThreshold(FilterPosYInfo, glm::vec2(0.f), glm::vec2(-10.f), glm::vec2(10.f))
    , _posZThreshold(FilterPosZInfo, glm::vec2(0.f), glm::vec2(-10.f), glm::vec2(10.f))
    , _gMagThreshold(FilterGMagInfo, glm::vec2(20.f), glm::vec2(-10.f), glm::vec2(30.f))
    , _bpRpThreshold(FilterBpRpInfo, glm::vec2(0.f), glm::vec2(-10.f), glm::vec2(30.f))
    , _distThreshold(FilterDistInfo, glm::vec2(0.f), glm::vec2(0.f), glm::vec2(100.f))
    , _firstRow(FirstRowInfo, 0, 0, 2539913) // DR1-max: 2539913
    , _lastRow(LastRowInfo, 0, 0, 2539913)
    , _columnNamesList(ColumnNamesInfo)
    , _fileReaderOption(
        FileReaderOptionInfo,
        properties::OptionProperty::DisplayType::Dropdown
    )
    , _renderMode(RenderModeInfo, properties::OptionProperty::DisplayType::Dropdown)
    , _shaderOption(ShaderOptionInfo, properties::OptionProperty::DisplayType::Dropdown)
    , _nRenderedStars(NumRenderedStarsInfo, 0, 0, 2000000000) // 2 Billion stars
    , _cpuRamBudgetProperty(CpuRamBudgetInfo, 0.f, 0.f, 1.f)
    , _gpuStreamBudgetProperty(GpuStreamBudgetInfo, 0.f, 0.f, 1.f)
    , _maxGpuMemoryPercent(MaxGpuMemoryPercentInfo, 0.45f, 0.f, 1.f)
    , _maxCpuMemoryPercent(MaxCpuMemoryPercentInfo, 0.5f, 0.f, 1.f)
    , _reportGlErrors(ReportGlErrorsInfo, false)
    , _accumulatedIndices(1, 0)
{
    using File = ghoul::filesystem::File;

    const Parameters p = codegen::bake<Parameters>(dictionary);

    _filePath = absPath(p.file).string();
    _dataFile = std::make_unique<File>(_filePath.value());
    _dataFile->setCallback([this]() { _dataIsDirty = true; });

    _filePath.onChange([this]() {
        if (std::filesystem::exists(_filePath.value())) {
            _dataIsDirty = true;
        }
        else {
            LWARNING(std::format("File not found: {}", _filePath.value()));
        }
    });
    addProperty(_filePath);

    _fileReaderOption.addOptions({
        { gaia::FileReaderOption::Fits, "Fits" },
        { gaia::FileReaderOption::Speck, "Speck" },
        { gaia::FileReaderOption::BinaryRaw, "BinaryRaw" },
        { gaia::FileReaderOption::BinaryOctree, "BinaryOctree" },
        { gaia::FileReaderOption::StreamOctree, "StreamOctree" }
    });
    _fileReaderOption = codegen::map<gaia::FileReaderOption>(p.fileReaderOption);

    _renderMode.addOptions({
        { gaia::RenderMode::Static, "Static" },
        { gaia::RenderMode::Color, "Color" },
        { gaia::RenderMode::Motion, "Motion" }
    });
    if (p.renderMode.has_value()) {
        _renderMode = codegen::map<gaia::RenderMode>(*p.renderMode);
    }
    _renderMode.onChange([this]() { _buffersAreDirty = true; });
    addProperty(_renderMode);

    _shaderOption.addOptions({
        { gaia::ShaderOption::PointSSBO, "Point_SSBO" },
        { gaia::ShaderOption::PointVBO, "Point_VBO" },
        { gaia::ShaderOption::BillboardSSBO, "Billboard_SSBO" },
        { gaia::ShaderOption::BillboardVBO, "Billboard_VBO" },
        { gaia::ShaderOption::BillboardSSBONoFBO, "Billboard_SSBO_noFBO" }
    });

    if (p.shaderOption.has_value()) {
        _shaderOption = codegen::map<gaia::ShaderOption>(*p.shaderOption);

#ifdef __APPLE__
        switch (_shaderOption) {
            case gaia::ShaderOption::PointSSBO:
            case gaia::ShaderOption::BillboardSSBO:
            case gaia::ShaderOption::BillboardSSBONoFBO:
                LWARNING("Shader option unsupported, changing to Point VBO");
                _shaderOption = gaia::ShaderOption::PointVBO;
        }
#endif // __APPLE__
    }
    _shaderOption.onChange([this]() {
        _buffersAreDirty = true;
        _shadersAreDirty = true;
    });
    addProperty(_shaderOption);

    _pointSpreadFunctionTexturePath = absPath(p.texture).string();
    _pointSpreadFunctionTexturePath.onChange(
        [this](){ _pointSpreadFunctionTextureIsDirty = true; }
    );
    _pointSpreadFunctionFile = std::make_unique<File>(
        _pointSpreadFunctionTexturePath.value()
    );
    _pointSpreadFunctionFile->setCallback(
        [this]() { _pointSpreadFunctionTextureIsDirty = true; }
    );

    _colorTexturePath = absPath(p.colorMap).string();
    _colorTextureFile = std::make_unique<File>(_colorTexturePath.value());
    _colorTexturePath.onChange([this]() { _colorTextureIsDirty = true; });
    _colorTextureFile->setCallback([this]() { _colorTextureIsDirty = true; });

    _luminosityMultiplier = p.luminosityMultiplier.value_or(_luminosityMultiplier);
    _magnitudeBoost = p.magnitudeBoost.value_or(_magnitudeBoost);
    _cutOffThreshold = p.cutOffThreshold.value_or(_cutOffThreshold);
    _sharpness = p.sharpness.value_or(_sharpness);
    _billboardSize = p.billboardSize.value_or(_billboardSize);
    _closeUpBoostDist = p.closeUpBoostDist.value_or(_closeUpBoostDist);
    _tmPointFilterSize = p.filterSize.value_or(_tmPointFilterSize);
    _tmPointSigma = p.sigma.value_or(_tmPointSigma);
    _tmPointPixelWeightThreshold =
        p.pixelWeightThreshold.value_or(_tmPointPixelWeightThreshold);
    _additionalNodes = p.additionalNodes.value_or(_additionalNodes);
    _lodPixelThreshold = p.lodPixelThreshold.value_or(_lodPixelThreshold);

    _maxGpuMemoryPercent = p.maxGpuMemoryPercent.value_or(_maxGpuMemoryPercent);
    _maxGpuMemoryPercent.onChange([this]() {
        if (_ssboData != 0) {
            glDeleteBuffers(1, &_ssboData);
            glGenBuffers(1, &_ssboData);
            LDEBUG(std::format(
                "Re-generating Data Shader Storage Buffer Object id '{}'", _ssboData
            ));
        }

        // Find out our new budget. Use dedicated video memory instead of current
        // available to always be consistant with previous call(s).
        GLint nDedicatedVidMemoryInKB = 0;
        glGetIntegerv(GL_GPU_MEMORY_INFO_DEDICATED_VIDMEM_NVX, &nDedicatedVidMemoryInKB);
        const float dedicatedVidMem = static_cast<float>(
            static_cast<long long>(nDedicatedVidMemoryInKB) * 1024
        );

        // TODO: Need to fix what happens if we can't query! For now use 2 GB by default.
        _gpuMemoryBudgetInBytes = dedicatedVidMem > 0 ?
            static_cast<long long>(dedicatedVidMem * _maxGpuMemoryPercent) :
            2147483648;
        _buffersAreDirty = true;
        _maxStreamingBudgetInBytes = 0;
    });

    _maxCpuMemoryPercent = p.maxCpuMemoryPercent.value_or(_maxCpuMemoryPercent);
    _posXThreshold = p.filterPosX.value_or(_posXThreshold);
    addProperty(_posXThreshold);

    _posYThreshold = p.filterPosY.value_or(_posYThreshold);
    addProperty(_posYThreshold);

    _posZThreshold = p.filterPosZ.value_or(_posZThreshold);
    addProperty(_posZThreshold);

    _gMagThreshold = p.filterGMag.value_or(_gMagThreshold);
    addProperty(_gMagThreshold);

    _bpRpThreshold = p.filterBpRp.value_or(_bpRpThreshold);
    addProperty(_bpRpThreshold);

    _distThreshold = p.filterDist.value_or(_distThreshold);
    addProperty(_distThreshold);

    // Only add properties correlated to fits files if we're reading from a fits file.
    if (_fileReaderOption == gaia::FileReaderOption::Fits) {
        _firstRow = p.firstRow.value_or(_firstRow);
        _firstRow.onChange([this]() { _dataIsDirty = true; });
        addProperty(_firstRow);

        _lastRow = p.lastRow.value_or(_lastRow);
        _lastRow.onChange([this]() { _dataIsDirty = true; });
        addProperty(_lastRow);

        if (p.columnNames.has_value()) {
            const ghoul::Dictionary tmpDict = dictionary.value<ghoul::Dictionary>(
                ColumnNamesInfo.identifier
            );

            // Ugly fix for ASCII sorting when there are more columns read than 10.
            std::set<int> intKeys;
            for (const std::string_view key : tmpDict.keys()) {
                intKeys.insert(std::stoi(std::string(key)));
            }

            for (const int key : intKeys) {
                _columnNames.push_back(tmpDict.value<std::string>(std::to_string(key)));
            }

            // Copy values to the StringListproperty to be shown in the Property list.
            _columnNamesList = _columnNames;
            // OBS - This is not used atm!
        }

        if (_firstRow > _lastRow) {
            throw ghoul::RuntimeError("User defined FirstRow is bigger than LastRow");
        }
    }

    _reportGlErrors = p.reportGlErrors.value_or(_reportGlErrors);
    addProperty(_reportGlErrors);

    // Add a read-only property for the number of rendered stars per frame.
    _nRenderedStars.setReadOnly(true);
    addProperty(_nRenderedStars);

    // Add CPU RAM Budget Property and GPU Stream Budget Property to menu.
    _cpuRamBudgetProperty.setReadOnly(true);
    addProperty(_cpuRamBudgetProperty);
    _gpuStreamBudgetProperty.setReadOnly(true);
    addProperty(_gpuStreamBudgetProperty);
}

bool RenderableGaiaStars::isReady() const {
    return _program && _programTM;
}

void RenderableGaiaStars::initializeGL() {
    //using IgnoreError = ghoul::opengl::ProgramObject::IgnoreError;
    //_program->setIgnoreUniformLocationError(IgnoreError::Yes);

    // Add common properties to menu.
    addProperty(_colorTexturePath);
    addProperty(_luminosityMultiplier);
    addProperty(_cutOffThreshold);
    addProperty(_lodPixelThreshold);
    addProperty(_maxGpuMemoryPercent);

    // Construct shader program depending on user-defined shader option.
    const int option = _shaderOption;
    switch (option) {
        case gaia::ShaderOption::PointSSBO:
            _program = ghoul::opengl::ProgramObject::Build(
                "GaiaStar",
                absPath("${MODULE_GAIA}/shaders/gaia_ssbo_vs.glsl"),
                absPath("${MODULE_GAIA}/shaders/gaia_point_fs.glsl"),
                absPath("${MODULE_GAIA}/shaders/gaia_point_ge.glsl")
            );
            _uniformCache.maxStarsPerNode = _program->uniformLocation("maxStarsPerNode");
            _uniformCache.valuesPerStar = _program->uniformLocation("valuesPerStar");
            _uniformCache.nChunksToRender = _program->uniformLocation("nChunksToRender");

            _programTM = global::renderEngine->buildRenderProgram(
                "ToneMapping",
                absPath("${MODULE_GAIA}/shaders/gaia_tonemapping_vs.glsl"),
                absPath("${MODULE_GAIA}/shaders/gaia_tonemapping_point_fs.glsl")
            );
            _uniformCacheTM.screenSize = _programTM->uniformLocation("screenSize");
            _uniformCacheTM.filterSize = _programTM->uniformLocation("filterSize");
            _uniformCacheTM.sigma = _programTM->uniformLocation("sigma");
            _uniformCacheTM.projection = _programTM->uniformLocation("projection");
            _uniformCacheTM.pixelWeightThreshold =
                _programTM->uniformLocation("pixelWeightThreshold");

            addProperty(_tmPointFilterSize);
            addProperty(_tmPointSigma);
            addProperty(_tmPointPixelWeightThreshold);
            break;
        case gaia::ShaderOption::PointVBO:
            _program = ghoul::opengl::ProgramObject::Build(
                "GaiaStar",
                absPath("${MODULE_GAIA}/shaders/gaia_vbo_vs.glsl"),
                absPath("${MODULE_GAIA}/shaders/gaia_point_fs.glsl"),
                absPath("${MODULE_GAIA}/shaders/gaia_point_ge.glsl")
            );

            _programTM = global::renderEngine->buildRenderProgram("ToneMapping",
                absPath("${MODULE_GAIA}/shaders/gaia_tonemapping_vs.glsl"),
                absPath("${MODULE_GAIA}/shaders/gaia_tonemapping_point_fs.glsl")
            );
            _uniformCacheTM.screenSize = _programTM->uniformLocation("screenSize");
            _uniformCacheTM.filterSize = _programTM->uniformLocation("filterSize");
            _uniformCacheTM.sigma = _programTM->uniformLocation("sigma");
            _uniformCacheTM.projection = _programTM->uniformLocation("projection");
            _uniformCacheTM.pixelWeightThreshold =
                _programTM->uniformLocation("pixelWeightThreshold");

            addProperty(_tmPointFilterSize);
            addProperty(_tmPointSigma);
            addProperty(_tmPointPixelWeightThreshold);
            break;
        case gaia::ShaderOption::BillboardSSBO:
            _program = ghoul::opengl::ProgramObject::Build(
                "GaiaStar",
                absPath("${MODULE_GAIA}/shaders/gaia_ssbo_vs.glsl"),
                absPath("${MODULE_GAIA}/shaders/gaia_billboard_fs.glsl"),
                absPath("${MODULE_GAIA}/shaders/gaia_billboard_ge.glsl")
            );
            _uniformCache.cameraPos = _program->uniformLocation("cameraPos");
            _uniformCache.cameraLookUp = _program->uniformLocation("cameraLookUp");
            _uniformCache.magnitudeBoost = _program->uniformLocation("magnitudeBoost");
            _uniformCache.sharpness = _program->uniformLocation("sharpness");
            _uniformCache.billboardSize = _program->uniformLocation("billboardSize");
            _uniformCache.closeUpBoostDist = _program->uniformLocation(
                "closeUpBoostDist"
            );
            _uniformCache.psfTexture = _program->uniformLocation("psfTexture");

            _uniformCache.maxStarsPerNode = _program->uniformLocation("maxStarsPerNode");
            _uniformCache.valuesPerStar = _program->uniformLocation("valuesPerStar");
            _uniformCache.nChunksToRender = _program->uniformLocation("nChunksToRender");

            _programTM = global::renderEngine->buildRenderProgram(
                "ToneMapping",
                absPath("${MODULE_GAIA}/shaders/gaia_tonemapping_vs.glsl"),
                absPath("${MODULE_GAIA}/shaders/gaia_tonemapping_billboard_fs.glsl")
            );

            addProperty(_magnitudeBoost);
            addProperty(_sharpness);
            addProperty(_billboardSize);
            addProperty(_closeUpBoostDist);
            //addProperty(_pointSpreadFunctionTexturePath);
            break;
        case gaia::ShaderOption::BillboardSSBONoFBO:
            _program = global::renderEngine->buildRenderProgram("GaiaStar",
                absPath("${MODULE_GAIA}/shaders/gaia_ssbo_vs.glsl"),
                absPath("${MODULE_GAIA}/shaders/gaia_billboard_nofbo_fs.glsl"),
                absPath("${MODULE_GAIA}/shaders/gaia_billboard_ge.glsl")
            );
            _uniformCache.cameraPos = _program->uniformLocation("cameraPos");
            _uniformCache.cameraLookUp = _program->uniformLocation("cameraLookUp");
            _uniformCache.magnitudeBoost = _program->uniformLocation("magnitudeBoost");
            _uniformCache.sharpness = _program->uniformLocation("sharpness");
            _uniformCache.billboardSize = _program->uniformLocation("billboardSize");
            _uniformCache.closeUpBoostDist = _program->uniformLocation(
                "closeUpBoostDist"
            );
            _uniformCache.psfTexture = _program->uniformLocation("psfTexture");

            _uniformCache.maxStarsPerNode = _program->uniformLocation("maxStarsPerNode");
            _uniformCache.valuesPerStar = _program->uniformLocation("valuesPerStar");
            _uniformCache.nChunksToRender = _program->uniformLocation("nChunksToRender");

            addProperty(_magnitudeBoost);
            addProperty(_sharpness);
            addProperty(_billboardSize);
            addProperty(_closeUpBoostDist);
            break;
        case gaia::ShaderOption::BillboardVBO:
            _program = ghoul::opengl::ProgramObject::Build(
                "GaiaStar",
                absPath("${MODULE_GAIA}/shaders/gaia_vbo_vs.glsl"),
                absPath("${MODULE_GAIA}/shaders/gaia_billboard_fs.glsl"),
                absPath("${MODULE_GAIA}/shaders/gaia_billboard_ge.glsl")
            );
            _uniformCache.cameraPos = _program->uniformLocation("cameraPos");
            _uniformCache.cameraLookUp = _program->uniformLocation("cameraLookUp");
            _uniformCache.magnitudeBoost = _program->uniformLocation("magnitudeBoost");
            _uniformCache.sharpness = _program->uniformLocation("sharpness");
            _uniformCache.billboardSize = _program->uniformLocation("billboardSize");
            _uniformCache.closeUpBoostDist = _program->uniformLocation(
                "closeUpBoostDist"
            );
            _uniformCache.psfTexture = _program->uniformLocation("psfTexture");

            _programTM = global::renderEngine->buildRenderProgram("ToneMapping",
                absPath("${MODULE_GAIA}/shaders/gaia_tonemapping_vs.glsl"),
                absPath("${MODULE_GAIA}/shaders/gaia_tonemapping_billboard_fs.glsl")
            );

            addProperty(_magnitudeBoost);
            addProperty(_sharpness);
            addProperty(_billboardSize);
            addProperty(_closeUpBoostDist);
            //addProperty(_pointSpreadFunctionTexturePath);
            break;
    }

    // Common uniforms for all shaders:
    _uniformCache.model = _program->uniformLocation("model");
    _uniformCache.view = _program->uniformLocation("view");
    _uniformCache.projection = _program->uniformLocation("projection");
    _uniformCache.time = _program->uniformLocation("time");
    _uniformCache.renderOption = _program->uniformLocation("renderOption");
    _uniformCache.viewScaling = _program->uniformLocation("viewScaling");
    _uniformCache.cutOffThreshold = _program->uniformLocation("cutOffThreshold");
    _uniformCache.luminosityMultiplier = _program->uniformLocation(
        "luminosityMultiplier"
    );
    _uniformCache.colorTexture = _program->uniformLocation("colorTexture");

    _uniformFilterCache.posXThreshold = _program->uniformLocation("posXThreshold");
    _uniformFilterCache.posYThreshold = _program->uniformLocation("posYThreshold");
    _uniformFilterCache.posZThreshold = _program->uniformLocation("posZThreshold");
    _uniformFilterCache.gMagThreshold = _program->uniformLocation("gMagThreshold");
    _uniformFilterCache.bpRpThreshold = _program->uniformLocation("bpRpThreshold");
    _uniformFilterCache.distThreshold = _program->uniformLocation("distThreshold");

    _uniformCacheTM.renderedTexture = _programTM->uniformLocation("renderedTexture");


    // Find out how much GPU memory this computer has (Nvidia cards).
    GLint nDedicatedVidMemoryInKB = 0;
    glGetIntegerv(GL_GPU_MEMORY_INFO_DEDICATED_VIDMEM_NVX, &nDedicatedVidMemoryInKB);
    GLint nTotalMemoryInKB = 0;
    glGetIntegerv(GL_GPU_MEMORY_INFO_TOTAL_AVAILABLE_MEMORY_NVX, &nTotalMemoryInKB);
    GLint nCurrentAvailMemoryInKB = 0;
    glGetIntegerv(
        GL_GPU_MEMORY_INFO_CURRENT_AVAILABLE_VIDMEM_NVX,
        &nCurrentAvailMemoryInKB
    );

    LDEBUG(std::format(
        "nDedicatedVidMemoryKB: {} - nTotalMemoryKB: {} - nCurrentAvailMemoryKB: {}",
        nDedicatedVidMemoryInKB, nTotalMemoryInKB, nCurrentAvailMemoryInKB
    ));

    // Set ceiling for video memory to use in streaming.
    const float dedicatedVidMem = static_cast<float>(
        static_cast<long long>(nDedicatedVidMemoryInKB) * 1024
    );
    // TODO: Need to fix what happens if we can't query! For now use 2 GB by default.
    _gpuMemoryBudgetInBytes = dedicatedVidMem > 0 ?
        static_cast<long long>(dedicatedVidMem * _maxGpuMemoryPercent) :
        2147483648;

    // Set ceiling for how much of the installed CPU RAM to use for streaming
    const long long installedRam =
        static_cast<long long>(CpuCap.installedMainMemory()) * 1024 * 1024;
    // TODO: What to do if we can't query? As for now we use 4 GB by default.
    _cpuRamBudgetInBytes = installedRam > 0 ?
        static_cast<long long>(static_cast<float>(installedRam) * _maxCpuMemoryPercent) :
        4294967296;
    _cpuRamBudgetProperty.setMaxValue(static_cast<float>(_cpuRamBudgetInBytes));

    LDEBUG(std::format(
        "GPU Memory Budget (bytes): {} - CPU RAM Budget (bytes): {}",
        _gpuMemoryBudgetInBytes, _cpuRamBudgetInBytes
    ));
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

    if (_program) {
        global::renderEngine->removeRenderProgram(_program.get());
        _program = nullptr;
    }
    if (_programTM) {
        global::renderEngine->removeRenderProgram(_programTM.get());
        _programTM = nullptr;
    }
}

void RenderableGaiaStars::render(const RenderData& data, RendererTasks&) {
    checkGlErrors("Before render");

    // Save current FBO
    GLint defaultFbo = 0;
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFbo);

    const glm::dmat4 model = calcModelTransform(data);

    const float viewScaling = data.camera.scaling();
    const glm::dmat4 view = data.camera.combinedViewMatrix();
    const glm::dmat4 projection = data.camera.projectionMatrix();

    const glm::dmat4 modelViewProjMat = calcModelViewProjectionTransform(data, model);
    const glm::vec2 screenSize = glm::vec2(global::renderEngine->renderingResolution());

    // Wait until camera has stabilized before we traverse the Octree/stream from files.
    const double rotationDiff = std::abs(length(_previousCameraRotation) -
                                length(data.camera.rotationQuaternion()));
    if (_firstDrawCalls && rotationDiff > 1e-10) {
        _previousCameraRotation = data.camera.rotationQuaternion();
        return;
    }
    else {
        _firstDrawCalls = false;
    }

    // Update which nodes that are stored in memory as the camera moves around
    // (if streaming)
    if (_fileReaderOption == gaia::FileReaderOption::StreamOctree) {
        const glm::dvec3 cameraPos = data.camera.positionVec3();
        const size_t chunkSizeBytes = _chunkSize * sizeof(GLfloat);
        _octreeManager.fetchSurroundingNodes(cameraPos, chunkSizeBytes, _additionalNodes);

        // Update CPU Budget property.
        _cpuRamBudgetProperty = static_cast<float>(_octreeManager.cpuRamBudget());
    }

    // Traverse Octree and build a map with new nodes to render, uses mvp matrix to decide
    const int renderOption = _renderMode;
    int deltaStars = 0;
    const std::map<int, std::vector<float>> updateData = _octreeManager.traverseData(
        modelViewProjMat,
        screenSize,
        deltaStars,
        gaia::RenderMode(renderOption),
        _lodPixelThreshold
    );

    // Update number of rendered stars.
    _nStarsToRender += deltaStars;
    _nRenderedStars = _nStarsToRender;

    // Update GPU Stream Budget property.
    _gpuStreamBudgetProperty = static_cast<float>(_octreeManager.numFreeSpotsInBuffer());

    const int nChunksToRender = static_cast<int>(_octreeManager.biggestChunkIndexInUse());
    const int maxStarsPerNode = static_cast<int>(_octreeManager.maxStarsPerNode());
    const int valuesPerStar = static_cast<int>(_nRenderValuesPerStar);

    // Switch rendering technique depending on user-defined shader option.
    const int shaderOption = _shaderOption;
    if (shaderOption == gaia::ShaderOption::BillboardSSBO ||
        shaderOption == gaia::ShaderOption::PointSSBO ||
        shaderOption == gaia::ShaderOption::BillboardSSBONoFBO)
    {
#ifndef __APPLE__
        //------------------------ RENDER WITH SSBO ---------------------------
        // Update SSBO Index array with accumulated stars in all chunks.
        glBindBuffer(GL_SHADER_STORAGE_BUFFER, _ssboIdx);
        const int lastValue = _accumulatedIndices.back();
        _accumulatedIndices.resize(nChunksToRender + 1, lastValue);

        // Update vector with accumulated indices.
        for (const auto& [offset, subData] : updateData) {
            if (offset >= _accumulatedIndices.size() - 1) {
                // @TODO(2023-03-08, alebo) We want to redo the whole rendering pipeline
                // anyway, so right now we just bail out early if we get an invalid index
                // that would trigger a crash
                continue;
            }

            const int newValue =
                static_cast<int>(subData.size() / _nRenderValuesPerStar) +
                    _accumulatedIndices[offset];
            const int changeInValue = newValue - _accumulatedIndices[offset + 1];
            _accumulatedIndices[offset + 1] = newValue;
            // Propagate change.
            for (int i = offset + 1; i < nChunksToRender; i++) {
                _accumulatedIndices[i + 1] += changeInValue;
            }
        }

        // Fix number of stars rendered if it doesn't correspond to our buffers.
        if (_accumulatedIndices.back() != _nStarsToRender) {
            _nStarsToRender = _accumulatedIndices.back();
            _nRenderedStars = _nStarsToRender;
        }

        const size_t indexBufferSize = _accumulatedIndices.size() * sizeof(GLint);

        // Update SSBO Index (stars per chunk).
        glBufferData(
            GL_SHADER_STORAGE_BUFFER,
            indexBufferSize,
            _accumulatedIndices.data(),
            GL_STREAM_DRAW
        );

        // Use orphaning strategy for data SSBO.
        glBindBuffer(GL_SHADER_STORAGE_BUFFER, _ssboData);

        glBufferData(
            GL_SHADER_STORAGE_BUFFER,
            _maxStreamingBudgetInBytes,
            nullptr,
            GL_STREAM_DRAW
        );

        // Update SSBO with one insert per chunk/node.
        // The key in map holds the offset index.
        for (const auto& [offset, subData] : updateData) {
            // We don't need to fill chunk with zeros for SSBOs!
            // Just check if we have any values to update.
            if (!subData.empty()) {
                glBufferSubData(
                    GL_SHADER_STORAGE_BUFFER,
                    offset * _chunkSize * sizeof(GLfloat),
                    subData.size() * sizeof(GLfloat),
                    subData.data()
                );
            }
        }

        glBindBuffer(GL_SHADER_STORAGE_BUFFER, 0);
#endif // !__APPLE__
    }
    else {
        //---------------------- RENDER WITH VBO -----------------------------
        // Update VBOs with new nodes.
        // This will overwrite old data that's not visible anymore as well.
        glBindVertexArray(_vao);

        // Always update Position VBO.
        glBindBuffer(GL_ARRAY_BUFFER, _vboPos);
        const float posMemoryShare =
            static_cast<float>(PositionSize) / _nRenderValuesPerStar;
        const size_t posChunkSize = maxStarsPerNode * PositionSize;
        const long long posStreamingBudget = static_cast<long long>(
            _maxStreamingBudgetInBytes * posMemoryShare
        );

        // Use buffer orphaning to update a subset of total data.
        glBufferData(
            GL_ARRAY_BUFFER,
            posStreamingBudget,
            nullptr,
            GL_STREAM_DRAW
        );

        // Update buffer with one insert per chunk/node.
        //The key in map holds the offset index.
        for (const auto& [offset, subData] : updateData) {
            // Fill chunk by appending zeroes so we overwrite possible earlier values.
            // Only required when removing nodes because chunks are filled up in octree
            // fetch on add.
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
        if (renderOption != gaia::RenderMode::Static) {
            glBindBuffer(GL_ARRAY_BUFFER, _vboCol);
            const float colMemoryShare =
                static_cast<float>(ColorSize) / _nRenderValuesPerStar;
            const size_t colChunkSize = maxStarsPerNode * ColorSize;
            const long long colStreamingBudget = static_cast<long long>(
                _maxStreamingBudgetInBytes * colMemoryShare
            );

            // Use buffer orphaning to update a subset of total data.
            glBufferData(
                GL_ARRAY_BUFFER,
                colStreamingBudget,
                nullptr,
                GL_STREAM_DRAW
            );

            // Update buffer with one insert per chunk/node.
            //The key in map holds the offset index.
            for (const auto& [offset, subData] : updateData) {
                // Fill chunk by appending zeroes so we overwrite possible earlier values.
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
            if (renderOption == gaia::RenderMode::Motion) {
                glBindBuffer(GL_ARRAY_BUFFER, _vboVel);
                const float velMemoryShare = static_cast<float>(VelocitySize) /
                                       _nRenderValuesPerStar;
                const size_t velChunkSize = maxStarsPerNode * VelocitySize;
                const long long velStreamingBudget = static_cast<long long>(
                    _maxStreamingBudgetInBytes * velMemoryShare
                );

                // Use buffer orphaning to update a subset of total data.
                glBufferData(
                    GL_ARRAY_BUFFER,
                    velStreamingBudget,
                    nullptr,
                    GL_STREAM_DRAW
                );

                // Update buffer with one insert per chunk/node.
                //The key in map holds the offset index.
                for (const auto& [offset, subData] : updateData) {
                    // Fill chunk by appending zeroes.
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

    checkGlErrors("After buffer updates");

    // Activate shader program and send uniforms.
    glBlendFunc(GL_SRC_ALPHA, GL_ONE);
    glDepthMask(false);
    _program->activate();

    _program->setUniform(_uniformCache.model, model);
    _program->setUniform(_uniformCache.view, view);
    _program->setUniform(_uniformCache.projection, projection);
    _program->setUniform(
        _uniformCache.time,
        static_cast<float>(data.time.j2000Seconds())
    );
    _program->setUniform(_uniformCache.renderOption, _renderMode);
    _program->setUniform(_uniformCache.viewScaling, viewScaling);
    _program->setUniform(_uniformCache.cutOffThreshold, _cutOffThreshold);
    _program->setUniform(_uniformCache.luminosityMultiplier, _luminosityMultiplier);

    // Send filterValues.
    _program->setUniform(_uniformFilterCache.posXThreshold, _posXThreshold);
    _program->setUniform(_uniformFilterCache.posYThreshold, _posYThreshold);
    _program->setUniform(_uniformFilterCache.posZThreshold, _posZThreshold);
    _program->setUniform(_uniformFilterCache.gMagThreshold, _gMagThreshold);
    _program->setUniform(_uniformFilterCache.bpRpThreshold, _bpRpThreshold);
    _program->setUniform(_uniformFilterCache.distThreshold, _distThreshold);

    ghoul::opengl::TextureUnit colorUnit;
    if (_colorTexture) {
        colorUnit.activate();
        _colorTexture->bind();
        _program->setUniform(_uniformCache.colorTexture, colorUnit);
    }

    // Specify how many stars we will render. Will be overwritten if rendering billboards
    GLsizei nShaderCalls = _nStarsToRender;

    ghoul::opengl::TextureUnit psfUnit;
    switch (shaderOption) {
        case gaia::ShaderOption::PointSSBO:
            _program->setUniform(_uniformCache.maxStarsPerNode, maxStarsPerNode);
            _program->setUniform(_uniformCache.valuesPerStar, valuesPerStar);
            _program->setUniform(_uniformCache.nChunksToRender, nChunksToRender);
            break;
        case gaia::ShaderOption::PointVBO:
            // Specify how many potential stars we have to render.
            nShaderCalls = maxStarsPerNode * nChunksToRender;
            break;
        case gaia::ShaderOption::BillboardSSBO:
        case gaia::ShaderOption::BillboardSSBONoFBO:
            _program->setUniform(
                _uniformCache.cameraPos,
                data.camera.positionVec3()
            );
            _program->setUniform(
                _uniformCache.cameraLookUp,
                data.camera.lookUpVectorWorldSpace()
            );
            _program->setUniform(_uniformCache.maxStarsPerNode, maxStarsPerNode);
            _program->setUniform(_uniformCache.valuesPerStar, valuesPerStar);
            _program->setUniform(_uniformCache.nChunksToRender, nChunksToRender);

            _program->setUniform(_uniformCache.closeUpBoostDist,
                _closeUpBoostDist * static_cast<float>(distanceconstants::Parsec)
            );
            _program->setUniform(_uniformCache.billboardSize, _billboardSize);
            _program->setUniform(_uniformCache.magnitudeBoost, _magnitudeBoost);
            _program->setUniform(_uniformCache.sharpness, _sharpness);

            psfUnit.activate();
            _pointSpreadFunctionTexture->bind();
            _program->setUniform(_uniformCache.psfTexture, psfUnit);
            break;
        case gaia::ShaderOption::BillboardVBO:
            _program->setUniform(
                _uniformCache.cameraPos,
                data.camera.positionVec3()
            );
            _program->setUniform(
                _uniformCache.cameraLookUp,
                data.camera.lookUpVectorWorldSpace()
            );
            _program->setUniform(_uniformCache.closeUpBoostDist,
                _closeUpBoostDist * static_cast<float>(distanceconstants::Parsec)
            );
            _program->setUniform(_uniformCache.billboardSize, _billboardSize);
            _program->setUniform(_uniformCache.magnitudeBoost, _magnitudeBoost);
            _program->setUniform(_uniformCache.sharpness, _sharpness);

            psfUnit.activate();
            _pointSpreadFunctionTexture->bind();
            _program->setUniform(_uniformCache.psfTexture, psfUnit);

            // Specify how many potential stars we have to render.
            nShaderCalls = maxStarsPerNode * nChunksToRender;
            break;
    }

    if (shaderOption != gaia::ShaderOption::BillboardSSBONoFBO) {
        // Render to FBO.
        glBindFramebuffer(GL_FRAMEBUFFER, _fbo);
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    }

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

    if (shaderOption != gaia::ShaderOption::BillboardSSBONoFBO) {
        // Use ToneMapping shaders and render to default FBO again!
        _programTM->activate();

        glBindFramebuffer(GL_FRAMEBUFFER, defaultFbo);

        ghoul::opengl::TextureUnit fboTexUnit;
        if (_fboTexture) {
            fboTexUnit.activate();
            _fboTexture->bind();
            _programTM->setUniform(_uniformCacheTM.renderedTexture, fboTexUnit);
        }

        if (shaderOption == gaia::ShaderOption::PointSSBO ||
            shaderOption == gaia::ShaderOption::PointVBO)
        {
            _programTM->setUniform(_uniformCacheTM.screenSize, screenSize);
            _programTM->setUniform(_uniformCacheTM.filterSize, _tmPointFilterSize);
            _programTM->setUniform(_uniformCacheTM.sigma, _tmPointSigma);
            _programTM->setUniform(_uniformCacheTM.projection, projection);
            _programTM->setUniform(
                _uniformCacheTM.pixelWeightThreshold,
                _tmPointPixelWeightThreshold
            );
        }

        glBindVertexArray(_vaoQuad);
        glDrawArrays(GL_TRIANGLES, 0, 6); // 2 triangles
        glBindVertexArray(0);

        _programTM->deactivate();
    }

    glDepthMask(true);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    checkGlErrors("After render");
}

void RenderableGaiaStars::checkGlErrors(const std::string& identifier) const {
    if (_reportGlErrors) {
        const GLenum error = glGetError();
        if (error != GL_NO_ERROR) {
            switch (error) {
            case GL_INVALID_ENUM:
                LINFO(identifier + " - GL_INVALID_ENUM");
                break;
            case GL_INVALID_VALUE:
                LINFO(identifier + " - GL_INVALID_VALUE");
                break;
            case GL_INVALID_OPERATION:
                LINFO(identifier + " - GL_INVALID_OPERATION");
                break;
            case GL_INVALID_FRAMEBUFFER_OPERATION:
                LINFO(identifier + " - GL_INVALID_FRAMEBUFFER_OPERATION");
                break;
            case GL_OUT_OF_MEMORY:
                LINFO(identifier + " - GL_OUT_OF_MEMORY");
                break;
            default:
                LINFO(identifier + " - Unknown error");
                break;
            }
        }
    }
}

void RenderableGaiaStars::update(const UpdateData&) {
    const int shaderOption = _shaderOption;
    const int renderOption = _renderMode;

    // Don't update anything if we are in the middle of a rebuild.
    if (_octreeManager.isRebuildOngoing()) {
        return;
    }

    if (_dataIsDirty) {
        LDEBUG("Regenerating data");
        // Reload data file. This may reconstruct the Octree as well.
        const bool success = readDataFile();
        if (!success) {
            throw ghoul::RuntimeError("Error loading Gaia Star data");
        }
        _dataIsDirty = false;
        // Make sure we regenerate buffers if data has reloaded!
        _buffersAreDirty = true;
    }

    if (_program->isDirty() || _shadersAreDirty) {
        switch (shaderOption) {
            case gaia::ShaderOption::PointSSBO: {
#ifndef __APPLE__
                std::unique_ptr<ghoul::opengl::ProgramObject> program =
                    ghoul::opengl::ProgramObject::Build(
                        "GaiaStar",
                        absPath("${MODULE_GAIA}/shaders/gaia_ssbo_vs.glsl"),
                        absPath("${MODULE_GAIA}/shaders/gaia_point_fs.glsl"),
                        absPath("${MODULE_GAIA}/shaders/gaia_point_ge.glsl")
                    );
                if (_program) {
                    global::renderEngine->removeRenderProgram(_program.get());
                }
                _program = std::move(program);

                _uniformCache.maxStarsPerNode = _program->uniformLocation(
                    "maxStarsPerNode"
                );
                _uniformCache.valuesPerStar = _program->uniformLocation(
                    "valuesPerStar"
                );
                _uniformCache.nChunksToRender = _program->uniformLocation(
                    "nChunksToRender"
                );

                // If rebuild was triggered by switching ShaderOption then ssboBinding may
                // not have been initialized yet. Binding will happen in later in
                // buffersAredirty.
                if (!_shadersAreDirty) {
                    _program->setSsboBinding(
                        "ssbo_idx_data",
                        _ssboIdxBinding->bindingNumber()
                    );
                    _program->setSsboBinding(
                        "ssbo_comb_data",
                        _ssboDataBinding->bindingNumber()
                    );
                }
                if (hasProperty(&_magnitudeBoost)) {
                    removeProperty(_magnitudeBoost);
                }
                if (hasProperty(&_sharpness)) {
                    removeProperty(_sharpness);
                }
                if (hasProperty(&_billboardSize)) {
                    removeProperty(_billboardSize);
                }
                if (hasProperty(&_closeUpBoostDist)) {
                    removeProperty(_closeUpBoostDist);
                }
                if (hasProperty(&_pointSpreadFunctionTexturePath)) {
                    removeProperty(_pointSpreadFunctionTexturePath);
                }
                if (!hasProperty(&_tmPointFilterSize)) {
                    addProperty(_tmPointFilterSize);
                }
                if (!hasProperty(&_tmPointSigma)) {
                    addProperty(_tmPointSigma);
                }
                if (!hasProperty(&_tmPointPixelWeightThreshold)) {
                    addProperty(_tmPointPixelWeightThreshold);
                }
    #endif // !__APPLE__
                break;
            }
            case gaia::ShaderOption::PointVBO: {
                std::unique_ptr<ghoul::opengl::ProgramObject> program =
                    ghoul::opengl::ProgramObject::Build(
                        "GaiaStar",
                        absPath("${MODULE_GAIA}/shaders/gaia_vbo_vs.glsl"),
                        absPath("${MODULE_GAIA}/shaders/gaia_point_fs.glsl"),
                        absPath("${MODULE_GAIA}/shaders/gaia_point_ge.glsl")
                    );
                if (_program) {
                    global::renderEngine->removeRenderProgram(_program.get());
                }
                _program = std::move(program);

                if (hasProperty(&_magnitudeBoost)) {
                    removeProperty(_magnitudeBoost);
                }
                if (hasProperty(&_sharpness)) {
                    removeProperty(_sharpness);
                }
                if (hasProperty(&_billboardSize)) {
                    removeProperty(_billboardSize);
                }
                if (hasProperty(&_closeUpBoostDist)) {
                    removeProperty(_closeUpBoostDist);
                }
                if (hasProperty(&_pointSpreadFunctionTexturePath)) {
                    removeProperty(_pointSpreadFunctionTexturePath);
                }
                if (!hasProperty(&_tmPointFilterSize)) {
                    addProperty(_tmPointFilterSize);
                }
                if (!hasProperty(&_tmPointSigma)) {
                    addProperty(_tmPointSigma);
                }
                if (!hasProperty(&_tmPointPixelWeightThreshold)) {
                    addProperty(_tmPointPixelWeightThreshold);
                }
                break;
            }
            case gaia::ShaderOption::BillboardSSBO:
            case gaia::ShaderOption::BillboardSSBONoFBO: {
    #ifndef __APPLE__
                std::unique_ptr<ghoul::opengl::ProgramObject> program;
                if (shaderOption == gaia::ShaderOption::BillboardSSBO) {
                    program = ghoul::opengl::ProgramObject::Build(
                        "GaiaStar",
                        absPath("${MODULE_GAIA}/shaders/gaia_ssbo_vs.glsl"),
                        absPath("${MODULE_GAIA}/shaders/gaia_billboard_fs.glsl"),
                        absPath("${MODULE_GAIA}/shaders/gaia_billboard_ge.glsl")
                    );
                }
                else {
                    program = global::renderEngine->buildRenderProgram("GaiaStar",
                        absPath("${MODULE_GAIA}/shaders/gaia_ssbo_vs.glsl"),
                        absPath("${MODULE_GAIA}/shaders/gaia_billboard_nofbo_fs.glsl"),
                        absPath("${MODULE_GAIA}/shaders/gaia_billboard_ge.glsl")
                    );
                }

                if (_program) {
                    global::renderEngine->removeRenderProgram(_program.get());
                }
                _program = std::move(program);

                _uniformCache.cameraPos = _program->uniformLocation("cameraPos");
                _uniformCache.cameraLookUp = _program->uniformLocation("cameraLookUp");
                _uniformCache.magnitudeBoost = _program->uniformLocation(
                    "magnitudeBoost"
                );
                _uniformCache.sharpness = _program->uniformLocation("sharpness");
                _uniformCache.billboardSize = _program->uniformLocation("billboardSize");
                _uniformCache.closeUpBoostDist = _program->uniformLocation(
                    "closeUpBoostDist"
                );
                _uniformCache.psfTexture = _program->uniformLocation("psfTexture");

                _uniformCache.maxStarsPerNode = _program->uniformLocation(
                    "maxStarsPerNode"
                );
                _uniformCache.valuesPerStar = _program->uniformLocation("valuesPerStar");
                _uniformCache.nChunksToRender = _program->uniformLocation(
                    "nChunksToRender"
                );

                // If rebuild was triggered by switching ShaderOption then ssboBinding
                // may not have been initialized yet. Binding will happen in later in
                // buffersAredirty.
                if (!_shadersAreDirty) {
                    _program->setSsboBinding(
                        "ssbo_idx_data",
                        _ssboIdxBinding->bindingNumber()
                    );
                    _program->setSsboBinding(
                        "ssbo_comb_data",
                        _ssboDataBinding->bindingNumber()
                    );
                }

                if (!hasProperty(&_magnitudeBoost)) {
                    addProperty(_magnitudeBoost);
                }
                if (!hasProperty(&_sharpness)) {
                    addProperty(_sharpness);
                }
                if (!hasProperty(&_billboardSize)) {
                    addProperty(_billboardSize);
                }
                if (!hasProperty(&_closeUpBoostDist)) {
                    addProperty(_closeUpBoostDist);
                }
                if (!hasProperty(&_pointSpreadFunctionTexturePath)) {
                    addProperty(_pointSpreadFunctionTexturePath);
                }
                if (hasProperty(&_tmPointFilterSize)) {
                    removeProperty(_tmPointFilterSize);
                }
                if (hasProperty(&_tmPointSigma)) {
                    removeProperty(_tmPointSigma);
                }
                if (hasProperty(&_tmPointPixelWeightThreshold)) {
                    removeProperty(_tmPointPixelWeightThreshold);
                }
    #endif // !__APPLE__
                break;
            }
            case gaia::ShaderOption::BillboardVBO: {
                std::unique_ptr<ghoul::opengl::ProgramObject> program =
                    ghoul::opengl::ProgramObject::Build(
                        "GaiaStar",
                        absPath("${MODULE_GAIA}/shaders/gaia_vbo_vs.glsl"),
                        absPath("${MODULE_GAIA}/shaders/gaia_billboard_fs.glsl"),
                        absPath("${MODULE_GAIA}/shaders/gaia_billboard_ge.glsl")
                    );
                if (_program) {
                    global::renderEngine->removeRenderProgram(_program.get());
                }
                _program = std::move(program);

                _uniformCache.cameraPos = _program->uniformLocation("cameraPos");
                _uniformCache.cameraLookUp = _program->uniformLocation("cameraLookUp");
                _uniformCache.magnitudeBoost = _program->uniformLocation(
                    "magnitudeBoost"
                );
                _uniformCache.sharpness = _program->uniformLocation("sharpness");
                _uniformCache.billboardSize = _program->uniformLocation("billboardSize");
                _uniformCache.closeUpBoostDist = _program->uniformLocation(
                    "closeUpBoostDist"
                );
                _uniformCache.psfTexture = _program->uniformLocation("psfTexture");

                if (!hasProperty(&_magnitudeBoost)) {
                    addProperty(_magnitudeBoost);
                }
                if (!hasProperty(&_sharpness)) {
                    addProperty(_sharpness);
                }
                if (!hasProperty(&_billboardSize)) {
                    addProperty(_billboardSize);
                }
                if (!hasProperty(&_closeUpBoostDist)) {
                    addProperty(_closeUpBoostDist);
                }
                if (!hasProperty(&_pointSpreadFunctionTexturePath)) {
                    addProperty(_pointSpreadFunctionTexturePath);
                }
                if (hasProperty(&_tmPointFilterSize)) {
                    removeProperty(_tmPointFilterSize);
                }
                if (hasProperty(&_tmPointSigma)) {
                    removeProperty(_tmPointSigma);
                }
                if (hasProperty(&_tmPointPixelWeightThreshold)) {
                    removeProperty(_tmPointPixelWeightThreshold);
                }
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
        _uniformCache.luminosityMultiplier = _program->uniformLocation(
            "luminosityMultiplier"
        );
        _uniformCache.colorTexture = _program->uniformLocation("colorTexture");
        // Filter uniforms:
        _uniformFilterCache.posXThreshold = _program->uniformLocation("posXThreshold");
        _uniformFilterCache.posYThreshold = _program->uniformLocation("posYThreshold");
        _uniformFilterCache.posZThreshold = _program->uniformLocation("posZThreshold");
        _uniformFilterCache.gMagThreshold = _program->uniformLocation("gMagThreshold");
        _uniformFilterCache.bpRpThreshold = _program->uniformLocation("bpRpThreshold");
        _uniformFilterCache.distThreshold = _program->uniformLocation("distThreshold");
    }

    if (_programTM->isDirty() || _shadersAreDirty) {
        switch (shaderOption) {
            case gaia::ShaderOption::PointSSBO:
            case gaia::ShaderOption::PointVBO: {
                std::unique_ptr<ghoul::opengl::ProgramObject> programTM =
                    global::renderEngine->buildRenderProgram(
                        "ToneMapping",
                        absPath("${MODULE_GAIA}/shaders/gaia_tonemapping_vs.glsl"),
                        absPath("${MODULE_GAIA}/shaders/gaia_tonemapping_point_fs.glsl")
                    );
                if (_programTM) {
                    global::renderEngine->removeRenderProgram(_programTM.get());
                }
                _programTM = std::move(programTM);

                _uniformCacheTM.screenSize = _programTM->uniformLocation("screenSize");
                _uniformCacheTM.filterSize = _programTM->uniformLocation("filterSize");
                _uniformCacheTM.sigma = _programTM->uniformLocation("sigma");
                _uniformCacheTM.projection = _programTM->uniformLocation("projection");
                _uniformCacheTM.pixelWeightThreshold = _programTM->uniformLocation(
                    "pixelWeightThreshold"
                );
                break;
            }
            case gaia::ShaderOption::BillboardSSBO:
            case gaia::ShaderOption::BillboardVBO: {
                const std::filesystem::path vs = absPath(
                    "${MODULE_GAIA}/shaders/gaia_tonemapping_vs.glsl"
                );
                const std::filesystem::path fs = absPath(
                    "${MODULE_GAIA}/shaders/gaia_tonemapping_billboard_fs.glsl"
                );
                std::unique_ptr<ghoul::opengl::ProgramObject> programTM =
                    global::renderEngine->buildRenderProgram("ToneMapping", vs, fs);
                if (_programTM) {
                    global::renderEngine->removeRenderProgram(_programTM.get());
                }
                _programTM = std::move(programTM);
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
        if (renderOption == gaia::RenderMode::Static) {
            _nRenderValuesPerStar = PositionSize;
        }
        else if (renderOption == gaia::RenderMode::Color) {
            _nRenderValuesPerStar = PositionSize + ColorSize;
        }
        else { // (renderOption == gaia::RenderOption::Motion)
            _nRenderValuesPerStar = PositionSize + ColorSize + VelocitySize;
        }

        // Calculate memory budgets.
        _chunkSize = _octreeManager.maxStarsPerNode() * _nRenderValuesPerStar;
        const long long totalChunkSizeInBytes =
            _octreeManager.totalNodes() * _chunkSize * sizeof(GLfloat);
        _maxStreamingBudgetInBytes = std::min(
            totalChunkSizeInBytes,
            _gpuMemoryBudgetInBytes
        );
        long long maxNodesInStream = _maxStreamingBudgetInBytes /
                                     (_chunkSize * sizeof(GLfloat));

        _gpuStreamBudgetProperty.setMaxValue(static_cast<float>(maxNodesInStream));
        const bool datasetFitInMemory =
            static_cast<float>(_totalDatasetSizeInBytes) < (_cpuRamBudgetInBytes * 0.9f);

        if (!datasetFitInMemory && !hasProperty(&_additionalNodes)) {
            addProperty(_additionalNodes);
        }
        else if (hasProperty(&_additionalNodes)) {
            removeProperty(_additionalNodes);
        }

        LDEBUG(std::format(
            "Chunk size: {} - Max streaming budget (bytes): {} - Max nodes in stream: {}",
            _chunkSize, _maxStreamingBudgetInBytes, maxNodesInStream
        ));

        // ------------------ RENDER WITH SSBO -----------------------
        if (shaderOption == gaia::ShaderOption::BillboardSSBO ||
            shaderOption == gaia::ShaderOption::PointSSBO ||
            shaderOption == gaia::ShaderOption::BillboardSSBONoFBO)
        {
#ifndef __APPLE__
            _useVBO = false;

            // Trigger a rebuild of buffer data from octree.
            // With SSBO we won't fill the chunks.
            _octreeManager.initBufferIndexStack(
                maxNodesInStream,
                _useVBO,
                datasetFitInMemory
            );
            _nStarsToRender = 0;

            // Generate SSBO Buffers and bind them.
            if (_vaoEmpty == 0) {
                glGenVertexArrays(1, &_vaoEmpty);
                LDEBUG(std::format("Generating Empty Vertex Array id '{}'", _vaoEmpty));
            }
            if (_ssboIdx == 0) {
                glGenBuffers(1, &_ssboIdx);
                LDEBUG(std::format(
                    "Generating Index Shader Storage Buffer Object id '{}'", _ssboIdx
                ));
            }
            if (_ssboData == 0) {
                glGenBuffers(1, &_ssboData);
                LDEBUG(std::format(
                    "Generating Data Shader Storage Buffer Object id '{}'", _ssboData
                ));
            }

            // Bind SSBO blocks to our shader positions.
            // Number of stars per chunk (a.k.a. Index).
            glBindBuffer(GL_SHADER_STORAGE_BUFFER, _ssboIdx);

            _ssboIdxBinding = std::make_unique<ghoul::opengl::BufferBinding<
                ghoul::opengl::bufferbinding::Buffer::ShaderStorage>
            >();
            glBindBufferBase(
                GL_SHADER_STORAGE_BUFFER,
                _ssboIdxBinding->bindingNumber(),
                _ssboIdx
            );
            _program->setSsboBinding("ssbo_idx_data", _ssboIdxBinding->bindingNumber());

            // Combined SSBO with all data.
            glBindBuffer(GL_SHADER_STORAGE_BUFFER, _ssboData);

            _ssboDataBinding = std::make_unique<ghoul::opengl::BufferBinding<
                ghoul::opengl::bufferbinding::Buffer::ShaderStorage>
            >();
            glBindBufferBase(
                GL_SHADER_STORAGE_BUFFER,
                _ssboDataBinding->bindingNumber(),
                _ssboData
            );
            _program->setSsboBinding("ssbo_comb_data", _ssboDataBinding->bindingNumber());

            glBindBuffer(GL_SHADER_STORAGE_BUFFER, 0);

            // Deallocate VBO Buffers if any existed.
            if (_vboPos != 0) {
                glBindBuffer(GL_ARRAY_BUFFER, _vboPos);
                glBufferData(
                    GL_ARRAY_BUFFER,
                    0,
                    nullptr,
                    GL_STREAM_DRAW
                );
            }
            if (_vboCol != 0) {
                glBindBuffer(GL_ARRAY_BUFFER, _vboCol);
                glBufferData(
                    GL_ARRAY_BUFFER,
                    0,
                    nullptr,
                    GL_STREAM_DRAW
                );
            }
            if (_vboVel != 0) {
                glBindBuffer(GL_ARRAY_BUFFER, _vboVel);
                glBufferData(
                    GL_ARRAY_BUFFER,
                    0,
                    nullptr,
                    GL_STREAM_DRAW
                );
            }
            glBindBuffer(GL_ARRAY_BUFFER, 0);
#endif // !__APPLE__
        }
        else { // ------------------ RENDER WITH VBO -----------------------
            _useVBO = true;

            // Trigger a rebuild of buffer data from octree.
            // With VBO we will fill the chunks.
            _octreeManager.initBufferIndexStack(
                maxNodesInStream,
                _useVBO,
                datasetFitInMemory
            );
            _nStarsToRender = 0;

            // Generate VAO and VBOs
            if (_vao == 0) {
                glGenVertexArrays(1, &_vao);
                LDEBUG(std::format("Generating Vertex Array id '{}'", _vao));
            }
            if (_vboPos == 0) {
                glGenBuffers(1, &_vboPos);
                LDEBUG(std::format(
                    "Generating Position Vertex Buffer Object id '{}'", _vboPos
                ));
            }
            if (_vboCol == 0) {
                glGenBuffers(1, &_vboCol);
                LDEBUG(std::format(
                    "Generating Color Vertex Buffer Object id '{}'", _vboCol
                ));
            }
            if (_vboVel == 0) {
                glGenBuffers(1, &_vboVel);
                LDEBUG(std::format(
                    "Generating Velocity Vertex Buffer Object id '{}'", _vboVel
                ));
            }

            // Bind our different VBOs to our vertex array layout.
            glBindVertexArray(_vao);

            switch (renderOption) {
                case gaia::RenderMode::Static: {
                    glBindBuffer(GL_ARRAY_BUFFER, _vboPos);
                    const GLint position = _program->attributeLocation("in_position");
                    glEnableVertexAttribArray(position);

                    glVertexAttribPointer(
                        position,
                        PositionSize,
                        GL_FLOAT,
                        GL_FALSE,
                        0,
                        nullptr
                    );

                    break;
                }
                case gaia::RenderMode::Color: {
                    glBindBuffer(GL_ARRAY_BUFFER, _vboPos);
                    const GLint position = _program->attributeLocation("in_position");
                    glEnableVertexAttribArray(position);

                    glVertexAttribPointer(
                        position,
                        PositionSize,
                        GL_FLOAT,
                        GL_FALSE,
                        0,
                        nullptr
                    );

                    glBindBuffer(GL_ARRAY_BUFFER, _vboCol);
                    const GLint brightness = _program->attributeLocation("in_brightness");
                    glEnableVertexAttribArray(brightness);

                    glVertexAttribPointer(
                        brightness,
                        ColorSize,
                        GL_FLOAT,
                        GL_FALSE,
                        0,
                        nullptr
                    );
                    break;
                }
                case gaia::RenderMode::Motion: {
                    glBindBuffer(GL_ARRAY_BUFFER, _vboPos);
                    const GLint position = _program->attributeLocation("in_position");
                    glEnableVertexAttribArray(position);

                    glVertexAttribPointer(
                        position,
                        PositionSize,
                        GL_FLOAT,
                        GL_FALSE,
                        0,
                        nullptr
                    );

                    glBindBuffer(GL_ARRAY_BUFFER, _vboCol);
                    const GLint brightness = _program->attributeLocation("in_brightness");
                    glEnableVertexAttribArray(brightness);

                    glVertexAttribPointer(
                        brightness,
                        ColorSize,
                        GL_FLOAT,
                        GL_FALSE,
                        0,
                        nullptr
                    );

                    glBindBuffer(GL_ARRAY_BUFFER, _vboVel);
                    const GLint velocity = _program->attributeLocation("in_velocity");
                    glEnableVertexAttribArray(velocity);

                    glVertexAttribPointer(
                        velocity,
                        VelocitySize,
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

#ifndef __APPLE__
            // Deallocate SSBO buffers if they existed.
            if (_ssboIdx != 0) {
                glBindBuffer(GL_SHADER_STORAGE_BUFFER, _ssboIdx);
                glBufferData(
                    GL_SHADER_STORAGE_BUFFER,
                    0,
                    nullptr,
                    GL_STREAM_DRAW
                );
            }
            if (_ssboData != 0) {
                glBindBuffer(GL_SHADER_STORAGE_BUFFER, _ssboData);
                glBufferData(
                    GL_SHADER_STORAGE_BUFFER,
                    0,
                    nullptr,
                    GL_STREAM_DRAW
                );
            }
            glBindBuffer(GL_SHADER_STORAGE_BUFFER, 0);
#endif //!__APPLE__
        }

        // Generate VAO and VBO for Quad.
        if (_vaoQuad == 0) {
            glGenVertexArrays(1, &_vaoQuad);
            LDEBUG(std::format("Generating Quad Vertex Array id '{}'", _vaoQuad));
        }
        if (_vboQuad == 0) {
            glGenBuffers(1, &_vboQuad);
            LDEBUG(std::format("Generating Quad Vertex Buffer Object id '{}'", _vboQuad));
        }

        // Bind VBO and VAO for Quad rendering.
        glBindVertexArray(_vaoQuad);
        glBindBuffer(GL_ARRAY_BUFFER, _vboQuad);

        // Quad for fullscreen.
        constexpr std::array<GLfloat, 24> VboQuadData = {
            -1.f, -1.f, 0.f,
             1.f, -1.f, 0.f,
            -1.f,  1.f, 0.f,
            -1.f,  1.f, 0.f,
             1.f, -1.f, 0.f,
             1.f,  1.f, 0.f,
        };

        glBufferData(
            GL_ARRAY_BUFFER,
            sizeof(VboQuadData),
            VboQuadData.data(),
            GL_STATIC_DRAW
        );

        const GLint tmPositionAttrib = _programTM->attributeLocation("in_position");
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
            LDEBUG(std::format("Generating Framebuffer Object id '{}'", _fbo));
        }
        if (!_fboTexture) {
            // Generate a new texture and attach it to our FBO.
            const glm::vec2 screenSize = glm::vec2(
                global::renderEngine->renderingResolution()
            );
            _fboTexture = std::make_unique<ghoul::opengl::Texture>(
                glm::uvec3(screenSize, 1),
                GL_TEXTURE_2D,
                ghoul::opengl::Texture::Format::RGBA,
                GL_RGBA32F,
                GL_FLOAT
            );
            _fboTexture->uploadTexture();
            LDEBUG("Generating Framebuffer Texture");
        }
        // Bind render texture to FBO.
        glBindFramebuffer(GL_FRAMEBUFFER, _fbo);
        glBindTexture(GL_TEXTURE_2D, *_fboTexture);
        glFramebufferTexture(
            GL_FRAMEBUFFER,
            GL_COLOR_ATTACHMENT0,
            *_fboTexture,
            0
        );
        const GLenum textureBuffer = GL_COLOR_ATTACHMENT0;
        glDrawBuffers(1, &textureBuffer);

        // Check that our framebuffer is ok.
        if (glCheckFramebufferStatus(GL_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE) {
            LERROR("Error when generating GaiaStar Framebuffer");
        }
        glBindFramebuffer(GL_FRAMEBUFFER, 0);

        _buffersAreDirty = false;
    }

    if (_pointSpreadFunctionTextureIsDirty) {
        LDEBUG("Reloading Point Spread Function texture");
        _pointSpreadFunctionTexture = nullptr;
        if (!_pointSpreadFunctionTexturePath.value().empty()) {
            _pointSpreadFunctionTexture = ghoul::io::TextureReader::ref().loadTexture(
                absPath(_pointSpreadFunctionTexturePath),
                2
            );

            if (_pointSpreadFunctionTexture) {
                LDEBUG(std::format(
                    "Loaded texture from '{}'", absPath(_pointSpreadFunctionTexturePath)
               ));
                _pointSpreadFunctionTexture->uploadTexture();
            }
            _pointSpreadFunctionTexture->setFilter(
                ghoul::opengl::Texture::FilterMode::AnisotropicMipMap
            );

            _pointSpreadFunctionFile = std::make_unique<ghoul::filesystem::File>(
                _pointSpreadFunctionTexturePath.value()
            );
            _pointSpreadFunctionFile->setCallback(
                [this]() { _pointSpreadFunctionTextureIsDirty = true; }
            );
        }
        _pointSpreadFunctionTextureIsDirty = false;
    }

    if (_colorTextureIsDirty) {
        LDEBUG("Reloading Color Texture");
        _colorTexture = nullptr;
        if (!_colorTexturePath.value().empty()) {
            _colorTexture = ghoul::io::TextureReader::ref().loadTexture(
                absPath(_colorTexturePath),
                1
            );
            if (_colorTexture) {
                LDEBUG(std::format(
                    "Loaded texture from '{}'", absPath(_colorTexturePath)
                ));
                _colorTexture->uploadTexture();
            }

            _colorTextureFile = std::make_unique<ghoul::filesystem::File>(
                _colorTexturePath.value()
            );
            _colorTextureFile->setCallback([this]() { _colorTextureIsDirty = true; });
        }
        _colorTextureIsDirty = false;
    }

    if (global::windowDelegate->windowHasResized()) {
        // Update FBO texture resolution if we haven't already
        const glm::vec2 screenSize = glm::vec2(
            global::renderEngine->renderingResolution()
        );
        const bool hasChanged = glm::any(
            glm::notEqual(_fboTexture->dimensions(), glm::uvec3(screenSize, 1))
        );

        if (hasChanged) {
            _fboTexture = std::make_unique<ghoul::opengl::Texture>(
                glm::uvec3(screenSize, 1),
                GL_TEXTURE_2D,
                ghoul::opengl::Texture::Format::RGBA,
                GL_RGBA32F,
                GL_FLOAT
            );
            _fboTexture->uploadTexture();
            LDEBUG("Re-Generating Gaia Framebuffer Texture");

            glBindFramebuffer(GL_FRAMEBUFFER, _fbo);
            glBindTexture(GL_TEXTURE_2D, *_fboTexture);
            glFramebufferTexture(
                GL_FRAMEBUFFER,
                GL_COLOR_ATTACHMENT0,
                *_fboTexture,
                0
            );
            const GLenum textureBuffer = GL_COLOR_ATTACHMENT0;
            glDrawBuffers(1, &textureBuffer);

            // Check that our framebuffer is ok.
            if (glCheckFramebufferStatus(GL_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE) {
                LERROR("Error when re-generating GaiaStar Framebuffer");
            }
            glBindFramebuffer(GL_FRAMEBUFFER, 0);
        }
    }
}

bool RenderableGaiaStars::readDataFile() {
    const int fileReaderOption = _fileReaderOption;
    int nReadStars = 0;

    _octreeManager.initOctree(_cpuRamBudgetInBytes);

    std::filesystem::path file = absPath(_filePath.value());
    LINFO(std::format("Loading data file '{}'", file));

    switch (fileReaderOption) {
        case gaia::FileReaderOption::Fits:
            // Read raw fits file and construct Octree
            nReadStars = readFitsFile(file);
            break;
        case gaia::FileReaderOption::Speck:
            // Read raw speck file and construct Octree
            nReadStars = readSpeckFile(file);
            break;
        case gaia::FileReaderOption::BinaryRaw:
            // Stars are stored in an ordered binary file
            nReadStars = readBinaryRawFile(file);
            break;
        case gaia::FileReaderOption::BinaryOctree:
            // Octree already constructed and stored as a binary file
            nReadStars = readBinaryOctreeFile(file);
            break;
        case gaia::FileReaderOption::StreamOctree:
            // Read Octree structure from file, without data
            nReadStars = readBinaryOctreeStructureFile(file);
            break;
        default:
            LERROR("Wrong FileReaderOption - no data file loaded");
            break;
    }

    //_octreeManager->printStarsPerNode();
    _nRenderedStars.setMaxValue(nReadStars);
    LINFO(std::format("Dataset contains a total of {} stars", nReadStars));
    _totalDatasetSizeInBytes = nReadStars * (PositionSize + ColorSize + VelocitySize) * 4;

    return nReadStars > 0;
}

int RenderableGaiaStars::readFitsFile(const std::filesystem::path& filePath) {
    int nReadValuesPerStar = 0;

    FitsFileReader fitsFileReader(false);
    std::vector<float> fullData = fitsFileReader.readFitsFile(
        filePath,
        nReadValuesPerStar,
        _firstRow,
        _lastRow,
        _columnNames
    );

    // Insert stars into octree.
    for (size_t i = 0; i < fullData.size(); i += nReadValuesPerStar) {
        const auto first = fullData.begin() + i;
        const auto last = fullData.begin() + i + nReadValuesPerStar;
        const std::vector<float> starValues(first, last);
        _octreeManager.insert(starValues);
    }
    _octreeManager.sliceLodData();
    return static_cast<int>(fullData.size() / nReadValuesPerStar);
}

int RenderableGaiaStars::readSpeckFile(const std::filesystem::path& filePath) {
    int nReadValuesPerStar = 0;

    FitsFileReader fileReader(false);
    std::vector<float> fullData = fileReader.readSpeckFile(filePath, nReadValuesPerStar);

    // Insert stars into octree.
    for (size_t i = 0; i < fullData.size(); i += nReadValuesPerStar) {
        auto first = fullData.begin() + i;
        auto last = fullData.begin() + i + nReadValuesPerStar;
        const std::vector<float> starValues = std::vector<float>(first, last);
        _octreeManager.insert(starValues);
    }
    _octreeManager.sliceLodData();
    return static_cast<int>(fullData.size() / nReadValuesPerStar);
}

int RenderableGaiaStars::readBinaryRawFile(const std::filesystem::path& filePath) {
    std::vector<float> fullData;
    int nReadStars = 0;

    std::ifstream fileStream(filePath, std::ifstream::binary);
    if (fileStream.good()) {
        int32_t nValues = 0;
        int32_t nReadValuesPerStar = 0;
        const int renderValues = 8;
        fileStream.read(reinterpret_cast<char*>(&nValues), sizeof(int32_t));
        fileStream.read(reinterpret_cast<char*>(&nReadValuesPerStar), sizeof(int32_t));

        fullData.resize(nValues);
        fileStream.read(
            reinterpret_cast<char*>(fullData.data()),
            nValues * sizeof(fullData[0])
        );

        // Insert stars into octree.
        for (size_t i = 0; i < fullData.size(); i += nReadValuesPerStar) {
            auto first = fullData.begin() + i;
            auto last = fullData.begin() + i + renderValues;
            const std::vector<float> starValues(first, last);

            _octreeManager.insert(starValues);
        }
        _octreeManager.sliceLodData();

        nReadStars = nValues / nReadValuesPerStar;
        fileStream.close();
    }
    else {
        LERROR(std::format(
            "Error opening file '{}' for loading raw binary file", filePath
        ));
        return nReadStars;
    }
    return nReadStars;
}

int RenderableGaiaStars::readBinaryOctreeFile(const std::filesystem::path& filePath) {
    int nReadStars = 0;

    std::ifstream fileStream(filePath, std::ifstream::binary);
    if (fileStream.good()) {
        nReadStars = _octreeManager.readFromFile(fileStream, true);

        fileStream.close();
    }
    else {
        LERROR(std::format(
            "Error opening file '{}' for loading binary Octree file", filePath
        ));
        return nReadStars;
    }
    return nReadStars;
}

int RenderableGaiaStars::readBinaryOctreeStructureFile(
                                                  const std::filesystem::path& folderPath)
{
    std::filesystem::path indexFile = folderPath / "index.bin";

    std::ifstream fileStream = std::ifstream(indexFile, std::ifstream::binary);
    if (!fileStream.good()) {
        LERROR(std::format(
            "Error opening file '{}' for loading binary Octree file", indexFile
        ));
        return 0;
    }

    const int nReadStars = _octreeManager.readFromFile(fileStream, false, folderPath);
    return nReadStars;
}

} // namespace openspace
