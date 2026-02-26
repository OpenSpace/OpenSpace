/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/distanceconstants.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/format.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/exception.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/systemcapabilities/generalcapabilitiescomponent.h>
#include <algorithm>
#include <array>
#include <cstdint>
#include <cstdlib>
#include <fstream>
#include <set>
#include <utility>

namespace {
    using namespace openspace;

    constexpr std::string_view _loggerCat = "RenderableGaiaStars";

    constexpr size_t PositionSize = 3;
    constexpr size_t ColorSize = 2;
    constexpr size_t VelocitySize = 3;

    constexpr Property::PropertyInfo FilePathInfo = {
        "File",
        "File path",
        "The path to the file with data for the stars to be rendered.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo FileReaderOptionInfo = {
        "FileReaderOption",
        "File reader option",
        "This value tells the renderable what format the input data file has. "
        "'Fits' will read a FITS file, construct an Octree from it and render full "
        "data. 'Speck' will read a SPECK file, construct an Octree from it and render "
        "full data. 'BinaryRaw' will read a preprocessed binary file with ordered star "
        "data, construct an Octree and render it. 'BinaryOctree' will read a constructed "
        "Octree from binary file and render full data. 'StreamOctree' will read an index "
        "file with full Octree structure and then stream nodes during runtime. (This "
        "option is suited for bigger datasets).",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo RenderModeInfo = {
        "RenderMode",
        "Render mode",
        "This value determines which predefined columns to use in rendering. If "
        "'Static' only the position of the stars is used. 'Color' uses position + color "
        "parameters and 'Motion' uses pos, color as well as velocity for the stars.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo LuminosityMultiplierInfo = {
        "LuminosityMultiplier",
        "Luminosity multiplier",
        "Factor by which to multiply the luminosity with. [Works in Color and Motion "
        "modes].",
        Property::Visibility::User
    };

    constexpr Property::PropertyInfo CutOffThresholdInfo = {
        "CutOffThreshold",
        "Cut off threshold",
        "Set threshold for when to cut off star rendering. Stars closer than this "
        "threshold are given full opacity. Farther away, stars dim proportionally to the "
        "4-logarithm of their distance.",
        Property::Visibility::User
    };

    constexpr Property::PropertyInfo TmPointFilterSizeInfo = {
        "FilterSize",
        "Filter size [px]",
        "Set the filter size in pixels used in tonemapping for point splatting "
        "rendering.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo TmPointSigmaInfo = {
        "Sigma",
        "Normal distribution sigma",
        "Set the normal distribution sigma used in tonemapping for point splatting "
        "rendering.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo AdditionalNodesInfo = {
        "AdditionalNodes",
        "Additional nodes",
        "Determines how many additional nodes around the camera that will be fetched "
        "from disk. The first value determines how many additional layers of parents "
        "that will be fetched. The second value determines how many layers of descendant "
        "that will be fetched from the found parents.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo TmPointPxThresholdInfo = {
        "PixelWeightThreshold",
        "Pixel weight threshold",
        "Set the threshold for how big the elliptic weight of a pixel has to be to "
        "contribute to the final elliptic shape. A smaller value gives a more visually "
        "pleasing result while a bigger value will speed up the rendering on skewed "
        "frustums (aka Domes).",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo ColorTextureInfo = {
        "ColorMap",
        "Color texture",
        "The path to the texture that is used to convert from the magnitude of the star "
        "to its color. The texture is used as a one dimensional lookup function.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo FirstRowInfo = {
        "FirstRow",
        "First row to read",
        "Defines the first row that will be read from the specified FITS file No need to "
        "define if data already has been processed. [Works only with "
        "FileReaderOption::Fits].",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo LastRowInfo = {
        "LastRow",
        "Last row to read",
        "Defines the last row that will be read from the specified FITS file; has to be "
        "equal to or greater than FirstRow. No need to define if data already has been "
        "processed. [Works only with FileReaderOption::Fits].",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo NumRenderedStarsInfo = {
        "NumRenderedStars",
        "Rendered stars",
        "The number of rendered stars in the current frame.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo CpuRamBudgetInfo = {
        "CpuRamBudget",
        "CPU RAM budget",
        "Current remaining budget (bytes) on the CPU RAM for loading more node data "
        "files.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo GpuStreamBudgetInfo = {
        "GpuStreamBudget",
        "GPU stream budget",
        "Current remaining memory budget [in number of chunks] on the GPU for streaming "
        "additional stars.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo LodPixelThresholdInfo = {
        "LodPixelThreshold",
        "LOD pixel threshold",
        "The number of total pixels a nodes AABB can have in clipping space before its "
        "parent is fetched as LOD cache.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo MaxGpuMemoryPercentInfo = {
        "MaxGpuMemoryPercent",
        "Max GPU memory",
        "Sets the max percent of existing GPU memory budget that the streaming will use.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo MaxCpuMemoryPercentInfo = {
        "MaxCpuMemoryPercent",
        "Max CPU memory",
        "Sets the max percent of existing CPU memory budget that the streaming of files "
        "will use.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo FilterPosXInfo = {
        "FilterPosX",
        "PosX threshold",
        "If defined then only stars with Position X values between [min, max] will be "
        "rendered (if min is set to 0.0 it is read as -Inf, if max is set to 0.0 it is "
        "read as +Inf). Measured in KiloParsec.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo FilterPosYInfo = {
        "FilterPosY",
        "PosY threshold",
        "If defined then only stars with Position Y values between [min, max] will be "
        "rendered (if min is set to 0.0 it is read as -Inf, if max is set to 0.0 it is "
        "read as +Inf). Measured in KiloParsec.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo FilterPosZInfo = {
        "FilterPosZ",
        "PosZ threshold",
        "If defined then only stars with Position Z values between [min, max] will be "
        "rendered (if min is set to 0.0 it is read as -Inf, if max is set to 0.0 it is "
        "read as +Inf). Measured in KiloParsec.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo FilterGMagInfo = {
        "FilterGMag",
        "GMag threshold",
        "If defined then only stars with G mean magnitude values between [min, max] will "
        "be rendered (if min is set to 20.0 it is read as -Inf, if max is set to 20.0 it "
        "is read as +Inf). If min = max then all values equal min|max will be filtered "
        "away.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo FilterBpRpInfo = {
        "FilterBpRp",
        "Bp-Rp threshold",
        "If defined then only stars with Bp-Rp color values between [min, max] will be "
        "rendered (if min is set to 0.0 it is read as -Inf, if max is set to 0.0 it is "
        "read as +Inf). If min = max then all values equal min|max will be filtered "
        "away.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo FilterDistInfo = {
        "FilterDist",
        "Dist threshold",
        "If defined then only stars with Distances values between [min, max] will be "
        "rendered (if min is set to 0.0 it is read as -Inf, if max is set to 0.0 it is "
        "read as +Inf). Measured in KiloParsec.",
        Property::Visibility::AdvancedUser
    };

    struct [[codegen::Dictionary(RenderableGaiaStars)]] Parameters {
        // [[codegen::verbatim(FilePathInfo.description)]]
        std::string file;

        enum class [[codegen::map(openspace::FileReaderOption)]] FileReader {
            Fits,
            Speck,
            BinaryRaw,
            BinaryOctree,
            StreamOctree
        };
        // [[codegen::verbatim(FileReaderOptionInfo.description)]]
        FileReader fileReaderOption;

        enum class [[codegen::map(openspace::RenderMode)]] RenderMode {
            Static,
            Color,
            Motion
        };
        // [[codegen::verbatim(RenderModeInfo.description)]]
        std::optional<RenderMode> renderMode;

        // [codegen::verbatim(ColorTextureInfo.description)]]
        std::string colorMap;

        // [codegen::verbatim(LuminosityMultiplierInfo.description)]]
        std::optional<float> luminosityMultiplier;

        // [codegen::verbatim(CutOffThresholdInfo.description)]]
        std::optional<float> cutOffThreshold;

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

        // A list of strings with the names of all the columns that are to be read from
        // the specified FITS file. No need to define if data already has been processed.
        // [Works only with FileReaderOption::Fits].
        std::optional<ghoul::Dictionary> columnNames;

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
} // namespace
#include "renderablegaiastars_codegen.cpp"

namespace openspace {

Documentation RenderableGaiaStars::Documentation() {
    return codegen::doc<Parameters>("gaiamission_renderablegaiastars");
}

RenderableGaiaStars::RenderableGaiaStars(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _filePath(FilePathInfo)
    , _colorTexturePath(ColorTextureInfo)
    , _luminosityMultiplier(LuminosityMultiplierInfo, 35.f, 1.f, 250.f)
    , _cutOffThreshold(CutOffThresholdInfo, 38.f, 0.f, 50.f)
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
    , _fileReaderOption(FileReaderOptionInfo)
    , _renderMode(RenderModeInfo)
    , _nRenderedStars(NumRenderedStarsInfo, 0, 0, 2000000000) // 2 Billion stars
    , _cpuRamBudgetProperty(CpuRamBudgetInfo, 0.f, 0.f, 1.f)
    , _gpuStreamBudgetProperty(GpuStreamBudgetInfo, 0.f, 0.f, 1.f)
    , _maxGpuMemoryPercent(MaxGpuMemoryPercentInfo, 0.45f, 0.f, 1.f)
    , _maxCpuMemoryPercent(MaxCpuMemoryPercentInfo, 0.5f, 0.f, 1.f)
    , _accumulatedIndices(1, 0)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _dataFile = std::make_unique<ghoul::filesystem::File>(p.file);
    _dataFile->setCallback([this]() { _dataIsDirty = true; });

    _filePath = p.file;
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
        { FileReaderOption::Fits, "Fits" },
        { FileReaderOption::Speck, "Speck" },
        { FileReaderOption::BinaryRaw, "BinaryRaw" },
        { FileReaderOption::BinaryOctree, "BinaryOctree" },
        { FileReaderOption::StreamOctree, "StreamOctree" }
    });
    _fileReaderOption = codegen::map<FileReaderOption>(p.fileReaderOption);

    _renderMode.addOptions({
        { RenderMode::Static, "Static" },
        { RenderMode::Color, "Color" },
        { RenderMode::Motion, "Motion" }
    });
    if (p.renderMode.has_value()) {
        _renderMode = codegen::map<RenderMode>(*p.renderMode);
    }
    _renderMode.onChange([this]() { _buffersAreDirty = true; });
    addProperty(_renderMode);

    _colorTextureFile = std::make_unique<ghoul::filesystem::File>(p.colorMap);
    _colorTextureFile->setCallback([this]() { _colorTextureIsDirty = true; });
    _colorTexturePath = p.colorMap;
    _colorTexturePath.onChange([this]() { _colorTextureIsDirty = true; });
    addProperty(_colorTexturePath);

    _luminosityMultiplier = p.luminosityMultiplier.value_or(_luminosityMultiplier);
    addProperty(_luminosityMultiplier);

    _cutOffThreshold = p.cutOffThreshold.value_or(_cutOffThreshold);
    addProperty(_cutOffThreshold);

    _tmPointFilterSize = p.filterSize.value_or(_tmPointFilterSize);
    addProperty(_tmPointFilterSize);

    _tmPointSigma = p.sigma.value_or(_tmPointSigma);
    addProperty(_tmPointSigma);

    _tmPointPixelWeightThreshold =
        p.pixelWeightThreshold.value_or(_tmPointPixelWeightThreshold);
    addProperty(_tmPointPixelWeightThreshold);

    _additionalNodes = p.additionalNodes.value_or(_additionalNodes);
    addProperty(_additionalNodes);

    _lodPixelThreshold = p.lodPixelThreshold.value_or(_lodPixelThreshold);
    addProperty(_lodPixelThreshold);

    _maxGpuMemoryPercent = p.maxGpuMemoryPercent.value_or(_maxGpuMemoryPercent);
    _maxGpuMemoryPercent.onChange([this]() {
        if (_ssboData != 0) {
            glDeleteBuffers(1, &_ssboData);
            glCreateBuffers(1, &_ssboData);
            LDEBUG(std::format(
                "Re-generating Data Shader Storage Buffer Object id '{}'", _ssboData
            ));
        }

        // Find out our new budget. Use dedicated video memory instead of current
        // available to always be consistant with previous call(s)
        GLint nDedicatedVidMemoryInKB = 0;
        glGetIntegerv(GL_GPU_MEMORY_INFO_DEDICATED_VIDMEM_NVX, &nDedicatedVidMemoryInKB);
        const float dedicatedVidMem = static_cast<float>(
            static_cast<long long>(nDedicatedVidMemoryInKB) * 1024
        );

        // TODO: Need to fix what happens if we can't query! For now use 2 GB by default
        _gpuMemoryBudgetInBytes = dedicatedVidMem > 0 ?
            static_cast<long long>(dedicatedVidMem * _maxGpuMemoryPercent) :
            2147483648;
        _buffersAreDirty = true;
        _maxStreamingBudgetInBytes = 0;
    });
    addProperty(_maxGpuMemoryPercent);

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

    // Only add properties correlated to fits files if we're reading from a fits file
    if (_fileReaderOption == FileReaderOption::Fits) {
        _firstRow = p.firstRow.value_or(_firstRow);
        _firstRow.onChange([this]() { _dataIsDirty = true; });
        addProperty(_firstRow);

        _lastRow = p.lastRow.value_or(_lastRow);
        _lastRow.onChange([this]() { _dataIsDirty = true; });
        addProperty(_lastRow);

        const ghoul::Dictionary d = p.columnNames.value_or(ghoul::Dictionary());

        // Ugly fix for ASCII sorting when there are more columns read than 10
        std::set<int> intKeys;
        for (const std::string_view key : d.keys()) {
            intKeys.insert(std::stoi(std::string(key)));
        }

        for (const int key : intKeys) {
            _columnNames.push_back(d.value<std::string>(std::to_string(key)));
        }

        if (_firstRow > _lastRow) {
            throw ghoul::RuntimeError("User defined FirstRow is bigger than LastRow");
        }
    }

    // Add a read-only property for the number of rendered stars per frame
    _nRenderedStars.setReadOnly(true);
    addProperty(_nRenderedStars);

    // Add CPU RAM Budget Property and GPU Stream Budget Property to menu
    _cpuRamBudgetProperty.setReadOnly(true);
    addProperty(_cpuRamBudgetProperty);

    _gpuStreamBudgetProperty.setReadOnly(true);
    addProperty(_gpuStreamBudgetProperty);
}

bool RenderableGaiaStars::isReady() const {
    return _program && _programTM;
}

void RenderableGaiaStars::initializeGL() {
    _program = ghoul::opengl::ProgramObject::Build(
        "GaiaStar",
        absPath("${MODULE_GAIA}/shaders/gaia_ssbo_vs.glsl"),
        absPath("${MODULE_GAIA}/shaders/gaia_point_fs.glsl"),
        absPath("${MODULE_GAIA}/shaders/gaia_point_gs.glsl")
    );
    ghoul::opengl::updateUniformLocations(*_program, _uniformCache);

    _programTM = global::renderEngine->buildRenderProgram(
        "ToneMapping",
        absPath("${MODULE_GAIA}/shaders/gaia_tonemapping_vs.glsl"),
        absPath("${MODULE_GAIA}/shaders/gaia_tonemapping_point_fs.glsl")
    );
    ghoul::opengl::updateUniformLocations(*_programTM, _uniformCacheTM);

    glCreateVertexArrays(1, &_vaoEmpty);
    glCreateBuffers(1, &_ssboIdx);
    glCreateBuffers(1, &_ssboData);

    glCreateBuffers(1, &_vboQuad);
    constexpr std::array<glm::vec3, 8> VboQuadData = {
        glm::vec3(-1.f, -1.f, 0.f),
        glm::vec3( 1.f, -1.f, 0.f),
        glm::vec3(-1.f,  1.f, 0.f),
        glm::vec3(-1.f,  1.f, 0.f),
        glm::vec3( 1.f, -1.f, 0.f),
        glm::vec3( 1.f,  1.f, 0.f)
    };
    glNamedBufferStorage(_vboQuad, sizeof(VboQuadData), VboQuadData.data(), GL_NONE_BIT);

    glCreateVertexArrays(1, &_vaoQuad);
    glVertexArrayVertexBuffer(_vaoQuad, 0, _vboQuad, 0, sizeof(glm::vec3));

    const GLint tmPositionAttrib = _programTM->attributeLocation("in_position");
    glEnableVertexArrayAttrib(_vaoQuad, tmPositionAttrib);
    glVertexArrayAttribFormat(_vaoQuad, tmPositionAttrib, 3, GL_FLOAT, GL_FALSE, 0);
    glVertexArrayAttribBinding(_vaoQuad, 0, 0);

    glCreateFramebuffers(1, &_fbo);
    // Generate a new texture and attach it to our FBO
    _fboTexture = std::make_unique<ghoul::opengl::Texture>(
        ghoul::opengl::Texture::FormatInit{
            .dimensions = glm::uvec3(global::renderEngine->renderingResolution(), 1),
            .type = GL_TEXTURE_2D,
            .format = ghoul::opengl::Texture::Format::RGBA,
            .dataType = GL_FLOAT
        },
        ghoul::opengl::Texture::SamplerInit{}
    );
    glNamedFramebufferTexture(_fbo, GL_COLOR_ATTACHMENT0, *_fboTexture, 0);
    const GLenum textureBuffer = GL_COLOR_ATTACHMENT0;
    glNamedFramebufferDrawBuffers(_fbo, 1, &textureBuffer);

    // Find out how much GPU memory this computer has (Nvidia cards)
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

    // Set ceiling for video memory to use in streaming
    const float dedicatedVidMem = static_cast<float>(
        static_cast<long long>(nDedicatedVidMemoryInKB) * 1024
    );
    // TODO: Need to fix what happens if we can't query! For now use 2 GB by default
    _gpuMemoryBudgetInBytes = dedicatedVidMem > 0 ?
        static_cast<long long>(dedicatedVidMem * _maxGpuMemoryPercent) :
        2147483648;

    // Set ceiling for how much of the installed CPU RAM to use for streaming
    const long long installedRam =
        static_cast<long long>(CpuCap.installedMainMemory()) * 1024 * 1024;
    // TODO: What to do if we can't query? As for now we use 4 GB by default
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
    glDeleteBuffers(1, &_ssboIdx);
    glDeleteBuffers(1, &_ssboData);
    glDeleteVertexArrays(1, &_vaoEmpty);

    glDeleteBuffers(1, &_vboQuad);
    glDeleteVertexArrays(1, &_vaoQuad);
    glDeleteFramebuffers(1, &_fbo);

    _dataFile = nullptr;
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
    // Wait until camera has stabilized before we traverse the Octree/stream from files
    if (_firstDrawCalls) {
        _firstDrawCalls = false;
        return;
    }

    GLint defaultFbo = 0;
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFbo);

    // Update which nodes are stored in memory as the camera moves around (if streaming)
    if (_fileReaderOption == FileReaderOption::StreamOctree) {
        const glm::dvec3 cameraPos = data.camera.positionVec3();
        const size_t chunkSizeBytes = _chunkSize * sizeof(GLfloat);
        _octreeManager.fetchSurroundingNodes(cameraPos, chunkSizeBytes, _additionalNodes);

        _cpuRamBudgetProperty = static_cast<float>(_octreeManager.cpuRamBudget());
    }

    // Traverse Octree and build a map with new nodes to render, uses mvp matrix to decide
    const glm::dmat4 model = calcModelTransform(data);
    const glm::dmat4 modelViewProjMat = calcModelViewProjectionTransform(data, model);
    const glm::vec2 screenSize = glm::vec2(global::renderEngine->renderingResolution());
    int deltaStars = 0;
    const std::map<int, std::vector<float>> updateData = _octreeManager.traverseData(
        modelViewProjMat,
        screenSize,
        deltaStars,
        RenderMode(_renderMode.value()),
        _lodPixelThreshold
    );

    // Update number of rendered stars
    _nStarsToRender += deltaStars;
    _nRenderedStars = _nStarsToRender;

    // Update GPU Stream Budget property
    _gpuStreamBudgetProperty = static_cast<float>(_octreeManager.numFreeSpotsInBuffer());

    const int nChunksToRender = static_cast<int>(_octreeManager.biggestChunkIndexInUse());

    // Update SSBO Index array with accumulated stars in all chunks
    const int lastValue = _accumulatedIndices.back();
    _accumulatedIndices.resize(nChunksToRender + 1, lastValue);

    // Update vector with accumulated indices
    for (const auto& [offset, subData] : updateData) {
        if (offset >= static_cast<int>(_accumulatedIndices.size()) - 1) {
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
        // Propagate change
        for (int i = offset + 1; i < nChunksToRender; i++) {
            _accumulatedIndices[i + 1] += changeInValue;
        }
    }

    // Fix number of stars rendered if it doesn't correspond to our buffers
    if (_accumulatedIndices.back() != _nStarsToRender) {
        _nStarsToRender = _accumulatedIndices.back();
        _nRenderedStars = _nStarsToRender;
    }

    const size_t idxBufferSize = _accumulatedIndices.size() * sizeof(GLint);

    // Update SSBO Index (stars per chunk)
    glNamedBufferData(
        _ssboIdx,
        idxBufferSize,
        _accumulatedIndices.data(),
        GL_STREAM_DRAW
    );

    // Use orphaning strategy for data SSBO
    glNamedBufferData(_ssboData, _maxStreamingBudgetInBytes, nullptr, GL_STREAM_DRAW);

    // Update SSBO with one insert per chunk/node.
    // The key in map holds the offset index
    for (const auto& [offset, subData] : updateData) {
        // We don't need to fill chunk with zeros for SSBOs.
        // Just check if we have any values to update
        if (!subData.empty()) {
            glNamedBufferSubData(
                _ssboData,
                offset * _chunkSize * sizeof(GLfloat),
                subData.size() * sizeof(GLfloat),
                subData.data()
            );
        }
    }

    glBlendFunc(GL_SRC_ALPHA, GL_ONE);
    glDepthMask(false);
    _program->activate();

    _program->setUniform(_uniformCache.model, model);
    _program->setUniform(_uniformCache.view, data.camera.combinedViewMatrix());
    _program->setUniform(_uniformCache.projection, data.camera.projectionMatrix());
    _program->setUniform(
        _uniformCache.time,
        static_cast<float>(data.time.j2000Seconds())
    );
    _program->setUniform(_uniformCache.renderOption, _renderMode);
    _program->setUniform(_uniformCache.viewScaling, data.camera.scaling());
    _program->setUniform(_uniformCache.cutOffThreshold, _cutOffThreshold);
    _program->setUniform(_uniformCache.luminosityMultiplier, _luminosityMultiplier);

    _program->setUniform(_uniformCache.posXThreshold, _posXThreshold);
    _program->setUniform(_uniformCache.posYThreshold, _posYThreshold);
    _program->setUniform(_uniformCache.posZThreshold, _posZThreshold);
    _program->setUniform(_uniformCache.gMagThreshold, _gMagThreshold);
    _program->setUniform(_uniformCache.bpRpThreshold, _bpRpThreshold);
    _program->setUniform(_uniformCache.distThreshold, _distThreshold);

    ghoul::opengl::TextureUnit colorUnit;
    if (_colorTexture) {
        colorUnit.bind(*_colorTexture);
        _program->setUniform(_uniformCache.colorTexture, colorUnit);
    }

    const int maxStarsPerNode = static_cast<int>(_octreeManager.maxStarsPerNode());
    _program->setUniform(_uniformCache.maxStarsPerNode, maxStarsPerNode);
    const int valuesPerStar = static_cast<int>(_nRenderValuesPerStar);
    _program->setUniform(_uniformCache.valuesPerStar, valuesPerStar);
    _program->setUniform(_uniformCache.nChunksToRender, nChunksToRender);

    glBindFramebuffer(GL_FRAMEBUFFER, _fbo);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glBindVertexArray(_vaoEmpty);
    glDrawArrays(GL_POINTS, 0, _nStarsToRender);
    glBindVertexArray(0);
    _program->deactivate();

    // Use ToneMapping shaders and render to default FBO again
    _programTM->activate();

    ghoul::opengl::TextureUnit fboTexUnit;
    if (_fboTexture) {
        fboTexUnit.bind(*_fboTexture);
        _programTM->setUniform(_uniformCacheTM.renderedTexture, fboTexUnit);
    }
    _programTM->setUniform(_uniformCacheTM.screenSize, screenSize);
    _programTM->setUniform(_uniformCacheTM.filterSize, _tmPointFilterSize);
    _programTM->setUniform(_uniformCacheTM.sigma, _tmPointSigma);
    _programTM->setUniform(_uniformCacheTM.projection, data.camera.projectionMatrix());
    _programTM->setUniform(
        _uniformCacheTM.pixelWeightThreshold,
        _tmPointPixelWeightThreshold
    );

    glBindFramebuffer(GL_FRAMEBUFFER, defaultFbo);
    glBindVertexArray(_vaoQuad);
    glDrawArrays(GL_TRIANGLES, 0, 6); // 2 triangles
    glBindVertexArray(0);

    _programTM->deactivate();

    glDepthMask(true);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
}

void RenderableGaiaStars::update(const UpdateData&) {
    // Don't update anything if we are in the middle of a rebuild
    if (_octreeManager.isRebuildOngoing()) {
        return;
    }

    if (_dataIsDirty) {
        LDEBUG("Regenerating data");
        // Reload data file. This may reconstruct the Octree as well
        const bool success = readDataFile();
        if (!success) {
            throw ghoul::RuntimeError("Error loading Gaia Star data");
        }
        _dataIsDirty = false;
        // Make sure we regenerate buffers if data has reloaded
        _buffersAreDirty = true;
    }

    if (_program->isDirty()) {
        global::renderEngine->removeRenderProgram(_program.get());
        _program = ghoul::opengl::ProgramObject::Build(
            "GaiaStar",
            absPath("${MODULE_GAIA}/shaders/gaia_ssbo_vs.glsl"),
            absPath("${MODULE_GAIA}/shaders/gaia_point_fs.glsl"),
            absPath("${MODULE_GAIA}/shaders/gaia_point_gs.glsl")
        );

        ghoul::opengl::updateUniformLocations(*_program, _uniformCache);

        _program->setSsboBinding("ssbo_idx_data", _ssboIdxBinding->bindingNumber());
        _program->setSsboBinding("ssbo_comb_data", _ssboDataBinding->bindingNumber());
    }

    if (_programTM->isDirty()) {
        global::renderEngine->removeRenderProgram(_programTM.get());
        _programTM = global::renderEngine->buildRenderProgram(
            "ToneMapping",
            absPath("${MODULE_GAIA}/shaders/gaia_tonemapping_vs.glsl"),
            absPath("${MODULE_GAIA}/shaders/gaia_tonemapping_point_fs.glsl")
        );
        ghoul::opengl::updateUniformLocations(*_programTM, _uniformCacheTM);
    }

    if (_buffersAreDirty) {
        LDEBUG("Regenerating buffers");

        // Set values per star slice depending on render option
        if (_renderMode == RenderMode::Static) {
            _nRenderValuesPerStar = PositionSize;
        }
        else if (_renderMode == RenderMode::Color) {
            _nRenderValuesPerStar = PositionSize + ColorSize;
        }
        else { // (renderOption == RenderOption::Motion)
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

        LDEBUG(std::format(
            "Chunk size: {} - Max streaming budget (bytes): {} - Max nodes in stream: {}",
            _chunkSize, _maxStreamingBudgetInBytes, maxNodesInStream
        ));

        // Trigger a rebuild of buffer data from octree.
        // With SSBO we won't fill the chunks
        _octreeManager.initBufferIndexStack(maxNodesInStream, datasetFitInMemory);
        _nStarsToRender = 0;

        // Bind SSBO blocks to our shader positions
        // Number of stars per chunk (a.k.a. Index)
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

        // Combined SSBO with all data
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

        _buffersAreDirty = false;
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
                    "Loaded texture from '{}'", _colorTexturePath.value()
                ));
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
                ghoul::opengl::Texture::FormatInit{
                    .dimensions = glm::uvec3(screenSize, 1),
                    .type = GL_TEXTURE_2D,
                    .format = ghoul::opengl::Texture::Format::RGBA,
                    .dataType = GL_FLOAT
                },
                ghoul::opengl::Texture::SamplerInit{}
            );
            LDEBUG("Re-Generating Gaia Framebuffer Texture");

            glNamedFramebufferTexture(_fbo, GL_COLOR_ATTACHMENT0, *_fboTexture, 0);
            constexpr GLenum TextureBuffer = GL_COLOR_ATTACHMENT0;
            glNamedFramebufferDrawBuffers(_fbo, 1, &TextureBuffer);
        }
    }
}

bool RenderableGaiaStars::readDataFile() {
    _octreeManager.initOctree(_cpuRamBudgetInBytes);

    std::filesystem::path file = absPath(_filePath.value());
    LINFO(std::format("Loading data file '{}'", file));

    int nReadStars = 0;
    switch (_fileReaderOption) {
        case FileReaderOption::Fits:
            // Read raw fits file and construct Octree
            nReadStars = readFitsFile(file);
            break;
        case FileReaderOption::Speck:
            // Read raw speck file and construct Octree
            nReadStars = readSpeckFile(file);
            break;
        case FileReaderOption::BinaryRaw:
            // Stars are stored in an ordered binary file
            nReadStars = readBinaryRawFile(file);
            break;
        case FileReaderOption::BinaryOctree:
            // Octree already constructed and stored as a binary file
            nReadStars = readBinaryOctreeFile(file);
            break;
        case FileReaderOption::StreamOctree:
            // Read Octree structure from file, without data
            nReadStars = readBinaryOctreeStructureFile(file);
            break;
    }

    _nRenderedStars.setMaxValue(nReadStars);
    LINFO(std::format("Dataset contains a total of {} stars", nReadStars));
    _totalDatasetSizeInBytes = nReadStars * (PositionSize + ColorSize + VelocitySize) * 4;
    return nReadStars > 0;
}

int RenderableGaiaStars::readFitsFile(const std::filesystem::path& filePath) {
    int nReadValuesPerStar = 0;

    FitsFileReader fitsFileReader = FitsFileReader(false);
    std::vector<float> fullData = fitsFileReader.readFitsFile(
        filePath,
        nReadValuesPerStar,
        _firstRow,
        _lastRow,
        _columnNames
    );

    // Insert stars into octree
    for (size_t i = 0; i < fullData.size(); i += nReadValuesPerStar) {
        const auto first = fullData.begin() + i;
        const auto last = fullData.begin() + i + nReadValuesPerStar;
        const std::vector<float> starValues = std::vector<float>(first, last);
        _octreeManager.insert(starValues);
    }
    _octreeManager.sliceLodData();
    return static_cast<int>(fullData.size() / nReadValuesPerStar);
}

int RenderableGaiaStars::readSpeckFile(const std::filesystem::path& filePath) {
    int nReadValuesPerStar = 0;

    FitsFileReader fileReader = FitsFileReader(false);
    std::vector<float> fullData = fileReader.readSpeckFile(filePath, nReadValuesPerStar);

    // Insert stars into octree
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
    std::ifstream fileStream = std::ifstream(filePath, std::ifstream::binary);
    if (!fileStream.good()) {
        LERROR(std::format("Error opening binary file '{}'", filePath));
        return 0;
    }

    constexpr int RenderValues = 8;

    int32_t nValues = 0;
    int32_t nReadValuesPerStar = 0;
    fileStream.read(reinterpret_cast<char*>(&nValues), sizeof(int32_t));
    fileStream.read(reinterpret_cast<char*>(&nReadValuesPerStar), sizeof(int32_t));

    std::vector<float> fullData;
    fullData.resize(nValues);
    fileStream.read(reinterpret_cast<char*>(fullData.data()), nValues * sizeof(float));

    // Insert stars into octree
    for (size_t i = 0; i < fullData.size(); i += nReadValuesPerStar) {
        auto first = fullData.begin() + i;
        auto last = fullData.begin() + i + RenderValues;
        const std::vector<float> starValues = std::vector<float>(first, last);
        _octreeManager.insert(starValues);
    }
    _octreeManager.sliceLodData();

    int nReadStars = nValues / nReadValuesPerStar;
    return nReadStars;
}

int RenderableGaiaStars::readBinaryOctreeFile(const std::filesystem::path& filePath) {
    std::ifstream fileStream = std::ifstream(filePath, std::ifstream::binary);
    if (!fileStream.good()) {
        LERROR(std::format("Error opening binary Octree file '{}'", filePath));
        return 0;
    }
    int nReadStars = _octreeManager.readFromFile(fileStream, true);
    return nReadStars;
}

int RenderableGaiaStars::readBinaryOctreeStructureFile(
                                                  const std::filesystem::path& folderPath)
{
    std::filesystem::path indexFile = folderPath / "index.bin";

    std::ifstream fileStream = std::ifstream(indexFile, std::ifstream::binary);
    if (!fileStream.good()) {
        LERROR(std::format("Error opening binary Octree file '{}'", indexFile));
        return 0;
    }

    const int nReadStars = _octreeManager.readFromFile(fileStream, false, folderPath);
    return nReadStars;
}

} // namespace openspace
