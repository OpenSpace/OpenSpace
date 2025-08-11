/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include <modules/fieldlinessequence/rendering/renderablefieldlinessequence.h>

#include <modules/fieldlinessequence/fieldlinessequencemodule.h>
#include <modules/fieldlinessequence/util/kameleonfieldlinehelper.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/timemanager.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/textureunit.h>
#include <iostream>
#include <thread>

namespace {
    constexpr std::string_view _loggerCat = "RenderableFieldlinesSequence";

    double extractTriggerTimeFromFilename(const std::filesystem::path& filePath) {
        // number of characters in filename (excluding '.osfls')
        std::string fileName = filePath.stem().string(); // excludes extention

        // Ensure the separators are correct
        fileName.replace(4, 1, "-");
        fileName.replace(7, 1, "-");
        fileName.replace(10, 1, "T");
        fileName.replace(13, 1, ":");
        fileName.replace(16, 1, ":");
        fileName.replace(19, 1, ".");
        return openspace::Time::convertTime(fileName);
    }

    constexpr openspace::properties::Property::PropertyInfo ColorMethodInfo = {
        "ColorMethod",
        "Color Method",
        "Color lines uniformly or using color tables based on extra quantities like, for "
        "examples, temperature or particle density.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo ColorQuantityInfo = {
        "ColorQuantity",
        "Quantity to Color By",
        "Quantity used to color lines if the 'By Quantity' color method is selected.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo ColorMinMaxInfo = {
        "ColorMinMaxRange",
        "Color Min Max Range",
        "A min-max range for what the lowest and highest values of the color table can "
        "be set to.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ColorTablePathInfo = {
        "ColorTablePath",
        "Path to Color Table",
        "Color Table/Transfer Function to use for 'By Quantity' coloring.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ColorUniformInfo = {
        "Color",
        "Uniform Line Color",
        "The uniform color of lines shown when 'Color Method' is set to 'Uniform'.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo ColorUseABlendingInfo = {
        "ABlendingEnabled",
        "Additive Blending",
        "Activate/deactivate additive blending.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo DomainEnabledInfo = {
        "DomainEnabled",
        "Domain Limits",
        "Enable/Disable domain limits.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo DomainXInfo = {
        "LimitsX",
        "X-limits",
        "Valid range along the X-axis. [Min, Max].",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo DomainYInfo = {
        "LimitsY",
        "Y-limits",
        "Valid range along the Y-axis. [Min, Max].",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo DomainZInfo = {
        "LimitsZ",
        "Z-limits",
        "Valid range along the Z-axis. [Min, Max].",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo DomainRInfo = {
        "LimitsR",
        "Radial limits",
        "Valid radial range. [Min, Max].",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo FlowEnabledInfo = {
        "FlowEnabled",
        "Flow Enabled",
        "Toggles the rendering of moving particles along the lines. Can, for example, "
        "illustrate magnetic flow.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo FlowColorInfo = {
        "FlowColor",
        "Flow Color",
        "Color of particles flow direction indication.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo FlowReversedInfo = {
        "ReversedFlow",
        "Reversed Flow",
        "Toggle to make the flow move in the opposite direction.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo FlowParticleSizeInfo = {
        "ParticleSize",
        "Particle Size",
        "Size of the particles.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo FlowParticleSpacingInfo = {
        "ParticleSpacing",
        "Particle Spacing",
        "Spacing inbetween particles.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo FlowSpeedInfo = {
        "FlowSpeed",
        "Speed",
        "Speed of the flow.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo MaskingEnabledInfo = {
        "MaskingEnabled",
        "Masking Enabled",
        "Enable/disable masking. Use masking to show lines where a given quantity is "
        "within a given range, for example, if you only want to see where the "
        "temperature is between 10 and 20 degrees. Also used for masking out line "
        "topologies like solar wind & closed lines.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo MaskingMinMaxInfo = {
        "MaskingMinLimit",
        "Lower Limit",
        "Lower and upper limit of the valid masking range.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo MaskingQuantityInfo = {
        "MaskingQuantity",
        "Quantity used for Masking",
        "Quantity used for masking.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo LineWidthInfo = {
        "LineWidth",
        "Line Width",
        "This value specifies the line width of the fieldlines.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo TimeJumpButtonInfo = {
        "TimeJumpToStart",
        "Jump to Start Of Sequence",
        "Performs a time jump to the start of the sequence.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo SaveDownloadsOnShutdown = {
        "SaveDownloadsOnShutdown",
        "Save Downloads On Shutdown",
        "This is an option for if dynamically downloaded should be saved between runs "
        "or not.",
        openspace::properties::Property::Visibility::User
    };

    // This `Renderable` visualizes field lines, mainly a sequence of time steps but works
    // with only one time step, too. A sequence is a data source consisting of multiple
    // data files that each correspond to a specific time and is therefore time varying
    // like the name of the renderable suggests.
    //
    // `LoadingType` can be specified in two ways;
    //
    // 1. `StaticLoading`: In this case all data is loaded when starting OpenSpace. A
    // `SourceFolder` is then required. The data format is also required to be set with
    // `InputFileType`.
    //
    // 2. `DynamicDownloading`: This case downloads the data during runtime. For this, a
    // few parameters are required: `InfoURL` together with `DataID` will construct a URL
    // that is used for a HTTP request that returns meta data. `DataURL` and `DataID`,
    // together with this meta data, will be used in constructing another HTTP request
    // that returns the list with data files. The `DataID` specify which data source to
    // use.
    //
    // When using CDF data `SeedPointDirectory` is required. Some prior knowledge of the
    // data is needed to use it in this way. `TracingVariable` needs to be set and
    // `ExtraVariables` will have to match what parameters are in the CDF data. Using CDF
    // directly in this `Renderable` is not recommended. Rather use the
    // `KameleonVolumeToFieldlinesTask` task first, to save the data to .osfls or .json.
    struct [[codegen::Dictionary(RenderableFieldlinesSequence)]] Parameters {
        enum class [[codegen::map(openspace::RenderableFieldlinesSequence::ColorMethod)]]
        ColorMethod {
            Uniform,
            ByQuantity [[codegen::key("By Quantity")]]
        };

        // [[codegen::verbatim(ColorMethodInfo.description)]]
        std::optional<ColorMethod> colorMethod;

        // [[codegen::verbatim(ColorQuantityInfo.description)]]
        std::optional<int> colorQuantity;

        // [[codegen::verbatim(ColorUniformInfo.description)]]
        std::optional<glm::vec4> color [[codegen::color()]];

        // A list of paths to transferfunction .txt files containing color tables
        // used for colorizing the fieldlines according to different parameters.
        std::optional<std::vector<std::filesystem::path>> colorTablePaths;

        // Ranges for which their corresponding parameters values will be
        // colorized by. Should be entered as min value, max value.
        std::optional<std::vector<glm::vec2>> colorTableRanges;

        // [[codegen::verbatim(ColorMinMaxInfo.description)]]
        std::optional<glm::vec2> colorMinMaxRange;

        // [[codegen::verbatim(FlowEnabledInfo.description)]]
        std::optional<bool> flowEnabled;

        // [[codegen::verbatim(FlowColorInfo.description)]]
        std::optional<glm::vec4> flowColor [[codegen::color()]];

        // [[codegen::verbatim(FlowReversedInfo.description)]]
        std::optional<bool> reversedFlow;

        // [[codegen::verbatim(FlowParticleSizeInfo.description)]]
        std::optional<int> particleSize;

        // [[codegen::verbatim(FlowParticleSpacingInfo.description)]]
        std::optional<int> particleSpacing;

        // [[codegen::verbatim(FlowSpeedInfo.description)]]
        std::optional<int> flowSpeed;

        // [[codegen::verbatim(MaskingEnabledInfo.description)]]
        std::optional<bool> maskingEnabled;

        // [[codegen::verbatim(MaskingQuantityInfo.description)]]
        std::optional<int> maskingQuantity;

        // Ranges for which their corresponding quantity parameter value will be
        // masked by. Should be entered as a list of min value, max value pairs.
        std::optional<std::vector<glm::vec2>> maskingRanges;

        // Ranges for which their corresponding parameters values will be
        // masked by. Should be entered as a min value, max value pair.
        std::optional<glm::vec2> maskingMinMaxRange;

        // [[codegen::verbatim(DomainEnabledInfo.description)]]
        std::optional<bool> domainEnabled;

        // [[codegen::verbatim(LineWidthInfo.description)]]
        std::optional<float> lineWidth;

        // [[codegen::verbatim(ColorUseABlendingInfo.description)]]
        std::optional<bool> alphaBlendingEnabled;

        // Set if first/last file should render when time is outside of the sequence
        // interval. This can be used regardless of LoadingType. If this value is not
        // specified, the field lines are shown at all times.
        std::optional<bool> showAtAllTimes;

        // If data sets parameter start_time differ from start of run,
        // elapsed_time_in_seconds might be in relation to start of run.
        // ManuelTimeOffset will be added to trigger time.
        std::optional<float> manualTimeOffset;

        enum class [[codegen::map(openspace::fls::Model)]] Model {
            Batsrus,
            Enlil,
            Pfss
        };

        // If the simulation model is not specified, it means that the scaleFactor
        // (scaleToMeters) will be 1.0 assuming meter as input.
        std::optional<Model> simulationModel;

        // Convert the models distance unit, ex. AU to meters for Enlil.
        // 1.0 is default, assuming meters as input.
        // Does not need to be specified if simulationModel is specified.
        // When using a different model, use this value to scale your vertex positions to
        // meters.
        std::optional<float> scaleToMeters;

        enum class [[codegen::map(openspace::RenderableFieldlinesSequence::LoadingType)]]
        LoadingType {
            StaticLoading,
            DynamicDownloading
        };

        // Choose type of loading:
        // `StaticLoading`: Download and load files on startup.
        // `DynamicDownloading`: Download and load files during run time.
        std::optional<LoadingType> loadingType;

        // A maximum number to limit the number of files being downloaded simultaneously.
        std::optional<int> numberOfFilesToQueue;

        // A data ID that corresponds to what dataset to use if using dynamic data
        // downloading.
        std::optional<int> dataID;

        // A URL to a JSON-formatted page with metadata for the `DataURL`.
        // Required if using dynamic downloading.
        std::optional<std::string> infoURL;

        // A URL to a JSON-formatted page with a list of each available data file.
        // Required if using dynamic downloading.
        std::optional<std::string> dataURL;

        enum class
        [[codegen::map(openspace::RenderableFieldlinesSequence::SourceFileType)]]
        SourceFileType {
            Cdf,
            Json,
            Osfls
        };

        // Specify the file format of the data used.
        SourceFileType inputFileType;

        // Path to folder containing the input files.
        std::optional<std::filesystem::path> sourceFolder [[codegen::directory()]];

        // Path to a directory including .txt files that contain seed points. The files
        // need a file name with a timestamp in the format: yyyymmdd_hhmmss.txt.
        // Required if CDF is used as input.
        std::optional<std::filesystem::path> seedPointDirectory [[codegen::directory()]];

        // Extra variables that can be used to color the field lines.
        std::optional<std::vector<std::string>> extraVariables;

        // Which variable in CDF file that represents which vector field in the data to
        // trace field lines in.
        std::optional<std::string> tracingVariable;

        // Decides whether or not to cache the downloaded data between runs. By default,
        // caching is disabled and all downloaded content will be deleted when OpenSpace
        // is shut down. Set to true to save all the downloaded files.
        std::optional<bool> cacheData;
    };
#include "renderablefieldlinessequence_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderableFieldlinesSequence::Documentation() {
    return codegen::doc<Parameters>("fieldlinessequence_renderablefieldlinessequence");
}

RenderableFieldlinesSequence::RenderableFieldlinesSequence(
                                                      const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _colorGroup({ "Color" })
    , _colorMethod(ColorMethodInfo)
    , _colorQuantity(ColorQuantityInfo)
    , _selectedColorRange(
        ColorMinMaxInfo,
        glm::vec2(0.f, 100.f),
        glm::vec2(-5000.f),
        glm::vec2(5000.f)
    )
    , _colorTablePath(ColorTablePathInfo)
    , _colorUniform(
        ColorUniformInfo,
        glm::vec4(0.3f, 0.57f, 0.75f, 0.5f),
        glm::vec4(0.f),
        glm::vec4(1.f)
    )
    , _colorABlendEnabled(ColorUseABlendingInfo, true)
    , _domainEnabled(DomainEnabledInfo, false)
    , _domainGroup({ "Domain" })
    , _domainX(DomainXInfo)
    , _domainY(DomainYInfo)
    , _domainZ(DomainZInfo)
    , _domainR(DomainRInfo)
    , _flowEnabled(FlowEnabledInfo, false)
    , _flowGroup({ "Flow" })
    , _flowColor(
        FlowColorInfo,
        glm::vec4(0.96f, 0.88f, 0.8f, 1.f),
        glm::vec4(0.f),
        glm::vec4(1.f)
    )
    , _flowParticleSize(FlowParticleSizeInfo, 5, 0, 500)
    , _flowParticleSpacing(FlowParticleSpacingInfo, 60, 0, 500)
    , _flowReversed(FlowReversedInfo, false)
    , _flowSpeed(FlowSpeedInfo, 20, 0, 1000)
    , _maskingEnabled(MaskingEnabledInfo, false)
    , _maskingGroup({ "Masking" })
    , _selectedMaskingRange(
        MaskingMinMaxInfo,
        glm::vec2(0.f, 100.f),
        glm::vec2(-5000.f),
        glm::vec2(5000.f)
    )
    , _maskingQuantity(MaskingQuantityInfo)
    , _lineWidth(LineWidthInfo, 1.f, 1.f, 20.f)
    , _jumpToStart(TimeJumpButtonInfo)
    , _saveDownloadsOnShutdown(SaveDownloadsOnShutdown, false)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    addProperty(Fadeable::_opacity);

    _inputFileType = codegen::map<SourceFileType>(p.inputFileType);
    if (p.loadingType.has_value()) {
        _loadingType = codegen::map<LoadingType>(*p.loadingType);
    }
    else {
        _loadingType = LoadingType::StaticLoading;
    }

    if (_loadingType == LoadingType::DynamicDownloading &&
        _inputFileType == SourceFileType::Cdf)
    {
        throw ghoul::RuntimeError(
            "Dynamic loading (or downloading) is only supported for .osfls and .json "
            "files"
        );
    }
    if (_loadingType == LoadingType::StaticLoading && !p.sourceFolder.has_value()) {
        throw ghoul::RuntimeError(
            "Either dynamic downloading parameters or a sync folder must be specified"
        );
    }

    if (p.simulationModel.has_value()) {
        _model = codegen::map<openspace::fls::Model>(*p.simulationModel);
    }
    else {
        _model = fls::Model::Invalid;
    }

    setModelDependentConstants();

    // Setting the scaling factor after model to support the case with unknown models
    // (model = invalid), but scaling factor being specified.
    _scalingFactor = p.scaleToMeters.value_or(_scalingFactor);

    if (_loadingType == LoadingType::DynamicDownloading) {
        if (!p.dataID.has_value()) {
            throw ghoul::RuntimeError(
                "If running with dynamic downloading, DataID needs to be specified"
            );
        }
        _dataID = *p.dataID;

        _nFilesToQueue = static_cast<size_t>(
            p.numberOfFilesToQueue.value_or(_nFilesToQueue)
        );

        if (!p.infoURL.has_value()) {
            throw ghoul::RuntimeError("InfoURL has to be provided");
        }
        _infoURL = *p.infoURL;

        if (!p.dataURL.has_value()) {
            throw ghoul::RuntimeError("DataURL has to be provided");
        }
        _dataURL = *p.dataURL;
    }
    else {
        ghoul_assert(
            p.sourceFolder.has_value(),
            "sourceFolder not specified though it should not be able to get here"
        );
        std::filesystem::path path = p.sourceFolder.value();
        namespace fs = std::filesystem;
        for (const fs::directory_entry& e : fs::directory_iterator(path)) {
            if (!e.is_regular_file()) {
                continue;
            }
            File file = {
                .status = File::FileStatus::Downloaded,
                .path = e.path(),
                .timestamp = -1.0
            };
            _files.push_back(std::move(file));
            if (_files[0].path.empty()) {
                throw ghoul::RuntimeError(std::format(
                    "Error finding file '{}' in folder '{}'",
                    e.path().filename(), path
                ));
            }
        }
        _maxLoadedFiles = _files.size();
    }

    _extraVars = p.extraVariables.value_or(_extraVars);
    _flowEnabled = p.flowEnabled.value_or(_flowEnabled);
    _flowColor = p.flowColor.value_or(_flowColor);
    _flowReversed = p.reversedFlow.value_or(_flowReversed);
    _flowParticleSize = p.particleSize.value_or(_flowParticleSize);
    _flowParticleSpacing = p.particleSpacing.value_or(_flowParticleSpacing);
    _flowSpeed = p.flowSpeed.value_or(_flowSpeed);
    _maskingEnabled = p.maskingEnabled.value_or(_maskingEnabled);
    _maskingQuantityTemp = p.maskingQuantity.value_or(_maskingQuantityTemp);
    _domainEnabled = p.domainEnabled.value_or(_domainEnabled);
    _lineWidth = p.lineWidth.value_or(_lineWidth);
    _colorABlendEnabled = p.alphaBlendingEnabled.value_or(_colorABlendEnabled);
    _renderForever = p.showAtAllTimes.value_or(_renderForever);
    _manualTimeOffset = p.manualTimeOffset.value_or(_manualTimeOffset);
    _saveDownloadsOnShutdown = p.cacheData.value_or(_saveDownloadsOnShutdown);

    if (_loadingType == LoadingType::StaticLoading){
        staticallyLoadFiles(p.seedPointDirectory, p.tracingVariable);
        computeSequenceEndTime();
    }

    _colorTablePath = FieldlinesSequenceModule::DefaultTransferFunctionFile.string();
    if (p.colorTablePaths.has_value()) {
        for (const std::filesystem::path& path : *p.colorTablePaths) {
            if (!std::filesystem::exists(path)) {
                throw ghoul::RuntimeError(std::format(
                    "Color table path '{}' is not a valid file", path
                ));
            }
            _colorTablePaths.emplace_back(path);
        }
    }
    if (!p.colorTablePaths.has_value() || _colorTablePaths.empty()) {
        _colorTablePaths.emplace_back(
            FieldlinesSequenceModule::DefaultTransferFunctionFile
        );
    }
    _colorUniform = p.color.value_or(_colorUniform);
    _colorMethod.addOption(static_cast<int>(ColorMethod::Uniform), "Uniform");
    _colorMethod.addOption(static_cast<int>(ColorMethod::ByQuantity), "By Quantity");
    if (p.colorMethod.has_value()) {
        _colorMethod = static_cast<int>(
            codegen::map<openspace::RenderableFieldlinesSequence::ColorMethod>(
                *p.colorMethod
            )
        );
    }
    else {
        _colorMethod = static_cast<int>(ColorMethod::Uniform);
    }
    _colorQuantityTemp = p.colorQuantity.value_or(_colorQuantityTemp);

    if (p.colorTableRanges.has_value()) {
        _colorTableRanges = *p.colorTableRanges;
        if (_colorQuantityTemp < static_cast<int>(_colorTableRanges.size()) &&
            _colorQuantityTemp >= 0)
        {
            _selectedColorRange = _colorTableRanges[_colorQuantityTemp];
        }
        else {
            _selectedColorRange = _colorTableRanges[0];
        }
    }
    else {
        _colorTableRanges.push_back(glm::vec2(0.f, 1.f));
        _selectedColorRange = glm::vec2(0.f, 1.f);
    }

    if (p.colorMinMaxRange.has_value()) {
        _selectedColorRange.setMinValue(glm::vec2(p.colorMinMaxRange->x));
        _selectedColorRange.setMaxValue(glm::vec2(p.colorMinMaxRange->y));
    }

    if (p.maskingRanges.has_value()) {
        _maskingRanges = *p.maskingRanges;
    }
    else {
        _maskingRanges.push_back(glm::vec2(0.f, 1.f));
        _selectedMaskingRange = glm::vec2(0.f, 1.f);
    }

    if (p.maskingMinMaxRange.has_value()) {
        _selectedMaskingRange.setMinValue(glm::vec2(p.maskingMinMaxRange->x));
        _selectedMaskingRange.setMaxValue(glm::vec2(p.maskingMinMaxRange->y));
    }

    _colorQuantity.onChange([this]() {
        if (_colorTablePaths.empty()) {
            return;
        }
        _shouldUpdateColorBuffer = true;
        // Note that we do not need to set _selectedColorRange in the constructor, due to
        // this onChange being declared before firstupdate() function that sets
        // _colorQuantity.
        if (_colorQuantity < static_cast<int>(_colorTableRanges.size())) {
            _selectedColorRange = _colorTableRanges[_colorQuantity];
        }
        // If fewer data ranges are given than there are parameters in the data, use the
        // first range.
        // @TODO (2025-06-10, elon) We should use a better structure for providing the
        // data, since this creates discrepancy for which range belongs to which parameter
        else {
            _selectedColorRange = _colorTableRanges[0];
        }

        if (_colorQuantity < static_cast<int>(_colorTablePaths.size())) {
            _colorTablePath = _colorTablePaths[_colorQuantity].string();
        }
        else {
            _colorTablePath = _colorTablePaths[0].string();
        }
    });

    // This is to save the changes done in the gui for when you switch between options
    _selectedColorRange.onChange([this]() {
        if (_colorQuantity < static_cast<int>(_colorTableRanges.size())) {
            _colorTableRanges[_colorQuantity] = _selectedColorRange;
        }
    });

    _colorTablePath.onChange([this]() {
        if (std::filesystem::exists(_colorTablePath.value())) {
            _transferFunction =
                std::make_unique<TransferFunction>(_colorTablePath.value());
        }
        else {
            LERROR(std::format(
                "Invalid path '{}' to transfer function. Please enter new path",
                _colorTablePath.value()
            ));
        }
    });

    _maskingQuantity.onChange([this]() {
        _shouldUpdateMaskingBuffer = true;
        if (_maskingQuantity < static_cast<int>(_maskingRanges.size())) {
            _selectedMaskingRange = _maskingRanges[_maskingQuantity];
        }
        else if (!_maskingRanges.empty()) {
            _selectedMaskingRange = _maskingRanges[0];
        }
        else {
            LERROR("Cannot set selected masking range");
        }
    });

    _selectedMaskingRange.onChange([this]() {
        if (_maskingQuantity < static_cast<int>(_maskingRanges.size())) {
            _maskingRanges[_maskingQuantity] = _selectedMaskingRange;
        }
    });

    _jumpToStart.onChange([this]() {
        if (_atLeastOneFileLoaded) {
            global::timeManager->setTimeNextFrame(Time(_files[0].timestamp));
        }
    });
    setupProperties();
}

void RenderableFieldlinesSequence::staticallyLoadFiles(
                           const std::optional<std::filesystem::path>& seedPointDirectory,
                                        const std::optional<std::string>& tracingVariable)
{
    switch (_inputFileType) {
        case SourceFileType::Cdf:
            _seedPointDirectory = seedPointDirectory.value_or(_seedPointDirectory);
            _tracingVariable = tracingVariable.value_or(_tracingVariable);
            for (File& file : _files) {
                if (!tracingVariable.has_value()) {
                    throw ghoul::RuntimeError("No tracing variable specified");
                }
                std::vector<std::string> extraMagVars =
                    fls::extractMagnitudeVarsFromStrings(_extraVars);
                std::unordered_map<std::string, std::vector<glm::vec3>> seedsPerFiles =
                    fls::extractSeedPointsFromFiles(_seedPointDirectory);
                if (seedsPerFiles.empty()) {
                    throw ghoul::RuntimeError("No seed files found");
                }
                fls::convertCdfToFieldlinesState(
                    file.state,
                    file.path.string(),
                    seedsPerFiles,
                    _manualTimeOffset,
                    _tracingVariable,
                    _extraVars,
                    extraMagVars
                );
            }
            break;
        case SourceFileType::Json:
            for (File& file : _files) {
                file.state.loadStateFromJson(file.path.string(), _model, _scalingFactor);
            }
            break;
        case SourceFileType::Osfls:
            for (File& file : _files) {
                loadFile(file);
            }
            break;
        default:
            throw ghoul::MissingCaseException();
    }

    _isLoadingStateFromDisk = false;
    for (File& file : _files) {
        if (!file.path.empty() && file.status != File::FileStatus::Loaded) {
            file.status = File::FileStatus::Loaded;
            file.timestamp = extractTriggerTimeFromFilename(file.path);
            _atLeastOneFileLoaded = true;
        }
    }
    std::sort(_files.begin(), _files.end());
}

void RenderableFieldlinesSequence::initialize() {
    _isFirstLoad = true;
}

void RenderableFieldlinesSequence::initializeGL() {
    _shaderProgram = global::renderEngine->buildRenderProgram(
        "FieldlinesSequence",
        absPath("${MODULE_FIELDLINESSEQUENCE}/shaders/fieldlinessequence_vs.glsl"),
        absPath("${MODULE_FIELDLINESSEQUENCE}/shaders/fieldlinessequence_fs.glsl")
    );

    glGenVertexArrays(1, &_vertexArrayObject);
    glGenBuffers(1, &_vertexPositionBuffer);
    glGenBuffers(1, &_vertexColorBuffer);
    glGenBuffers(1, &_vertexMaskingBuffer);

    // Needed for additive blending
    setRenderBin(Renderable::RenderBin::Overlay);
}

void RenderableFieldlinesSequence::setupProperties() {
    addProperty(_colorABlendEnabled);
    addProperty(_lineWidth);
    addProperty(_jumpToStart);

    // Add Property Groups
    addPropertySubOwner(_colorGroup);
    addPropertySubOwner(_domainGroup);
    addPropertySubOwner(_flowGroup);
    addPropertySubOwner(_maskingGroup);

    _colorUniform.setViewOption(properties::Property::ViewOptions::Color);
    _colorGroup.addProperty(_colorUniform);
    _colorGroup.addProperty(_colorMethod);
    _colorGroup.addProperty(_colorQuantity);
    _selectedColorRange.setViewOption(properties::Property::ViewOptions::MinMaxRange);
    _colorGroup.addProperty(_selectedColorRange);
    _colorGroup.addProperty(_colorTablePath);

    _domainGroup.addProperty(_domainEnabled);
    _domainGroup.addProperty(_domainX);
    _domainGroup.addProperty(_domainY);
    _domainGroup.addProperty(_domainZ);
    _domainGroup.addProperty(_domainR);

    _flowGroup.addProperty(_flowEnabled);
    _flowGroup.addProperty(_flowReversed);
    _flowColor.setViewOption(properties::Property::ViewOptions::Color);
    _flowGroup.addProperty(_flowColor);
    _flowGroup.addProperty(_flowParticleSize);
    _flowGroup.addProperty(_flowParticleSpacing);
    _flowGroup.addProperty(_flowSpeed);

    _maskingGroup.addProperty(_maskingEnabled);
    _maskingGroup.addProperty(_maskingQuantity);
    _selectedMaskingRange.setViewOption(properties::Property::ViewOptions::MinMaxRange);
    _maskingGroup.addProperty(_selectedMaskingRange);

    addProperty(_saveDownloadsOnShutdown);
}

void RenderableFieldlinesSequence::setModelDependentConstants() {
    float limit = 100.f; // Just used as a default value.
    switch (_model) {
        case fls::Model::Batsrus:
            _scalingFactor = fls::ReToMeter;
            limit = 300.f; // Should include a long magnetotail
            break;
        case fls::Model::Enlil:
            _flowReversed = true;
            _scalingFactor = fls::AuToMeter;
            limit = 50.f; // Should include Plutos furthest distance from the Sun
            break;
        case fls::Model::Pfss:
            _scalingFactor = fls::RsToMeter;
            limit = 100.f; // Just a default value far away from the solar surface
            break;
        default:
            break;
    }
    _domainX.setMinValue(glm::vec2(-limit));
    _domainX.setMaxValue(glm::vec2(limit));

    _domainY.setMinValue(glm::vec2(-limit));
    _domainY.setMaxValue(glm::vec2(limit));

    _domainZ.setMinValue(glm::vec2(-limit));
    _domainZ.setMaxValue(glm::vec2(limit));

    // Radial should range from 0 out to a corner of the cartesian box:
    // sqrt(3) = 1.732..., 1.75 is a nice and round number
    _domainR.setMinValue(glm::vec2(0.f));
    _domainR.setMaxValue(glm::vec2(limit * 1.75f));

    _domainX = glm::vec2(-limit, limit);
    _domainY = glm::vec2(-limit, limit);
    _domainZ = glm::vec2(-limit, limit);
    _domainR = glm::vec2(0.f, limit * 1.5f);
}

void RenderableFieldlinesSequence::deinitializeGL() {
    glDeleteVertexArrays(1, &_vertexArrayObject);
    _vertexArrayObject = 0;

    glDeleteBuffers(1, &_vertexPositionBuffer);
    _vertexPositionBuffer = 0;

    glDeleteBuffers(1, &_vertexColorBuffer);
    _vertexColorBuffer = 0;

    glDeleteBuffers(1, &_vertexMaskingBuffer);
    _vertexMaskingBuffer = 0;

    if (_shaderProgram) {
        global::renderEngine->removeRenderProgram(_shaderProgram.get());
        _shaderProgram = nullptr;
    }

    _files.clear();

    if (_loadingType == LoadingType::DynamicDownloading && _dynamicFileDownloader) {
        _dynamicFileDownloader->deinitialize(_saveDownloadsOnShutdown);
    }
}

void RenderableFieldlinesSequence::computeSequenceEndTime() {
    if (_files.empty()) {
        _sequenceEndTime = 0.f;
    }
    else if (_files.size() == 1) {
        _sequenceEndTime = _files[0].timestamp + 7200.f;
        if (_loadingType == LoadingType::StaticLoading && !_renderForever) {
            // TODO (2025-06-10, Elon) Alternativly check at construction and throw
            // exeption.
            LWARNING(
                "Only one file in data set, but ShowAtAllTimes set to false. Using a 2h "
                "window after the files time stamp to visualize the data"
            );
        }
    }
    else if (_files.size() > 1) {
        const double lastTriggerTime = _files.back().timestamp;
        const double sequenceDuration = lastTriggerTime - _files[0].timestamp;
        const double averageCadence = sequenceDuration / (_files.size() - 1);
        // A multiplier of 3 to the average cadence is added at the end as a buffer
        // 3 because if you start it just before new data came in, you might just be
        // outside the sequence end time otherwise
        _sequenceEndTime = lastTriggerTime + 3 * averageCadence;
    }
}

void RenderableFieldlinesSequence::loadFile(File& file) {
    _isLoadingStateFromDisk = true;
    try {
        if (_inputFileType == SourceFileType::Osfls) {
            file.state = FieldlinesState::createStateFromOsfls(file.path.string());
        }
        else if (_inputFileType == SourceFileType::Json) {
            file.state.loadStateFromJson(
                file.path.string(),
                fls::Model::Invalid,
                _scalingFactor
            );
        }
    }
    catch (const ghoul::RuntimeError& e) {
        LERRORC(e.component, e.message);
    }
}

void RenderableFieldlinesSequence::trackOldest(File& file) {
    if (file.status == File::FileStatus::Loaded) {
        std::deque<File*>::iterator it =
            std::find(_loadedFiles.begin(), _loadedFiles.end(), &file);
        if (it == _loadedFiles.end()) {
            _loadedFiles.push_back(&file);
        }
        // Repopulate the queue if new File makes the queue full
        if (!_loadedFiles.empty() &&
            _loadingType == LoadingType::DynamicDownloading &&
            _loadedFiles.size() >= _maxLoadedFiles)
        {
            File* oldest = _loadedFiles.front();
            // The edge case of when queue just got full and user jumped back to where
            // they started which would make the oldes file in queue to be the active
            // file. In that case we need to make sure we do not unload it
            if (oldest == &file) {
                return;
            }
            oldest->status = File::FileStatus::Downloaded;
            oldest->state.clear();
            _loadedFiles.pop_front();
        }
    }
}

int RenderableFieldlinesSequence::updateActiveIndex(double currentTime) {
    if (_files.empty()) {
        return -1;
    }
    // Sets index to 0 if time is at the first file time stamp and also,
    // if size == 1 then we can expect to not have a sequence and wants to show
    // the one file of fieldlines at all points in time
    if (_files.begin()->timestamp == currentTime || _files.size() == 1) {
        return 0;
    }

    const std::deque<File>::const_iterator iter = std::upper_bound(
        _files.begin(),
        _files.end(),
        currentTime,
        [](double timeRef, const File& fileRef) {
            return timeRef < fileRef.timestamp;
        }
    );

    if (iter == _files.begin()) {
        return 0;
    }
    else if (iter != _files.end()) {
        return static_cast<int>(std::distance(_files.cbegin(), iter));
    }
    else {
        return static_cast<int>(_files.size()) - 1;
    }
}

bool RenderableFieldlinesSequence::isReady() const {
    return _shaderProgram != nullptr;
}

void RenderableFieldlinesSequence::updateDynamicDownloading(double currentTime,
                                                            double deltaTime)
{
    _dynamicFileDownloader->update(currentTime, deltaTime);
    const std::vector<std::filesystem::path>& filesToRead =
        _dynamicFileDownloader->downloadedFiles();
    for (const std::filesystem::path& filePath : filesToRead) {
        File newFile = {
            .status = File::FileStatus::Downloaded,
            .path = filePath,
            .timestamp = extractTriggerTimeFromFilename(filePath.filename())
        };
        const std::deque<File>::const_iterator iter = std::upper_bound(
            _files.begin(),
            _files.end(),
            newFile.timestamp,
            [](double timeRef, const File& fileRef) {
                return timeRef < fileRef.timestamp;
            }
        );
        std::deque<File>::iterator it = _files.insert(iter, std::move(newFile));
        trackOldest(*it);
    }

    // If all files are moved into _sourceFiles then we can
    // empty the DynamicFileSequenceDownloader's downloaded files list
    _dynamicFileDownloader->clearDownloaded();
}

void RenderableFieldlinesSequence::firstUpdate() {
    std::deque<File>::iterator file = std::find_if(
        _files.begin(),
        _files.end(),
        [](File& f) { return f.status == File::FileStatus::Loaded; }
    );
    if (file == _files.end()) {
        return;
    }

    const std::vector<std::vector<float>>& quantities = file->state.extraQuantities();
    const std::vector<std::string>& extraNamesVec =
        file->state.extraQuantityNames();

    for (size_t i = 0; i < quantities.size(); ++i) {
        _colorQuantity.addOption(static_cast<int>(i), extraNamesVec[i]);
        _maskingQuantity.addOption(static_cast<int>(i), extraNamesVec[i]);
    }
    _colorQuantity = _colorQuantityTemp;
    _maskingQuantity = _maskingQuantityTemp;

    if (_colorQuantity < static_cast<int>(_colorTablePaths.size())) {
        _colorTablePath = _colorTablePaths[_colorQuantity].string();
    }
    else {
        _colorTablePath = _colorTablePaths[0].string();
    }

    if (std::filesystem::exists(_colorTablePath.value())) {
        _transferFunction = std::make_unique<TransferFunction>(_colorTablePath.value());
    }
    else {
        LWARNING("Invalid path to transfer function, please enter new path");
        _colorTablePath = FieldlinesSequenceModule::DefaultTransferFunctionFile.string();
        _transferFunction =
            std::make_unique<TransferFunction>(_colorTablePath.stringValue());
    }

    _shouldUpdateColorBuffer = true;
    _shouldUpdateMaskingBuffer = true;

    _isFirstLoad = false;
}

void RenderableFieldlinesSequence::update(const UpdateData& data) {
    if (_shaderProgram->isDirty()) {
        _shaderProgram->rebuildFromFile();
    }
    const double currentTime = data.time.j2000Seconds();
    const double deltaTime = global::timeManager->deltaTime();

    if (_loadingType == LoadingType::DynamicDownloading) {
        if (!_dynamicFileDownloader) {
            const std::string& identifier = parent()->identifier();
            _dynamicFileDownloader = std::make_unique<DynamicFileSequenceDownloader>(
                _dataID,
                identifier,
                _infoURL,
                _dataURL,
                _nFilesToQueue
            );
        }
        updateDynamicDownloading(currentTime, deltaTime);
        computeSequenceEndTime();
    }
    // At least one file in data set needs to be loaded to read extra variables
    if (_isFirstLoad && _atLeastOneFileLoaded) {
        firstUpdate();
    }

    _inInterval = !_files.empty() &&
        currentTime >= _files[0].timestamp &&
        currentTime < _sequenceEndTime;

    // For the sake of this if statment, it is easiest to think of activeIndex as the
    // previous index and nextIndex as the current
    const int nextIndex = _activeIndex + 1;
    // if _activeIndex is -1 but we are in interval, it means we were before the start
    //     of the sequence in the previous frame
    if (_activeIndex == -1 ||
        // if currentTime < active timestamp, it means that we stepped back to a
        // time represented by another state
        // _activeIndex has already been checked if it is <0 in the line above
        currentTime < _files[_activeIndex].timestamp ||
        // if currentTime >= next timestamp, it means that we stepped forward to a
        // time represented by another state
        (nextIndex < static_cast<int>(_files.size()) &&
        currentTime >= _files[nextIndex].timestamp) ||
        // The case when we jumped passed last file. where nextIndex is not < file.size()
        currentTime >= _files[_activeIndex].timestamp)
    {
        int previousIndex = _activeIndex;
        _activeIndex = updateActiveIndex(currentTime);
        // check index again after updating
        if (_activeIndex == -1) {
            return;
        }
        File& file = _files[_activeIndex];
        if (file.status == File::FileStatus::Downloaded) {
            // if LoadingType is StaticLoading all files will be Loaded
            // would be optimal if loading of next file would happen in the background
            loadFile(file);
            _isLoadingStateFromDisk = false;
            file.status = File::FileStatus::Loaded;
            file.timestamp = extractTriggerTimeFromFilename(file.path);
            _atLeastOneFileLoaded = true;
            computeSequenceEndTime();
            trackOldest(file);
        }
        // If we have a new index, buffers needs to update
        if (previousIndex != _activeIndex) {
            _shouldUpdateColorBuffer = true;
            _shouldUpdateMaskingBuffer = true;
        }

        updateVertexPositionBuffer();
    }

    if (_shouldUpdateColorBuffer) {
        updateVertexColorBuffer();
    }

    if (_shouldUpdateMaskingBuffer) {
        updateVertexMaskingBuffer();
    }
}

void RenderableFieldlinesSequence::render(const RenderData& data, RendererTasks&) {
    if (_files.empty() || _isFirstLoad) {
        return;
    }
    if (!_inInterval && !_renderForever) {
        return;
    }

    _shaderProgram->activate();

    // Calculate Model View MatrixProjection
    const glm::dmat4 rotMat = glm::dmat4(data.modelTransform.rotation);
    const glm::dmat4 modelMat =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
        rotMat *
        glm::dmat4(glm::scale(glm::dmat4(1), glm::dvec3(data.modelTransform.scale)));
    const glm::dmat4 modelViewMat = data.camera.combinedViewMatrix() * modelMat;

    _shaderProgram->setUniform(
        "modelViewProjection",
        data.camera.sgctInternal.projectionMatrix() * glm::mat4(modelViewMat)
    );

    _shaderProgram->setUniform("colorMethod", _colorMethod);
    _shaderProgram->setUniform("lineColor", _colorUniform);
    _shaderProgram->setUniform("usingDomain", _domainEnabled);
    _shaderProgram->setUniform("usingMasking", _maskingEnabled);

    if (_colorMethod == static_cast<int>(ColorMethod::ByQuantity)) {
        ghoul::opengl::TextureUnit textureUnit;
        textureUnit.activate();
        _transferFunction->bind();
        _shaderProgram->setUniform("transferFunction", textureUnit);
        _shaderProgram->setUniform("selectedColorRange", _selectedColorRange);
    }

    if (_maskingEnabled) {
        _shaderProgram->setUniform("maskingRange", _selectedMaskingRange);
    }

    _shaderProgram->setUniform("domainLimR", _domainR.value() * _scalingFactor);
    _shaderProgram->setUniform("domainLimX", _domainX.value() * _scalingFactor);
    _shaderProgram->setUniform("domainLimY", _domainY.value() * _scalingFactor);
    _shaderProgram->setUniform("domainLimZ", _domainZ.value() * _scalingFactor);

    // Flow / Particles
    _shaderProgram->setUniform("flowColor", _flowColor);
    _shaderProgram->setUniform("usingParticles", _flowEnabled);
    _shaderProgram->setUniform("particleSize", _flowParticleSize);
    _shaderProgram->setUniform("particleSpacing", _flowParticleSpacing);
    _shaderProgram->setUniform("particleSpeed", _flowSpeed);
    _shaderProgram->setUniform(
        "time",
        global::windowDelegate->applicationTime() * (_flowReversed ? -1 : 1)
    );

    _shaderProgram->setUniform("opacity", opacity());

    bool additiveBlending = false;
    if (_colorABlendEnabled) {
        additiveBlending = true;
        glDepthMask(false);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE);
    }

    glBindVertexArray(_vertexArrayObject);
#ifndef __APPLE__
    glLineWidth(_lineWidth);
#else
    glLineWidth(1.f);
#endif

    int loadedIndex = _activeIndex;
    if (loadedIndex == -1) {
        return;
    }
    while (_files[loadedIndex].status != File::FileStatus::Loaded) {
        --loadedIndex;
        if (loadedIndex < 0) {
            LWARNING("No file at or before current time is loaded");
            return;
        }
    }

    const FieldlinesState& state = _files[loadedIndex].state;
    glMultiDrawArrays(
        GL_LINE_STRIP,
        state.lineStart().data(),
        state.lineCount().data(),
        static_cast<GLsizei>(state.lineStart().size())
    );

    glBindVertexArray(0);
    _shaderProgram->deactivate();

    if (additiveBlending) {
        // Restores OpenGL Rendering State
        global::renderEngine->openglStateCache().resetBlendState();
        global::renderEngine->openglStateCache().resetDepthState();
    }
}

// Unbind buffers and arrays
void unbindGL() {
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);
}

void RenderableFieldlinesSequence::updateVertexPositionBuffer() {
    glBindVertexArray(_vertexArrayObject);
    glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer);

    const FieldlinesState& state = _files[_activeIndex].state;
    const std::vector<glm::vec3>& vertPos = state.vertexPositions();

    glBufferData(
        GL_ARRAY_BUFFER,
        vertPos.size() * sizeof(glm::vec3),
        vertPos.data(),
        GL_STATIC_DRAW
    );

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, nullptr);

    unbindGL();
}

void RenderableFieldlinesSequence::updateVertexColorBuffer() {
    glBindVertexArray(_vertexArrayObject);
    glBindBuffer(GL_ARRAY_BUFFER, _vertexColorBuffer);

    const FieldlinesState& state = _files[_activeIndex].state;
    bool success = false;
    const std::vector<float>& quantities = state.extraQuantity(_colorQuantity, success);

    if (success) {
        glBufferData(
            GL_ARRAY_BUFFER,
            quantities.size() * sizeof(float),
            quantities.data(),
            GL_STATIC_DRAW
        );

        glEnableVertexAttribArray(1);
        glVertexAttribPointer(1, 1, GL_FLOAT, GL_FALSE, 0, nullptr);

        _shouldUpdateColorBuffer = false;
    }
    unbindGL();
}

void RenderableFieldlinesSequence::updateVertexMaskingBuffer() {
    glBindVertexArray(_vertexArrayObject);
    glBindBuffer(GL_ARRAY_BUFFER, _vertexMaskingBuffer);

    const FieldlinesState& state = _files[_activeIndex].state;
    bool success = false;
    const std::vector<float>& quantities = state.extraQuantity(_maskingQuantity, success);

    if (success) {
        glBufferData(
            GL_ARRAY_BUFFER,
            quantities.size() * sizeof(float),
            quantities.data(),
            GL_STATIC_DRAW
        );

        glEnableVertexAttribArray(2);
        glVertexAttribPointer(2, 1, GL_FLOAT, GL_FALSE, 0, nullptr);

        unbindGL();
        _shouldUpdateMaskingBuffer = false;
    }
}

} // namespace openspace
