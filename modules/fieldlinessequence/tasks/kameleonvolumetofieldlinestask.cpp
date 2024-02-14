/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#include <modules/fieldlinessequence/tasks/kameleonvolumetofieldlinestask.h>

#include <modules/fieldlinessequence/util/fieldlinesstate.h>
#include <modules/fieldlinessequence/util/kameleonfieldlinehelper.h>
#include <modules/volume/rawvolumewriter.h>
#include <openspace/documentation/verifier.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/time.h>
#include <ghoul/fmt.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionaryluaformatter.h>

namespace {
    constexpr std::string_view _loggerCat = "KameleonVolumeToFieldlinesTask";

    struct [[codegen::Dictionary(KameleonVolumeToFieldlinesTask)]] Parameters {
        // The folder to the cdf files to extract data from
        std::filesystem::path input [[codegen::directory()]];
        // A path to folder with text files with seedpoints.
        // The format of points: x1 y1 z1 x2 y2 z2 ...
        // Seedpoints are expressed in the native coordinate system of the model.
        // Filename must mutch date and time for CDF file
        std::filesystem::path seedpoints [[codegen::directory()]];
        // If data sets parameter start_time differ from start of run,
        // elapsed_time_in_seconds might be in relation to start of run.
        // ManuelTimeOffset will be added to trigger time.
        std::optional<float> manualTimeOffset;
        // The name of the kameleon variable to use for tracing, like b, or u
        std::string tracingVar;
        // The folder to write the files to
        std::filesystem::path outputFolder [[codegen::directory()]];

        enum class [[codegen::map(openspace::KameleonVolumeToFieldlinesTask::OutputType)]] OutputType {
            Json,
            Osfls
        };
        // Output type. Either osfls (OpenSpace FieldLineSequence) or json
        OutputType outputType;
        // A list of scalar variables to extract along the fieldlines
        // like temperature or density
        std::optional<std::vector<std::string>> scalarVars;
        // A list of vector field variables. Must be in groups of 3,
        // for example \"bx, by, bz\", \"ux, uy, uz\"
        std::optional<std::vector<std::string>> magnitudeVars;
    };
#include "kameleonvolumetofieldlinestask_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation KameleonVolumeToFieldlinesTask::Documentation() {
    return codegen::doc<Parameters>("kameleon_volume_to_fieldlines_task");
}

KameleonVolumeToFieldlinesTask::KameleonVolumeToFieldlinesTask(
                                                      const ghoul::Dictionary& dictionary)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _inputPath = p.input;
    _seedpointsPath = p.seedpoints;
    _manualTimeOffset = p.manualTimeOffset.value_or(_manualTimeOffset);
    _outputFolder = p.outputFolder;
    if (_outputFolder.string().back() != '/') {
        _outputFolder += '/';
    }
    _outputType = codegen::map<OutputType>(
        p.outputType
    );
    _tracingVar = p.tracingVar;

    if (p.scalarVars.has_value()) {
        for (auto var : p.scalarVars.value()) {
            _scalarVars.push_back(var);
        }
    }
    else {
        LINFO("No scalar variable was specified to be saved");
    }

    if (p.magnitudeVars.has_value()) {
        for (auto var : p.magnitudeVars.value()) {
            _magnitudeVars.push_back(var);
        }
    }
    else {
        LINFO("No vector field variable was specified to be saved");
    }

    if (!std::filesystem::is_directory(_inputPath)) {
        LERROR(fmt::format(
            "{} is not a valid directory",
            _inputPath
        ));
    }
    for (const auto& e : std::filesystem::directory_iterator(_inputPath)) {
        if (e.is_regular_file()) {
            std::string eStr = e.path().string();
            _sourceFiles.push_back(eStr);
        }
    }

}

std::string KameleonVolumeToFieldlinesTask::description() {
    return fmt::format(
        "Extract fieldline data from cdf file {} and seedpoint file {}. "
        "Write osfls file into the folder {}.",
        _inputPath, _seedpointsPath, _outputFolder
    );
}

void KameleonVolumeToFieldlinesTask::perform(
                                           const Task::ProgressCallback& progressCallback)
{
    std::vector<std::string> extraMagVars =
        fls::extractMagnitudeVarsFromStrings(_magnitudeVars);

    std::unordered_map<std::string, std::vector<glm::vec3>> seedPoints =
        fls::extractSeedPointsFromFiles(_seedpointsPath);
    if (seedPoints.empty()) {
        LERROR("Falied to read seedpoints");
        return;
    }

    //SpiceManager::ref().loadKernel(_timeKernelPath);

    for (const std::string& cdfPath : _sourceFiles) {
        FieldlinesState newState;
        bool isSuccessful = fls::convertCdfToFieldlinesState(
            newState,
            cdfPath,
            seedPoints,
            _manualTimeOffset,
            _tracingVar,
            _scalarVars,
            extraMagVars
        );

        if (isSuccessful) {
            switch(_outputType) {
                case OutputType::Osfls:
                    newState.saveStateToOsfls(absPath(_outputFolder).string());
                    break;
                case OutputType::Json:
                    std::string timeStr =
                        std::string(Time(newState.triggerTime()).ISO8601());
                    timeStr.replace(13, 1, "-");
                    timeStr.replace(16, 1, "-");
                    timeStr.replace(19, 1, "-");
                    std::string fileName = timeStr;
                    newState.saveStateToJson(_outputFolder.string() + fileName);
                    break;
            }
        }
    }
    // Ideally, we would want to signal about progress earlier as well, but
    // convertCdfToFieldlinesState does all the work encapsulated in one function call.
    progressCallback(1.0f);
}


} // namespace openspace
