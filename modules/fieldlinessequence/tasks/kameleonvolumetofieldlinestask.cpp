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

#include <modules/fieldlinessequence/tasks/kameleonvolumetofieldlinestask.h>

#include <modules/fieldlinessequence/util/fieldlinesstate.h>
#include <modules/fieldlinessequence/util/kameleonfieldlinehelper.h>
#include <modules/volume/rawvolumewriter.h>
#include <openspace/documentation/verifier.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/time.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionaryluaformatter.h>

namespace {
    constexpr std::string_view _loggerCat = "KameleonVolumeToFieldlinesTask";

    // This task class traces field lines from volume data. It takes a task file which
    // specifies a folder with .cdf files, as well as a file that lists seed points from
    // which the tracing starts. For the outputs, specify an `outputFolder` for where
    // the field lines data will be saved and the `OutputType` parameter to be either
    // `Osfls` which is an OpenSpace specific binary format for field lines, or `Json` for
    // a readable version of the same data. Some knowledge of the data might be needed,
    // especially if coloring the field lines according to some data parameter like
    // temperature or density. These parameters needs to be specified in the `ScalarVars`
    // and match the name in the input data. For the magnitude of a vector parameter, such
    // as magnetic stength or velocity, there are specified in `MagntitudeVars`.
    struct [[codegen::Dictionary(KameleonVolumeToFieldlinesTask)]] Parameters {
        // The folder to the cdf files to extract data from.
        std::filesystem::path input [[codegen::directory()]];

        // Choose to decrease cadence and only use every nth time step / input file.
        std::optional<int> nthTimeStep;

        // A path to folder with text files with seedpoints.
        // The format of points: x1 y1 z1 x2 y2 z2 ...
        // Seedpoints are expressed in the native coordinate system of the model.
        // Filename must match date and time for CDF file.
        std::filesystem::path seedpoints [[codegen::directory()]];

        // Choose to only include every nth seedpoint from each file.
        std::optional<int> nthSeedpoint;

        // If data sets parameter start_time differ from start of run,
        // elapsed_time_in_seconds might be in relation to start of run.
        // ManualTimeOffset will be added to trigger time.
        std::optional<float> manualTimeOffset;

        // The name of the kameleon variable to use for tracing, like b, or u.
        std::string tracingVar;

        // The folder to write the files to.
        std::filesystem::path outputFolder [[codegen::directory()]];

        enum class [[codegen::map(openspace::KameleonVolumeToFieldlinesTask::OutputType)]]
        OutputType {
            Json,
            Osfls
        };
        // Output type
        OutputType outputType;

        // A list of scalar variables to extract along the fieldlines
        // like temperature or density.
        std::optional<std::vector<std::string>> scalarVars;

        // A list of vector field variables. Must be in groups of 3,
        // for example \"bx, by, bz\", \"ux, uy, uz\".
        std::optional<std::vector<std::string>> magnitudeVars;
    };
#include "kameleonvolumetofieldlinestask_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation KameleonVolumeToFieldlinesTask::Documentation() {
    return codegen::doc<Parameters>(
        "fieldlinesequence_kameleon_volume_to_fieldlines_task"
    );
}

KameleonVolumeToFieldlinesTask::KameleonVolumeToFieldlinesTask(
                                                      const ghoul::Dictionary& dictionary)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _inputPath = p.input;
    _nthTimeStep = p.nthTimeStep.value_or(_nthTimeStep);
    _seedpointsPath = p.seedpoints;
    _nthSeedPoint = p.nthSeedpoint.value_or(_nthSeedPoint);
    _manualTimeOffset = p.manualTimeOffset.value_or(_manualTimeOffset);
    _outputFolder = p.outputFolder;
    if (_outputFolder.string().back() != '/') {
        _outputFolder += '/';
    }
    _outputType = codegen::map<OutputType>(p.outputType);
    _tracingVar = p.tracingVar;

    _scalarVars = p.scalarVars.value_or(std::vector<std::string>());
    if (_scalarVars.empty()) {
        LINFO("No scalar variable was specified to be saved");
    }

    _magnitudeVars = p.magnitudeVars.value_or(std::vector<std::string>());
    if (_magnitudeVars.empty()) {
        LINFO("No vector field variable was specified to be saved");
    }

    namespace fs = std::filesystem;
    for (const fs::directory_entry& e : fs::directory_iterator(_inputPath)) {
        if (e.is_regular_file()) {
            std::string eStr = e.path().string();
            _sourceFiles.push_back(eStr);
        }
    }
}

std::string KameleonVolumeToFieldlinesTask::description() {
    return std::format(
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
        fls::extractSeedPointsFromFiles(_seedpointsPath, _nthSeedPoint);
    if (seedPoints.empty()) {
        throw ghoul::RuntimeError("Failed to read seed points");
    }

    size_t fileNumber = 0;
    for (const std::string& cdfPath : _sourceFiles) {
        if (fileNumber % _nthTimeStep != 0) {
            continue;
        }

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
            switch (_outputType) {
            case OutputType::Osfls:
                newState.saveStateToOsfls(_outputFolder.string());
                break;
            case OutputType::Json:
                {
                    std::string timeStr =
                        std::string(Time(newState.triggerTime()).ISO8601());
                    timeStr.replace(13, 1, "-");
                    timeStr.replace(16, 1, "-");
                    timeStr.replace(19, 1, "-");
                    std::string fileName = timeStr;
                    newState.saveStateToJson(_outputFolder.string() + fileName);
                    break;
                }
            default :
                throw ghoul::MissingCaseException();
            }
        }
        ++fileNumber;
    }

    // Ideally, we would want to signal about progress earlier as well, but
    // convertCdfToFieldlinesState does all the work encapsulated in one function call.
    progressCallback(1.f);
}

} // namespace openspace
