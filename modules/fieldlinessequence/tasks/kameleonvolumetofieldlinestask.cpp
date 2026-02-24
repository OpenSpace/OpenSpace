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

#include <modules/fieldlinessequence/tasks/kameleonvolumetofieldlinestask.h>

#include <modules/fieldlinessequence/util/fieldlinesstate.h>
#include <modules/fieldlinessequence/util/kameleonfieldlinehelper.h>
#include <openspace/documentation/documentation.h>
#include <openspace/util/task.h>
#include <openspace/util/time.h>
#include <ghoul/format.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/exception.h>
#include <optional>

namespace {
    constexpr std::string_view _loggerCat = "KameleonVolumeToFieldlinesTask";

    struct [[codegen::Dictionary(KameleonVolumeToFieldlinesTask)]] Parameters {
        // The folder to the cdf files to extract data from.
        std::filesystem::path input [[codegen::directory()]];

        // A text file with seedpoints with the format x1 y1 z1 x2 y2 z2 ...
        // Seedpoints are expressed in the native coordinate system of the model.
        std::filesystem::path seedpoints [[codegen::directory()]];

        // If data sets parameter start_time differ from start of run,
        // elapsed_time_in_seconds might be in relation to start of run.
        // ManuelTimeOffset will be added to trigger time.
        std::optional<double> manualTimeOffset;

        // The name of the kameleon variable to use for tracing, like b, or u.
        std::string tracingVar;

        // The folder to write the files to.
        std::filesystem::path outputFolder [[codegen::directory()]];

        enum class
        [[codegen::map(openspace::KameleonVolumeToFieldlinesTask::OutputType)]]
        OutputType
        {
            Json,
            Osfls
        };

        // Output type. Either osfls (OpenSpace FieldLineSequence) or json
        OutputType outputType;

        // A list of vector variables to extract along the fieldlines
        std::optional<std::vector<std::string>> extraVars;
    };
} // namespace
#include "kameleonvolumetofieldlinestask_codegen.cpp"

namespace openspace {

Documentation KameleonVolumeToFieldlinesTask::Documentation() {
    return codegen::doc<Parameters>("fieldlinessequence_task_kameleonvolumetofieldlines");
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

    _outputType = codegen::map<openspace::KameleonVolumeToFieldlinesTask::OutputType>(
        p.outputType
    );

    _tracingVar = p.tracingVar;

    _extraVars = p.extraVars.value_or(std::vector<std::string>());

    if (!std::filesystem::is_directory(_inputPath)) {
        throw ghoul::RuntimeError(std::format(
            "KameleonVolumeToFieldlineTask: {} is not a valid directory", _inputPath
        ));
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
        "Write either osfls files or json files into the folder {}.",
        _inputPath, _seedpointsPath, _outputFolder
    );
}

void KameleonVolumeToFieldlinesTask::perform(
                                           const Task::ProgressCallback& progressCallback)
{
    std::vector<std::string> extraMagVars = extractMagnitudeVarsFromStrings(_extraVars);

    std::unordered_map<std::string, std::vector<glm::vec3>> seedPoints =
        extractSeedPointsFromFiles(_seedpointsPath);

    if (seedPoints.empty()) {
        LERROR("Falied to read seedpoints");
        return;
    }

    for (const std::string& cdfPath : _sourceFiles) {
        FieldlinesState newState;
        bool isSuccessful = convertCdfToFieldlinesState(
            newState,
            cdfPath,
            seedPoints,
            _manualTimeOffset,
            _tracingVar,
            _extraVars,
            extraMagVars
        );

        if (isSuccessful) {
            if (_outputType == OutputType::Osfls) {
                newState.saveStateToOsfls(_outputFolder.string());
            }
            else if (_outputType == OutputType::Json) {
                std::string timeStr = std::string(Time(newState.triggerTime()).ISO8601());
                timeStr.replace(13, 1, "-");
                timeStr.replace(16, 1, "-");
                timeStr.replace(19, 1, "-");
                std::string fileName = timeStr;
                newState.saveStateToJson(_outputFolder.string() + fileName);
            }
        }
    }

    // Ideally, we would want to signal about progress earlier as well, but
    // convertCdfToFieldlinesState does all the work encapsulated in one function call.
    progressCallback(1.f);
}

} // namespace openspace
