/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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
#include <openspace/util/spicemanager.h>
#include <openspace/documentation/verifier.h>
#include <ghoul/fmt.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionaryluaformatter.h>

namespace {
    constexpr const char* _loggerCat = "KameleonVolumeToFieldlinesTask";
    constexpr const char* KeyInput = "Input";
    constexpr const char* KeyTimeKernel = "TimeKernel";
    constexpr const char* KeyOutputFolder = "OutputFolder";
    constexpr const char* KeySeedpoints = "Seedpoints";
    constexpr const char* KeyTracingVar = "TracingVar";
    constexpr const char* KeyExtraScalarVars = "ExtraScalarVars";
    constexpr const char* KeyExtraMagnitudeVars = "ExtraMagnitudeVars";
} // namespace

namespace openspace {

documentation::Documentation KameleonVolumeToFieldlinesTask::documentation() {
    using namespace documentation;
    return {
        "KameleonVolumeToFieldlinesTask",
        "kameleon_metadata_to_fieldlines_task",
        {
            {
                "Type",
                new StringEqualVerifier("KameleonVolumeToFieldlinesTask"),
                Optional::No,
                "The type of this task",
            },
            {
                KeyInput,
                new StringAnnotationVerifier("A file path to a cdf file"),
                Optional::No,
                "The cdf file to extract data from",
            },
            {
                KeyTimeKernel,
                new StringAnnotationVerifier("A file path to a cdf file"),
                Optional::No,
                "A file path to a tls spice kernel used for time",
            },
            {
                KeyTracingVar,
                new StringAnnotationVerifier("A kameleon variable name"),
                Optional::No,
                "The name of the kameleon variable to use for tracing",
            },
            {
                KeyOutputFolder,
                new StringAnnotationVerifier("A valid path to a folder"),
                Optional::No,
                "The folder to write the osfls file to",
            },
            {
                KeySeedpoints,
                new StringAnnotationVerifier("A valid filepath"),
                Optional::No,
                "A text file with seedpoints with the format x1 y1 z1 x2 y2 z2 ... "
                "Seedpoints are expressed in the native coordinate system of the model."
            },
            {
                KeyExtraScalarVars,
                new StringListVerifier("A list of kameleon variables"),
                Optional::Yes,
                "A list of scalar variables to extract along the fieldlines",
            },
            {
                KeyExtraMagnitudeVars,
                new StringListVerifier("A list of kameleon variables"),
                Optional::Yes,
                "A list of vector variables whose magnitude to extract along the "
                "fieldlines",
            },
        }
    };
}


KameleonVolumeToFieldlinesTask::KameleonVolumeToFieldlinesTask(
                                                      const ghoul::Dictionary& dictionary)
{
    openspace::documentation::testSpecificationAndThrow(
        documentation(),
        dictionary,
        "KameleonVolumeToFieldlinesTask"
    );
    _inputPath = absPath(dictionary.value<std::string>(KeyInput));
    _timeKernelPath = absPath(dictionary.value<std::string>(KeyTimeKernel));
    _seedpointsPath = absPath(dictionary.value<std::string>(KeySeedpoints));
    _outputFolder = absPath(dictionary.value<std::string>(KeyOutputFolder));
    _tracingVar = dictionary.value<std::string>(KeyTracingVar);

    if (dictionary.hasKey(KeyExtraScalarVars)) {
        ghoul::Dictionary list = dictionary.value<ghoul::Dictionary>(KeyExtraScalarVars);
        for (size_t i = 0; i < list.size(); ++i) {
            _extraScalarVars.push_back(list.value<std::string>(std::to_string(i)));
        }
    }
    if (dictionary.hasKey(KeyExtraMagnitudeVars)) {
        ghoul::Dictionary list =
            dictionary.value<ghoul::Dictionary>(KeyExtraMagnitudeVars);

        for (size_t i = 0; i < list.size(); ++i) {
            _extraMagnitudeVars.push_back(list.value<std::string>(std::to_string(i)));
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

    std::vector<glm::vec3> seedPoints;
    bool readSeedpoints = fls::extractSeedPointsFromFile(_seedpointsPath, seedPoints);

    if (!readSeedpoints) {
        LERROR("Falied to read seedpoints");
        return;
    }

    SpiceManager::ref().loadKernel(_timeKernelPath);

    FieldlinesState newState;
    bool isSuccessful = fls::convertCdfToFieldlinesState(
        newState,
        _inputPath,
        seedPoints,
        _tracingVar,
        _extraScalarVars,
        _extraMagnitudeVars
    );

    if (isSuccessful) {
        return newState.saveStateToOsfls(_outputFolder);
    }

    // Ideally, we would want to signal about progress earlier as well, but
    // convertCdfToFieldlinesState does all the work encapsulated in one function call.
    progressCallback(1.0f);
}

} // namespace openspace
