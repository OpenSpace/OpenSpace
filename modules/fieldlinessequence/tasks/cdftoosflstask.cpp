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

#include <modules/fieldlinessequence/tasks/cdftoosflstask.h>

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
    constexpr std::string_view _loggerCat = "CDFtoOSFSLtask";
    constexpr std::string_view KeyInput = "Input";
    constexpr std::string_view KeyTimeKernel = "TimeKernel";
    constexpr std::string_view KeyOutputFolder = "OutputFolder";
    constexpr std::string_view KeySeedpoints = "Seedpoints";
    constexpr std::string_view KeyTracingVar = "TracingVar";
    constexpr std::string_view KeyExtraScalarVars = "ExtraScalarVars";
    constexpr std::string_view KeyExtraMagnitudeVars = "ExtraMagnitudeVars";
} // namespace

namespace openspace {

CDFtoOSFLStask::CDFtoOSFLStask(const ghoul::Dictionary& dictionary) {
    openspace::documentation::testSpecificationAndThrow(
        documentation(),
        dictionary,
        "CDFtoOSFSLtask"
    );
    if(dictionary.hasKey(KeyInput)){
        _inputPath = dictionary.value<std::string>(KeyInput);
    }
    if (dictionary.hasKey(KeyTimeKernel)) {
        _timeKernelPath = dictionary.value<std::string>(KeyTimeKernel);
    }
    if (dictionary.hasKey(KeySeedpoints)) {
        _seedpointsPath = dictionary.value<std::string>(KeySeedpoints);
    }
    if (dictionary.hasKey(KeyOutputFolder)) {
        _outputFolder = dictionary.value<std::string>(KeyOutputFolder);
    }
    if (dictionary.hasKey(KeyTracingVar)) {
        _tracingVar = dictionary.value<std::string>(KeyTracingVar);
    }
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

std::string CDFtoOSFLStask::description() {
    return fmt::format(
        "Extract fieldline data from cdf file {} and seedpoint file {}. "
        "Write osfls file into the folder {}.",
        _inputPath, _seedpointsPath, _outputFolder
    );
}

void CDFtoOSFLStask::perform(const Task::ProgressCallback& progressCallback) {
    std::unordered_map<std::string, std::vector<glm::vec3>> seedPoints =
        fls::extractSeedPointsFromFiles(_seedpointsPath);

    SpiceManager::ref().loadKernel(_timeKernelPath);

    FieldlinesState newState;
    bool isSuccessful = fls::convertCdfToFieldlinesState(
        newState,
        _inputPath,
        seedPoints,
        _manualTimeOffset,
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

documentation::Documentation CDFtoOSFLStask::Documentation() {
    return {
        "CDFtoOSFSLtask"
    };
}

} // namespace openspace
