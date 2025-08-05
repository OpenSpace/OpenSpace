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

#include <modules/kameleonvolume/tasks/kameleonvolumetorawtask.h>

#include <modules/kameleonvolume/kameleonvolumereader.h>
#include <modules/volume/rawvolumewriter.h>
#include <openspace/documentation/verifier.h>
#include <ghoul/filesystem/filesystem.h>
#include <openspace/util/time.h>
#include <ghoul/format.h>
#include <ghoul/misc/dictionaryluaformatter.h>
#include <array>
#include <filesystem>
#include <ccmc/Kameleon.h>
#include <optional>
#include <openspace/util/spicemanager.h>
#include <modules/kameleon/include/kameleonwrapper.h>
#include <modules/kameleon/include/kameleonhelper.h>
#include <iostream>

namespace {
    struct [[codegen::Dictionary(KameleonVolumeToRawTask)]] Parameters {
        // The folder containing CDF files
        std::filesystem::path inputDirectory [[codegen::directory()]];

        // The base path (folder) for raw volume output files
        std::string rawVolumeOutputBase [[codegen::annotation("A valid folder path")]];

        // The base path (folder) for Lua dictionary output files
        std::string dictionaryOutputBase [[codegen::annotation("A valid folder path")]];

        // The variable name to read from the kameleon dataset
        std::string variable [[codegen::annotation("A valid kameleon variable")]];

        // A vector representing the number of cells in each dimension
        glm::ivec3 dimensions;

        // A vector representing the lower bound of the domain, in the native kameleon
        // grid units
        std::optional<glm::vec3> lowerDomainBound;

        // A vector representing the lower bound of the domain, in the native kameleon
        // grid units
        std::optional<glm::vec3> upperDomainBound;

        // Choose to decrese cadence and only use every nth time step/ input file
        std::optional<int> nthTimeStep;

        // The unit of the data
        std::optional<std::string> visUnit
            [[codegen::annotation("A valid kameleon unit")]];
    };
#include "kameleonvolumetorawtask_codegen.cpp"
} // namespace

namespace openspace::kameleonvolume {

documentation::Documentation KameleonVolumeToRawTask::documentation() {
    return codegen::doc<Parameters>("kameleon_metadata_to_json_task");
}

KameleonVolumeToRawTask::KameleonVolumeToRawTask(const ghoul::Dictionary& dictionary) {
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _inputDirectory = p.inputDirectory;
    _rawVolumeOutputBasePath = absPath(p.rawVolumeOutputBase);
    _dictionaryOutputBasePath = absPath(p.dictionaryOutputBase);
    _variable = p.variable;
    _dimensions = p.dimensions;
    _nthTimeStep = p.nthTimeStep.value_or(_nthTimeStep);

    if (p.lowerDomainBound.has_value()) {
        _lowerDomainBound = *p.lowerDomainBound;
    }
    else {
        _autoDomainBounds = true;
    }

    if (p.upperDomainBound.has_value()) {
        _upperDomainBound = *p.upperDomainBound;
    }
    else {
        _autoDomainBounds = true;
    }
}

std::string KameleonVolumeToRawTask::description() {
    return std::format(
        "Extract volumetric data from all CDF files in '{}'. Write raw volume data into '{}' "
        "and dictionaries with metadata to '{}'",
        _inputDirectory, _rawVolumeOutputBasePath, _dictionaryOutputBasePath
    );
}

void KameleonVolumeToRawTask::perform(const Task::ProgressCallback& progressCallback) {
    namespace fs = std::filesystem;

    size_t totalFiles = std::distance(
        fs::directory_iterator(_inputDirectory), fs::directory_iterator{}
    );
    size_t count = 0;

    // Status bar configuration
    const int barWidth = 50;

    // ANSI escape code to clear the entire line
    const std::string clearLine = "\033[2K\r";

    for (const fs::directory_entry& entry : fs::directory_iterator(_inputDirectory)) {
        if (!entry.is_regular_file() || entry.path().extension() != ".cdf") {
            continue;
        }
        if (count % _nthTimeStep == 0) {
            const fs::path& inputPath = entry.path();
            std::string baseName = inputPath.stem().string();

            fs::path rawOutput = _rawVolumeOutputBasePath / (baseName + ".rawvolume");
            fs::path dictOutput = _dictionaryOutputBasePath / (baseName + ".dictionary");

            KameleonVolumeReader reader(inputPath);
            std::array<std::string, 3> variables = reader.gridVariableNames();

            glm::vec3 lowerBound, upperBound;
            if (_autoDomainBounds) {
                lowerBound = glm::vec3(
                    reader.minValue(variables[0]),
                    reader.minValue(variables[1]),
                    reader.minValue(variables[2])
                );
                upperBound = glm::vec3(
                    reader.maxValue(variables[0]),
                    reader.maxValue(variables[1]),
                    reader.maxValue(variables[2])
                );
            }
            else {
                lowerBound = _lowerDomainBound;
                upperBound = _upperDomainBound;
            }

            std::unique_ptr<volume::RawVolume<float>> rawVolume = reader.readFloatVolume(
                _dimensions, _variable, lowerBound, upperBound
            );

            volume::RawVolumeWriter<float> writer(rawOutput);
            writer.write(*rawVolume);

            ghoul::Dictionary inputMetadata = reader.readMetaData();
            ghoul::Dictionary outputMetadata;

            std::string time = reader.time();

            std::unique_ptr<ccmc::Kameleon> kameleon = kameleonHelper::createKameleonObject(inputPath.string());
            double cdfDoubleTime = kameleonHelper::getTime(kameleon.get(), 0);

            std::string cdfStringTime = SpiceManager::ref().dateFromEphemerisTime(
                cdfDoubleTime, "YYYY-MM-DDTHR:MN:SC::RND"
            );

            if (time.back() == 'Z') {
                time.pop_back();
            }

            outputMetadata.setValue("Time", cdfStringTime);
            outputMetadata.setValue("Dimensions", glm::dvec3(_dimensions));
            outputMetadata.setValue("LowerDomainBound", glm::dvec3(lowerBound));
            outputMetadata.setValue("UpperDomainBound", glm::dvec3(upperBound));
            outputMetadata.setValue("MinValue", reader.minValue(_variable));
            outputMetadata.setValue("MaxValue", reader.maxValue(_variable));
            outputMetadata.setValue("VisUnit", reader.getVisUnit(_variable));

            std::string metadataString = ghoul::formatLua(outputMetadata);
            std::ofstream dictFile(dictOutput);
            dictFile << "return " << metadataString;
        }

        count++;
        float progress = static_cast<float>(count) / static_cast<float>(totalFiles);
        progressCallback(progress);

        // Update the console status bar - clear the line first
        std::cout << clearLine;

        // Draw the new progress bar
        std::cout << "[";
        int pos = static_cast<int>(barWidth * progress);
        for (int i = 0; i < barWidth; ++i) {
            if (i < pos) std::cout << "=";
            else if (i == pos) std::cout << ">";
            else std::cout << " ";
        }
        std::cout << "] " << int(progress * 100.0) << "% (" << count << "/" << totalFiles << ")" << std::flush;
    }

    std::cout << std::endl;


    progressCallback(1.f);
}

} // namespace openspace::kameleonvolume
