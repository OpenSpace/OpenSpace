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

#include <modules/kameleonvolume/tasks/kameleonvolumetorawtask.h>

#include <modules/kameleonvolume/kameleonvolumereader.h>
#include <modules/volume/rawvolumewriter.h>
#include <openspace/documentation/verifier.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/format.h>
#include <ghoul/misc/dictionaryluaformatter.h>
#include <array>
#include <filesystem>
#include <optional>

namespace {
    struct [[codegen::Dictionary(KameleonVolumeToRawTask)]] Parameters {
        // The cdf file to extract data from
        std::filesystem::path input;

        // The raw volume file to export data to
        std::string rawVolumeOutput [[codegen::annotation("A valid filepath")]];

        // The Lua dictionary file to export metadata to
        std::string dictionaryOutput [[codegen::annotation("A valid filepath")]];

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

    _inputPath = p.input;
    _rawVolumeOutputPath = absPath(p.rawVolumeOutput);
    _dictionaryOutputPath = absPath(p.dictionaryOutput);
    _variable = p.variable;
    _dimensions = p.dimensions;

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
        "Extract volumetric data from CDF file '{}'. Write raw volume data into '{}' "
        "and dictionary with metadata to '{}'",
        _inputPath, _rawVolumeOutputPath, _dictionaryOutputPath
    );
}

void KameleonVolumeToRawTask::perform(const Task::ProgressCallback& progressCallback) {
    KameleonVolumeReader reader = KameleonVolumeReader(_inputPath);

    std::array<std::string, 3> variables = reader.gridVariableNames();

    if (_autoDomainBounds) {
        _lowerDomainBound = glm::vec3(
            reader.minValue(variables[0]),
            reader.minValue(variables[1]),
            reader.minValue(variables[2])
        );

        _upperDomainBound = glm::vec3(
            reader.maxValue(variables[0]),
            reader.maxValue(variables[1]),
            reader.maxValue(variables[2])
        );
    }

    std::unique_ptr<volume::RawVolume<float>> rawVolume = reader.readFloatVolume(
        _dimensions,
        _variable,
        _lowerDomainBound,
        _upperDomainBound
    );

    progressCallback(0.5f);

    volume::RawVolumeWriter<float> writer(_rawVolumeOutputPath);
    writer.write(*rawVolume);

    progressCallback(0.9f);

    ghoul::Dictionary inputMetadata = reader.readMetaData();
    ghoul::Dictionary outputMetadata;

    std::string time = reader.time();

    // Do not include time offset in time string
    if (time.back() == 'Z') {
        time.pop_back();
    }

    outputMetadata.setValue("Time", time);
    outputMetadata.setValue("Dimensions", glm::dvec3(_dimensions));
    outputMetadata.setValue("LowerDomainBound", glm::dvec3(_lowerDomainBound));
    outputMetadata.setValue("UpperDomainBound", glm::dvec3(_upperDomainBound));

    outputMetadata.setValue("MinValue", reader.minValue(_variable));
    outputMetadata.setValue("MaxValue", reader.maxValue(_variable));
    outputMetadata.setValue("VisUnit", reader.getVisUnit(_variable));

    std::string metadataString = ghoul::formatLua(outputMetadata);

    std::fstream f = std::fstream(_dictionaryOutputPath, std::ios::out);
    f << "return " << metadataString;

    progressCallback(1.f);
}

} // namespace openspace::kameleonvolume
