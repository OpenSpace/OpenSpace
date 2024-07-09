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

#include <modules/volume/tasks/generaterawvolumefromfiletask.h>

#include <modules/volume/rawvolume.h>
#include <modules/volume/rawvolumemetadata.h>
#include <modules/volume/rawvolumewriter.h>
#include <openspace/data/csvloader.h>
#include <openspace/documentation/verifier.h>
#include <openspace/util/time.h>
#include <openspace/util/spicemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/format.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionaryluaformatter.h>
#include <filesystem>
#include <fstream>

namespace {
    constexpr std::string_view _loggerCat = "GenerateRawVolumeFromFileTask";

    struct [[codegen::Dictionary(GenerateRawVolumeFromFileTask)]] Parameters {
        // The volume file to import data from in csv format
        std::string dataInputPath;

        // The raw volume file to export data to
        std::string rawVolumeOutput;

        // The lua dictionary file to export metadata to
        std::string dictionaryOutput;

        // The data column value to use for the volume transfer function, must be one
        // of the names in the CSV header
        std::string dataValue [[codegen::notempty()]];

        // The timestamp that is written to the metadata of this volume
        std::string time [[codegen::notempty()]];

        // A vector representing the number of cells in each dimension
        glm::ivec3 dimensions;
    };
#include "generaterawvolumefromfiletask_codegen.cpp"
} // namespace

namespace openspace::volume {

documentation::Documentation GenerateRawVolumeFromFileTask::Documentation() {
    return codegen::doc<Parameters>("generate_raw_volume_task");
}

GenerateRawVolumeFromFileTask::GenerateRawVolumeFromFileTask(const ghoul::Dictionary& dictionary) {
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _inputFilePath = absPath(p.dataInputPath);
    _rawVolumeOutputPath = absPath(p.rawVolumeOutput);
    _dictionaryOutputPath = absPath(p.dictionaryOutput);
    _dataValue = p.dataValue;
    _dimensions = p.dimensions;
    _time = p.time;
    _lowerDomainBound = glm::vec3(std::numeric_limits<float>::max());
    _upperDomainBound = glm::vec3(std::numeric_limits<float>::lowest());
}

std::string GenerateRawVolumeFromFileTask::description() {
    return std::format(
        "Generate a raw volume with dimensions: ({}, {}, {}). "
        "Write raw volume data into '{}' and dictionary with metadata to '{}'",
        _dimensions.x, _dimensions.y, _dimensions.z,
        _rawVolumeOutputPath, _dictionaryOutputPath
    );
}

void GenerateRawVolumeFromFileTask::perform(const Task::ProgressCallback& progressCallback) {

    dataloader::Dataset data = dataloader::csv::loadCsvFile(_inputFilePath);
    progressCallback(0.3f);

    if (data.isEmpty()) {
        LERROR(std::format(
            "Error loading CSV data in file '{}'", _inputFilePath.string()
        ));
        return;
    }

    // Get min/max x, y, z position - ie. domain bounds of the volume
    for (const dataloader::Dataset::Entry& p : data.entries)
    {
        _lowerDomainBound = glm::vec3(
            std::min(_lowerDomainBound.x, p.position.x),
            std::min(_lowerDomainBound.y, p.position.y),
            std::min(_lowerDomainBound.z, p.position.z)
        );
        _upperDomainBound = glm::vec3(
            std::max(_upperDomainBound.x, p.position.x),
            std::max(_upperDomainBound.y, p.position.y),
            std::max(_upperDomainBound.z, p.position.z)
        );
    }
    progressCallback(0.4f);

    volume::RawVolume<float> rawVolume(_dimensions);

    float minVal = std::numeric_limits<float>::max();
    float maxVal = std::numeric_limits<float>::lowest();

    auto dataIndex = std::find_if(
        data.variables.begin(),
        data.variables.end(),
        [this](const dataloader::Dataset::Variable& var) {
            return var.name == _dataValue;
        }
    );

    if (dataIndex == data.variables.end()) {
        LERROR(std::format(
            "Could not find specified variable '{}' in dataset", _dataValue
        ));
        return;
    }
    progressCallback(0.5f);

    // Write data into volume data structure
    int k = 0;
    for (auto& entry : data.entries) {
        // Get the closest i, j , k voxel that should contain this data
        glm::vec3 normalizedPos{ (entry.position - _lowerDomainBound) /
            (_upperDomainBound - _lowerDomainBound) };

        glm::uvec3 cell{
            glm::min(
                static_cast<glm::uvec3>(glm::floor(
                    normalizedPos * static_cast<glm::vec3>(_dimensions)
                )),
                _dimensions - 1u
            )
        };

        const float value = entry.data[dataIndex->index];
        minVal = std::min(minVal, value);
        maxVal = std::max(maxVal, value);

        size_t index = rawVolume.coordsToIndex(cell);
        rawVolume.set(index, value);
        k++;
    }
    progressCallback(0.75f);

    const std::filesystem::path directory = _rawVolumeOutputPath.parent_path();
    if (!std::filesystem::is_directory(directory)) {
        std::filesystem::create_directories(directory);
    }

    volume::RawVolumeWriter<float> writer(_rawVolumeOutputPath);
    writer.write(rawVolume);
    progressCallback(0.9f);

    RawVolumeMetadata metadata;
    metadata.time = Time::convertTime(_time);
    metadata.dimensions = _dimensions;
    metadata.hasDomainUnit = true;
    metadata.domainUnit = "m";
    metadata.hasValueUnit = true;
    metadata.valueUnit = "K";
    metadata.gridType = VolumeGridType::Cartesian;
    metadata.hasDomainBounds = true;
    metadata.lowerDomainBound = _lowerDomainBound;
    metadata.upperDomainBound = _upperDomainBound;
    metadata.hasValueRange = true;
    metadata.minValue = minVal;
    metadata.maxValue = maxVal;

    const ghoul::Dictionary outputDictionary = metadata.dictionary();
    const std::string metadataString = ghoul::formatLua(outputDictionary);

    std::fstream f = std::fstream(_dictionaryOutputPath, std::ios::out);
    f << "return " << metadataString;

    progressCallback(1.f);
}

} // namespace openspace::volume
