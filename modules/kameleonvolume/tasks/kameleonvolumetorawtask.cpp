/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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
#include <modules/volume/rawvolume.h>
#include <modules/volume/rawvolumewriter.h>

#include <openspace/documentation/verifier.h>

#include <ghoul/misc/dictionaryjsonformatter.h>
#include <ghoul/filesystem/filesystem.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionaryluaformatter.h>

#include <fstream>

namespace {
    const char* KeyInput = "Input";
    const char* KeyRawVolumeOutput = "RawVolumeOutput";
    const char* KeyDictionaryOutput = "DictionaryOutput";
    const char* KeyDimensions = "Dimensions";
    const char* KeyVariable = "Variable";
    const char* KeyTime = "Time";
    const char* KeyLowerDomainBound = "LowerDomainBound";
    const char* KeyUpperDomainBound = "UpperDomainBound";

    const char* KeyMinValue = "MinValue";
    const char* KeyMaxValue = "MaxValue";

    const char* _loggerCat = "KameleonVolumeToRawTask";
} // namespace

namespace openspace {
namespace kameleonvolume {

KameleonVolumeToRawTask::KameleonVolumeToRawTask(const ghoul::Dictionary& dictionary)
    : _autoDomainBounds(false)
{
    openspace::documentation::testSpecificationAndThrow(
        documentation(),
        dictionary,
        "KameleonVolumeToRawTask"
    );

    _inputPath = absPath(dictionary.value<std::string>(KeyInput));
    _rawVolumeOutputPath = absPath(dictionary.value<std::string>(KeyRawVolumeOutput));
    _dictionaryOutputPath = absPath(dictionary.value<std::string>(KeyDictionaryOutput));
    _variable = dictionary.value<std::string>(KeyVariable);
    _dimensions = glm::uvec3(dictionary.value<glm::vec3>(KeyDimensions));
    
    if (!dictionary.getValue<glm::vec3>(KeyLowerDomainBound, _lowerDomainBound)) {
        _autoDomainBounds = true;
    }
    if (!dictionary.getValue<glm::vec3>(KeyUpperDomainBound, _upperDomainBound)) {
        _autoDomainBounds = true;
    }
}

std::string KameleonVolumeToRawTask::description() {
    return "Extract volumetric data from cdf-file " + _inputPath + "." +
        "Write raw volume data into " + _rawVolumeOutputPath +
        " and dictionary with metadata to " + _dictionaryOutputPath;
}

void KameleonVolumeToRawTask::perform(const Task::ProgressCallback& progressCallback) {
    KameleonVolumeReader reader(_inputPath);

    std::vector<std::string> variables = reader.gridVariableNames();

    if (variables.size() == 3 && _autoDomainBounds) {
        _lowerDomainBound = glm::vec3(
            reader.minValue(variables[0]),
            reader.minValue(variables[1]),
            reader.minValue(variables[2]));

        _upperDomainBound = glm::vec3(
            reader.maxValue(variables[0]),
            reader.maxValue(variables[1]),
            reader.maxValue(variables[2]));
    }

    
    std::unique_ptr<volume::RawVolume<float>> rawVolume = reader.readFloatVolume(
        _dimensions,
        _variable,
        _lowerDomainBound,
        _upperDomainBound);

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

    outputMetadata.setValue<std::string>(KeyTime, time);
    outputMetadata.setValue<glm::vec3>(KeyDimensions, _dimensions);
    outputMetadata.setValue<glm::vec3>(KeyLowerDomainBound, _lowerDomainBound);
    outputMetadata.setValue<glm::vec3>(KeyUpperDomainBound, _upperDomainBound);
    outputMetadata.setValue<float>(KeyMinValue, reader.minValue(_variable));
    outputMetadata.setValue<float>(KeyMaxValue, reader.maxValue(_variable));

    ghoul::DictionaryLuaFormatter formatter;
    std::string metadataString = formatter.format(outputMetadata);

    std::fstream f(_dictionaryOutputPath, std::ios::out);
    f << "return " << metadataString;
    f.close();

    progressCallback(1.0f);
}

documentation::Documentation KameleonVolumeToRawTask::documentation() {
    using namespace documentation;
    return {
        "KameleonVolumeToRawTask",
        "kameleon_metadata_to_json_task",
        {
            {
                "Type",
                new StringEqualVerifier("KameleonVolumeToRawTask"),
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
                KeyRawVolumeOutput,
                new StringAnnotationVerifier("A valid filepath"),
                Optional::No,
                "The raw volume file to export data to",
            },
            {
                KeyDictionaryOutput,
                new StringAnnotationVerifier("A valid filepath"),
                Optional::No,
                "The lua dictionary file to export metadata to",
            },
            {
                KeyVariable,
                new StringAnnotationVerifier("A valid kameleon variable"),
                Optional::No,
                "The variable name to read from the kameleon dataset",
            },
            {
                KeyDimensions,
                new DoubleVector3Verifier,
                Optional::No,
                "A vector representing the number of cells in each dimension",
            },
            {
                KeyLowerDomainBound,
                new DoubleVector3Verifier,
                Optional::Yes,
                "A vector representing the lower bound of the domain, "
                "in the native kameleon grid units",
            },
            {
                KeyUpperDomainBound,
                new DoubleVector3Verifier,
                Optional::Yes,
                "A vector representing the lower bound of the domain, "
                "in the native kameleon grid units",
            }
        }
    };
}

} // namespace kameleonvolume
} // namespace openspace
