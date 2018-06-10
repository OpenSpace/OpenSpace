/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <modules/kameleonvolume/tasks/kameleonmetadatatojsontask.h>

#include <modules/kameleonvolume/kameleonvolumereader.h>
#include <openspace/documentation/verifier.h>
#include <ghoul/fmt.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/dictionaryjsonformatter.h>
#include <fstream>

namespace {
    constexpr const char* KeyInput = "Input";
    constexpr const char* KeyOutput = "Output";
} // namespace

namespace openspace::kameleonvolume {

KameleonMetadataToJsonTask::KameleonMetadataToJsonTask(
                                                      const ghoul::Dictionary& dictionary)
{
    openspace::documentation::testSpecificationAndThrow(
        documentation(),
        dictionary,
        "KameleonMetadataToJsonTask"
    );

    _inputPath = absPath(dictionary.value<std::string>(KeyInput));
    _outputPath = absPath(dictionary.value<std::string>(KeyOutput));
}

std::string KameleonMetadataToJsonTask::description() {
    return fmt::format(
        "Extract metadata from cdf file {} and write as json to {}",
        _inputPath, _outputPath
    );
}

void KameleonMetadataToJsonTask::perform(const Task::ProgressCallback& progressCallback) {
    KameleonVolumeReader reader(_inputPath);
    ghoul::Dictionary dictionary = reader.readMetaData();
    progressCallback(0.5f);

    ghoul::DictionaryJsonFormatter formatter;
    std::string json = formatter.format(dictionary);
    std::ofstream output(_outputPath);
    output << std::move(json);
    progressCallback(1.0f);
}

documentation::Documentation KameleonMetadataToJsonTask::documentation() {
    using namespace documentation;
    return {
        "KameleonMetadataToJsonTask",
        "kameleon_metadata_to_json_task",
        {
            {
                "Type",
                new StringEqualVerifier("KameleonMetadataToJsonTask"),
                Optional::No,
                "The type of this task"
            },
            {
                KeyInput,
                new StringAnnotationVerifier("A file path to a cdf file"),
                Optional::No,
                "The cdf file to extract data from"
            },
            {
                KeyOutput,
                new StringAnnotationVerifier("A valid filepath"),
                Optional::No,
                "The JSON file to export data into"
            }
        }
    };
}

} // namespace openspace::kameleonvolume
