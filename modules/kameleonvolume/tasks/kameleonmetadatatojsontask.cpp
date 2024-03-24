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

#include <modules/kameleonvolume/tasks/kameleonmetadatatojsontask.h>

#include <modules/kameleonvolume/kameleonvolumereader.h>
#include <openspace/documentation/verifier.h>
#include <ghoul/fmt.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/dictionaryjsonformatter.h>
#include <filesystem>
#include <fstream>

namespace {
    struct [[codegen::Dictionary(KameleonMetadataToJsonTask)]] Parameters {
        // The CDF file to extract data from
        std::filesystem::path input;

        // The JSON file to export data into
        std::string output [[codegen::annotation("A valid filepath")]];
    };
#include "kameleonmetadatatojsontask_codegen.cpp"
} // namespace

namespace openspace::kameleonvolume {

documentation::Documentation KameleonMetadataToJsonTask::documentation() {
    return codegen::doc<Parameters>("kameleon_metadata_to_json_task");
}

KameleonMetadataToJsonTask::KameleonMetadataToJsonTask(
                                                      const ghoul::Dictionary& dictionary)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);
    _inputPath = absPath(p.input.string());
    _outputPath = absPath(p.output);
}

std::string KameleonMetadataToJsonTask::description() {
    return std::format(
        "Extract metadata from CDF file '{}' and write as JSON to '{}'",
        _inputPath, _outputPath
    );
}

void KameleonMetadataToJsonTask::perform(const Task::ProgressCallback& progressCallback) {
    KameleonVolumeReader reader(_inputPath.string());
    ghoul::Dictionary dictionary = reader.readMetaData();
    progressCallback(0.5f);

    std::string json = ghoul::formatJson(dictionary);
    std::ofstream output(_outputPath);
    output << std::move(json);
    progressCallback(1.f);
}

} // namespace openspace::kameleonvolume
