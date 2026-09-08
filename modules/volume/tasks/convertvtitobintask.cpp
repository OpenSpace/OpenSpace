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

#include <modules/volume/tasks/convertvtitobintask.h>

#include <modules/volume/rawvolumewriter.h>
#include <modules/volume/xmlreader.h>
#include <openspace/documentation/documentation.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/exception.h>
#include <format>

namespace
{
    constexpr std::string_view _loggerCat = "ConvertVtiToBinTask";

    // This task converts a directory of .vti (VTK Image Data) files into binary .bin
    // files compatible with the OpenSpace volume rendering pipeline.
    //
    // For each .vti file found in the input folder the raw scalar data is extracted and
    // written as a flat binary float array to the output folder. Files that already have
    // a corresponding .bin in the output folder are skipped, so the task can safely be
    // run multiple times or incrementally.
    struct [[codegen::Dictionary(ConvertVtiToBinTask)]] Parameters
    {
        // Directory containing the .vti files to convert.
        std::filesystem::path inputFolder [[codegen::directory()]];

        // Directory where the converted .bin files will be written. The directory will be
        // created if it does not already exist.
        std::filesystem::path outputFolder [[codegen::directory(),
                                             codegen::mustexist(false)]];
    };
} // namespace
#include "convertvtitobintask_codegen.cpp"

namespace openspace
{
    openspace::Documentation ConvertVtiToBinTask::Documentation() {
        return codegen::doc<Parameters>(
            "volume_task_convertvtitobin",
            Task::Documentation()
        );
    }

    ConvertVtiToBinTask::ConvertVtiToBinTask(const ghoul::Dictionary &dictionary) {
        const Parameters p = codegen::bake<Parameters>(dictionary);

        _inputFolder = p.inputFolder;
        _outputFolder = p.outputFolder;
    }

    std::string ConvertVtiToBinTask::description() {
        return std::format(
            "Convert .vti files in '{}' to binary .bin files in '{}'",
            _inputFolder, _outputFolder
        );
    }

    void ConvertVtiToBinTask::perform(const Task::ProgressCallback &progressCallback) {
        if (!std::filesystem::is_directory(_inputFolder)) {
            LERROR(std::format("Input folder '{}' does not exist", _inputFolder));
            return;
        }

        if (!std::filesystem::is_directory(_outputFolder)) {
            std::filesystem::create_directories(_outputFolder);
        }

        std::vector<std::filesystem::path> vtiFiles;
        for (const auto &entry : std::filesystem::directory_iterator(_inputFolder)) {
            if (entry.path().extension() == ".vti") {
                vtiFiles.push_back(entry.path());
            }
        }

        if (vtiFiles.empty()) {
            LWARNING(std::format("No .vti files found in '{}'", _inputFolder));
            return;
        }

        std::sort(vtiFiles.begin(), vtiFiles.end());

        // Copy the .metadata file to the output folder
        const std::filesystem::path metadataPath = _inputFolder / ".metadata";
        if (std::filesystem::is_regular_file(metadataPath)) {
            std::filesystem::copy(
                metadataPath,
                _outputFolder / ".metadata",
                std::filesystem::copy_options::skip_existing
            );
        }
        else {
            LWARNING(std::format(
                "No .metadata file found in '{}'. The renderable may not load correctly",
                _inputFolder.string()
            ));
        }

        const float total = static_cast<float>(vtiFiles.size());
        float processed = 0.f;

        for (const std::filesystem::path &vtiPath : vtiFiles) {
            const std::filesystem::path outputPath =
                _outputFolder / (vtiPath.stem().string() + ".bin");

            if (std::filesystem::exists(outputPath)) {
                LDEBUG(std::format(
                    "Skipping '{}', output file already exists", vtiPath.filename())
                );
                processed += 1.f;
                progressCallback(processed / total);
                continue;
            }

            try {
                const auto [metadata, scalars] = readVTIFile(vtiPath, 0.0);
                RawVolumeWriter<float> writer(outputPath);
                writer.write(scalars);
            }
            catch (const ghoul::RuntimeError &e) {
                LERRORC(e.component, e.message);
            }

            processed += 1.f;
            progressCallback(processed / total);
        }
    }

} // namespace openspace
