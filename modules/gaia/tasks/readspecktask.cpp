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

#include <modules/gaia/tasks/readspecktask.h>

#include <modules/fitsfilereader/include/fitsfilereader.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/format.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>
#include <fstream>

namespace {
    constexpr std::string_view _loggerCat = "ReadSpeckTask";

    struct [[codegen::Dictionary(ReadSpeckTask)]] Parameters {
        // The path to the SPECK file that are to be read
        std::string inFilePath;

        // The path to the file to export raw VBO data to
        std::string outFilePath;
    };
#include "readspecktask_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation ReadSpeckTask::Documentation() {
    return codegen::doc<Parameters>("gaiamission_speckfiletorawdata");
}

ReadSpeckTask::ReadSpeckTask(const ghoul::Dictionary& dictionary) {
    const Parameters p = codegen::bake<Parameters>(dictionary);
    _inFilePath = absPath(p.inFilePath);
    _outFilePath = absPath(p.outFilePath);
}

std::string ReadSpeckTask::description() {
    return std::format(
        "Read speck file '{}' and write raw star data into '{}'",
        _inFilePath, _outFilePath
    );
}

void ReadSpeckTask::perform(const Task::ProgressCallback& onProgress) {
    onProgress(0.f);

    int32_t nRenderValues = 0;

    FitsFileReader fileReader(false);
    std::vector<float> fullData = fileReader.readSpeckFile(
        _inFilePath,
        nRenderValues
    );

    onProgress(0.9f);

    std::ofstream fileStream(_outFilePath, std::ofstream::binary);
    if (fileStream.good()) {
        int32_t nValues = static_cast<int32_t>(fullData.size());
        LINFO("nValues: " + std::to_string(nValues));

        if (nValues == 0) {
            LERROR("Error writing file - No values were read from file");
        }
        fileStream.write(reinterpret_cast<const char*>(&nValues), sizeof(int32_t));
        fileStream.write(reinterpret_cast<const char*>(&nRenderValues), sizeof(int32_t));

        const size_t nBytes = nValues * sizeof(fullData[0]);
        fileStream.write(reinterpret_cast<const char*>(fullData.data()), nBytes);

        fileStream.close();
    }
    else {
        LERROR(std::format("Error opening file '{}' as output data file", _outFilePath));
    }

    onProgress(1.f);
}

} // namespace openspace
