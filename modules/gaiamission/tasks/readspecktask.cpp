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

#include <modules/gaiamission/tasks/readspecktask.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>

#include <ghoul/misc/dictionary.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/fmt.h>

#include <fstream>

namespace {
    const char* KeyInFilePath = "InFilePath";
    const char* KeyOutFilePath = "OutFilePath";

    constexpr const char* _loggerCat = "ReadSpeckTask";
} // namespace

namespace openspace {

ReadSpeckTask::ReadSpeckTask(const ghoul::Dictionary& dictionary) {
    
    openspace::documentation::testSpecificationAndThrow(
        documentation(),
        dictionary,
        "ReadSpeckTask"
    );

    _inFilePath = absPath(dictionary.value<std::string>(KeyInFilePath));
    _outFilePath = absPath(dictionary.value<std::string>(KeyOutFilePath));
}

ReadSpeckTask::~ReadSpeckTask() {}

std::string ReadSpeckTask::description() {
    return "Read speck file: " + _inFilePath + "\n and write raw star data into: "
        + _outFilePath + "\n.";
}

void ReadSpeckTask::perform(const Task::ProgressCallback& progressCallback) {
    progressCallback(0.0f);

    int32_t nRenderValues = 0;

    FitsFileReader fileReader(false);
    std::vector<float> fullData = fileReader.readSpeckFile(_inFilePath, nRenderValues);

    progressCallback(0.9f);

    std::ofstream fileStream(_outFilePath, std::ofstream::binary);
    if (fileStream.good()) {

        int32_t nValues = static_cast<int32_t>(fullData.size());
        LINFO("nValues: " + std::to_string(nValues));

        if (nValues == 0) {
            LERROR("Error writing file - No values were read from file.");
        }
        fileStream.write(reinterpret_cast<const char*>(&nValues), sizeof(int32_t));
        fileStream.write(reinterpret_cast<const char*>(&nRenderValues), sizeof(int32_t));

        size_t nBytes = nValues * sizeof(fullData[0]);
        fileStream.write(reinterpret_cast<const char*>(fullData.data()), nBytes);

        fileStream.close();
    }
    else {
        LERROR(fmt::format("Error opening file: {} as output data file.", _outFilePath));
    }

    progressCallback(1.0f);
}

documentation::Documentation ReadSpeckTask::Documentation() {
    using namespace documentation;
    return {
        "ReadSpeckTask",
        "gaiamission_speckfiletorawdata",
        {
            {
                "Type",
                new StringEqualVerifier("ReadSpeckTask"),
                Optional::No
            },
            {
                KeyInFilePath,
                new StringVerifier,
                Optional::No,
                "The path to the SPECK file that are to be read.",
            },
            {
                KeyOutFilePath,
                new StringVerifier,
                Optional::No,
                "The path to the file to export raw VBO data to.",
            },
        }
    };
}

} // namespace openspace
