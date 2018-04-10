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

#include <modules/gaiamission/tasks/constructoctreetask.h>

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

    constexpr const char* _loggerCat = "ConstructOctreeTask";
} // namespace

namespace openspace {

ConstructOctreeTask::ConstructOctreeTask(const ghoul::Dictionary& dictionary) {
    
    openspace::documentation::testSpecificationAndThrow(
        documentation(),
        dictionary,
        "ConstructOctreeTask"
    );

    _inFilePath = absPath(dictionary.value<std::string>(KeyInFilePath));
    _outFilePath = absPath(dictionary.value<std::string>(KeyOutFilePath));

    _octreeManager = std::make_shared<OctreeManager>();
}

ConstructOctreeTask::~ConstructOctreeTask() {}

std::string ConstructOctreeTask::description() {
    return "Read bin file: " + _inFilePath + "\n and write octree data into: "
        + _outFilePath + "\n";
}

void ConstructOctreeTask::perform(const Task::ProgressCallback& progressCallback) {
    std::vector<float> fullData;
    int32_t nValues = 0;
    int32_t nValuesPerStar = 0;
    
    _octreeManager->initOctree();

    progressCallback(0.0f);
    LINFO("Reading data file.");

    std::ifstream inFileStream(_inFilePath, std::ifstream::binary);
    if (inFileStream.good()) {

        inFileStream.read(reinterpret_cast<char*>(&nValues), sizeof(int32_t));
        inFileStream.read(reinterpret_cast<char*>(&nValuesPerStar), sizeof(int32_t));

        fullData.resize(nValues);
        inFileStream.read(reinterpret_cast<char*>(&fullData[0]),
            nValues * sizeof(fullData[0]));

        progressCallback(0.3f);
        LINFO("Constructing Octree.");

        // TODO: Parallellize with ThreadPool!
        // Insert star into octree. We assume the data already is in correct order.
        for (size_t i = 0; i < fullData.size(); i += nValuesPerStar) {
            auto first = fullData.begin() + i;
            auto last = fullData.begin() + i + nValuesPerStar;
            std::vector<float> values(first, last);

            _octreeManager->insert(values);
        }
        inFileStream.close();
    }
    else {
        LERROR(fmt::format("Error opening file '{}' for loading preprocessed file!"
            , _inFilePath));
    }

    progressCallback(0.9f);
    LINFO("Writing octree file.");

    // TODO: Write to several files!
    std::ofstream outFileStream(_outFilePath, std::ofstream::binary);
    if (outFileStream.good()) {

        if (nValues == 0) {
            LERROR("Error writing file - No values were read from file.");
        }
        _octreeManager->writeToFile(outFileStream);

        outFileStream.close();
    }
    else {
        LERROR(fmt::format("Error opening file: {} as output data file.", _outFilePath));
    }

    progressCallback(1.0f);
}

documentation::Documentation ConstructOctreeTask::Documentation() {
    using namespace documentation;
    return {
        "ConstructOctreeTask",
        "gaiamission_constructoctreefrombin",
        {
            {
                "Type",
                new StringEqualVerifier("ConstructOctreeTask"),
                Optional::No
            },
            {
                KeyInFilePath,
                new StringVerifier,
                Optional::No,
                "The path to the BIN file with sorted raw data.",
            },
            {
                KeyOutFilePath,
                new StringVerifier,
                Optional::No,
                "The path to the file which to save octree to.",
            },
        }
    };
}

} // namespace openspace
