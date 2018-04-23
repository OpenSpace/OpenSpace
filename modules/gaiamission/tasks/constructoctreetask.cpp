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
#include <ghoul/filesystem/directory.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/fmt.h>

#include <fstream>

namespace {
    const char* KeyInFolderPath = "InFolderPath";
    const char* KeyOutFolderPath = "OutFolderPath";

    constexpr const char* _loggerCat = "ConstructOctreeTask";
} // namespace

namespace openspace {

ConstructOctreeTask::ConstructOctreeTask(const ghoul::Dictionary& dictionary) {
    
    openspace::documentation::testSpecificationAndThrow(
        documentation(),
        dictionary,
        "ConstructOctreeTask"
    );

    _inFolderPath = absPath(dictionary.value<std::string>(KeyInFolderPath));
    _outFolderPath = absPath(dictionary.value<std::string>(KeyOutFolderPath));

    //_octreeManager = std::make_shared<OctreeManager>();
    _indexOctreeManager = std::make_unique<OctreeManager>();
}

ConstructOctreeTask::~ConstructOctreeTask() {}

std::string ConstructOctreeTask::description() {
    return "Read bin files in folder: " + _inFolderPath + "\n and write octree data files into: "
        + _outFolderPath + "\n";
}

void ConstructOctreeTask::perform(const Task::ProgressCallback& progressCallback) {
    std::vector<float> fullData;
    int32_t nValues = 0;
    int32_t nValuesPerStar = 0;

    ghoul::filesystem::Directory currentDir(_inFolderPath);
    std::vector<std::string> allInputFiles = currentDir.readFiles();
    std::vector<float> starData(8, 0.f);
    
    _indexOctreeManager->initOctree();

    progressCallback(0.0f);

    float processOneFile = 1.f / allInputFiles.size();

    // TODO: Parallelize!
    for (size_t idx = 0; idx < allInputFiles.size(); ++idx) {
        
        std::string inFilePath = allInputFiles[idx];

        LINFO("Reading data file: " + inFilePath);

        std::ifstream inFileStream(inFilePath, std::ifstream::binary);
        if (inFileStream.good()) {

            inFileStream.read(reinterpret_cast<char*>(&nValuesPerStar), sizeof(int32_t));
            nValuesPerStar = 8;

            while (inFileStream.read(reinterpret_cast<char*>(&starData[0]), 
                nValuesPerStar * sizeof(starData[0]))) {

                // TODO: Filter by parameters!

                _indexOctreeManager->insert(starData);
                nValues += nValuesPerStar;
            }

            /*inFileStream.read(reinterpret_cast<char*>(&nValues), sizeof(int32_t));
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
            }*/

            inFileStream.close();
        }
        else {
            LERROR(fmt::format("Error opening file '{}' for loading preprocessed file!"
                , inFilePath));
        }

        progressCallback((idx+1) * processOneFile / 2.f);
        LINFO(fmt::format("Writing {} values to octree files!", nValues));
        LINFO("Number of leaf nodes: " + std::to_string(_indexOctreeManager->numLeafNodes()) + 
            "\n Number of inner nodes: " + std::to_string(_indexOctreeManager->numInnerNodes()) + 
            "\n Total depth of tree: " + std::to_string(_indexOctreeManager->totalDepth()));

        // Write to several files! TODO: What happens if we don't use 8 files?
        _indexOctreeManager->writeToMultipleFiles(_outFolderPath, idx);

        // Remove all data from Octree structure.
        LINFO("Clear all data from Octree!");
        _indexOctreeManager->clearAllData(static_cast<int>(idx));

        progressCallback((idx + 1) * processOneFile);
    }

    LINFO("Writing index file!");

    // Write index file of Octree structure.
    std::string indexFileOutPath = _outFolderPath + "index.bin";
    std::ofstream outFileStream(indexFileOutPath, std::ofstream::binary);
    if (outFileStream.good()) {

        _indexOctreeManager->writeStructureToFile(outFileStream);

        outFileStream.close();
    }
    else {
        LERROR(fmt::format("Error opening file: {} as index output file.", indexFileOutPath));
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
                KeyInFolderPath,
                new StringVerifier,
                Optional::No,
                "The path to the folder with BIN files with sorted raw data.",
            },
            {
                KeyOutFolderPath,
                new StringVerifier,
                Optional::No,
                "The path to the folder which to save octree files to.",
            },
        }
    };
}

} // namespace openspace
