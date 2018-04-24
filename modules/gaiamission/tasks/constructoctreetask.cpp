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
    const char* KeyInFileOrFolderPath = "InFileOrFolderPath";
    const char* KeyOutFileOrFolderPath = "OutFileOrFolderPath";
    const char* KeySingleFileInput = "SingleFileInput";

    constexpr const char* _loggerCat = "ConstructOctreeTask";
} // namespace

namespace openspace {

ConstructOctreeTask::ConstructOctreeTask(const ghoul::Dictionary& dictionary)
    : _inFileOrFolderPath("")
    , _outFileOrFolderPath("")
    , _singleFileInput(false)
{
    
    openspace::documentation::testSpecificationAndThrow(
        documentation(),
        dictionary,
        "ConstructOctreeTask"
    );

    _inFileOrFolderPath = absPath(dictionary.value<std::string>(KeyInFileOrFolderPath));
    _outFileOrFolderPath = absPath(dictionary.value<std::string>(KeyOutFileOrFolderPath));

    if (dictionary.hasKey(KeySingleFileInput)) {
        _singleFileInput = dictionary.value<bool>(KeySingleFileInput);
    }

    _octreeManager = std::make_shared<OctreeManager>();
    _indexOctreeManager = std::make_unique<OctreeManager>();
}

ConstructOctreeTask::~ConstructOctreeTask() {}

std::string ConstructOctreeTask::description() {
    return "Read bin file (or files in folder): " + _inFileOrFolderPath + "\n "
        "and write octree data file (or files) into: " + _outFileOrFolderPath + "\n";
}

void ConstructOctreeTask::perform(const Task::ProgressCallback& progressCallback) {

    progressCallback(0.0f);

    if (_singleFileInput) {
        constructOctreeFromSingleFile(progressCallback);
    } 
    else {
        constructOctreeFromFolder(progressCallback);
    }

    progressCallback(1.0f);
}

void ConstructOctreeTask::constructOctreeFromSingleFile(const Task::ProgressCallback& progressCallback) {
    std::vector<float> fullData;
    int32_t nValues = 0;
    int32_t nValuesPerStar = 0;

    _octreeManager->initOctree();

    LINFO("Reading data file: " + _inFileOrFolderPath);

    std::ifstream inFileStream(_inFileOrFolderPath, std::ifstream::binary);
    if (inFileStream.good()) {

        inFileStream.read(reinterpret_cast<char*>(&nValues), sizeof(int32_t));
        inFileStream.read(reinterpret_cast<char*>(&nValuesPerStar), sizeof(int32_t));

        fullData.resize(nValues);
        inFileStream.read(reinterpret_cast<char*>(&fullData[0]), nValues * sizeof(fullData[0]));

        progressCallback(0.3f);
        LINFO("Constructing Octree.");

        // Insert star into octree. We assume the data already is in correct order.
        for (size_t i = 0; i < fullData.size(); i += nValuesPerStar) {
            auto first = fullData.begin() + i;
            auto last = fullData.begin() + i + nValuesPerStar;
            std::vector<float> filterValues(first, last);
            std::vector<float> renderValues(first, first + RENDER_VALUES);

            // TODO: Filter data by parameters!
            bool passFilters = true;

            // If all filters passed then insert render values into Octree.
            if (passFilters) {
                _octreeManager->insert(renderValues);
            }
            
        }
        inFileStream.close();
    }
    else {
        LERROR(fmt::format("Error opening file '{}' for loading preprocessed file!"
            , _inFileOrFolderPath));
    }

    LINFO("Writing octree to: " + _outFileOrFolderPath);
    std::ofstream outFileStream(_outFileOrFolderPath, std::ofstream::binary);
    if (outFileStream.good()) {

        if (nValues == 0) {
            LERROR("Error writing file - No values were read from file.");
        }
        _octreeManager->writeToFile(outFileStream, true);

        outFileStream.close();
    }
    else {
        LERROR(fmt::format("Error opening file: {} as output data file.", _outFileOrFolderPath));
    }
}

void ConstructOctreeTask::constructOctreeFromFolder(const Task::ProgressCallback& progressCallback) {
    
    int32_t nStars = 0;
    int32_t nValuesPerStar = 0;

    ghoul::filesystem::Directory currentDir(_inFileOrFolderPath);
    std::vector<std::string> allInputFiles = currentDir.readFiles();
    std::vector<float> filterValues;

    _indexOctreeManager->initOctree();

    float processOneFile = 1.f / allInputFiles.size();

    // TODO: Parallelize!
    for (size_t idx = 0; idx < allInputFiles.size(); ++idx) {

        std::string inFilePath = allInputFiles[idx];
        int nStarsInfile = 0;

        LINFO("Reading data file: " + inFilePath);

        std::ifstream inFileStream(inFilePath, std::ifstream::binary);
        if (inFileStream.good()) {

            inFileStream.read(reinterpret_cast<char*>(&nValuesPerStar), sizeof(int32_t));
            filterValues.resize(nValuesPerStar, 0.f);

            while (inFileStream.read(reinterpret_cast<char*>(&filterValues[0]),
                nValuesPerStar * sizeof(filterValues[0]))) {

                // TODO: Filter data by parameters!
                bool passFilters = true;

                // If all filters passed then insert render values into Octree.
                if (passFilters) {
                    std::vector<float> renderValues(filterValues.begin(),
                        filterValues.begin() + RENDER_VALUES);

                    _indexOctreeManager->insert(renderValues);
                    nStarsInfile++;
                }
            }

            inFileStream.close();
        }
        else {
            LERROR(fmt::format("Error opening file '{}' for loading preprocessed file!"
                , inFilePath));
        }

        progressCallback((idx + 1) * processOneFile / 2.f);
        nStars += nStarsInfile;

        LINFO(fmt::format("Writing {} stars to octree files!", nStarsInfile));
        LINFO("Number of leaf nodes: " + std::to_string(_indexOctreeManager->numLeafNodes()) +
            "\n Number of inner nodes: " + std::to_string(_indexOctreeManager->numInnerNodes()) +
            "\n Total depth of tree: " + std::to_string(_indexOctreeManager->totalDepth()));

        // Write to several files! TODO: What happens if we don't use 8 files?
        _indexOctreeManager->writeToMultipleFiles(_outFileOrFolderPath, idx);

        // Remove all data from Octree structure.
        LINFO("Clear all data from Octree!");
        _indexOctreeManager->clearAllData(static_cast<int>(idx));

        progressCallback((idx + 1) * processOneFile);
    }

    LINFO("A total of " + std::to_string(nStars) + " stars where read from files and distributed into "
        + std::to_string(_indexOctreeManager->totalNodes() + " total nodes!"));

    // Write index file of Octree structure.
    std::string indexFileOutPath = _outFileOrFolderPath + "index.bin";
    std::ofstream outFileStream(indexFileOutPath, std::ofstream::binary);
    if (outFileStream.good()) {

        LINFO("Writing index file!");
        _indexOctreeManager->writeToFile(outFileStream, false);

        outFileStream.close();
    }
    else {
        LERROR(fmt::format("Error opening file: {} as index output file.", indexFileOutPath));
    }
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
                KeyInFileOrFolderPath,
                new StringVerifier,
                Optional::No,
                "If SingleFileInput is set to true then this specifies the path to a single BIN file "
                "containing a full dataset. Otherwise this specifies the path to a folder with multiple "
                "BIN files containing subsets of sorted star data.",
            },
            {
                KeyOutFileOrFolderPath,
                new StringVerifier,
                Optional::No,
                "If SingleFileInput is set to true then this specifies the output file name (including "
                "full path). Otherwise this specifies the path to the folder which to save all files.",
            },
            {
                KeySingleFileInput,
                new BoolVerifier,
                Optional::Yes,
                "If true then task will read from a single file and output a single binary file "
                "with the full Octree. If false then task will read all files in specified folder and "
                "output multiple files for the Octree."
            },
        }
    };
}

} // namespace openspace
