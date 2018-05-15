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
    const char* KeyMaxDist = "MaxDist";
    const char* KeyMaxStarsPerNode = "MaxStarsPerNode";
    const char* KeySingleFileInput = "SingleFileInput";

    constexpr const char* _loggerCat = "ConstructOctreeTask";
} // namespace

namespace openspace {

ConstructOctreeTask::ConstructOctreeTask(const ghoul::Dictionary& dictionary)
    : _inFileOrFolderPath("")
    , _outFileOrFolderPath("")
    , _singleFileInput(false)
    , _maxDist(0)
    , _maxStarsPerNode(0)
{
    
    openspace::documentation::testSpecificationAndThrow(
        documentation(),
        dictionary,
        "ConstructOctreeTask"
    );

    _inFileOrFolderPath = absPath(dictionary.value<std::string>(KeyInFileOrFolderPath));
    _outFileOrFolderPath = absPath(dictionary.value<std::string>(KeyOutFileOrFolderPath));

    if (dictionary.hasKey(KeyMaxDist)) {
        _maxDist = static_cast<int>(dictionary.value<double>(KeyMaxDist));
    }

    if (dictionary.hasKey(KeyMaxStarsPerNode)) {
        _maxStarsPerNode = static_cast<int>(dictionary.value<double>(KeyMaxStarsPerNode));
    }

    if (dictionary.hasKey(KeySingleFileInput)) {
        _singleFileInput = dictionary.value<bool>(KeySingleFileInput);
    }

    _octreeManager = std::make_shared<OctreeManager>();
    _indexOctreeManager = std::make_shared<OctreeManager>();

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

    _octreeManager->initOctree(0, _maxDist, _maxStarsPerNode);

    LINFO("Reading data file: " + _inFileOrFolderPath);

    LINFO("MAX_DIST: " + std::to_string(_octreeManager->maxDist()) +
        " - MAX_STARS_PER_NODE: " + std::to_string(_octreeManager->maxStarsPerNode()));

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

    // Slice LOD data before writing to files.
    _octreeManager->sliceLodData();

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
    /*float maxRadius = 0.0;
    int starsOutside10 = 0;
    int starsOutside25 = 0;
    int starsOutside50 = 0;
    int starsOutside75 = 0;
    int starsOutside100 = 0;
    int starsOutside200 = 0;
    int starsOutside300 = 0;
    int starsOutside400 = 0;
    int starsOutside500 = 0;
    int starsOutside750 = 0;
    int starsOutside1000 = 0;
    int starsOutside1500 = 0;
    int starsOutside2000 = 0;
    int starsOutside5000 = 0;*/

    ghoul::filesystem::Directory currentDir(_inFileOrFolderPath);
    std::vector<std::string> allInputFiles = currentDir.readFiles();
    std::vector<float> filterValues;
    auto writeThreads = std::vector<std::thread>(8);

    _indexOctreeManager->initOctree(0, _maxDist, _maxStarsPerNode);

    float processOneFile = 1.f / allInputFiles.size();

    LINFO("MAX_DIST: " + std::to_string(_indexOctreeManager->maxDist()) +
        " - MAX_STARS_PER_NODE: " + std::to_string(_indexOctreeManager->maxStarsPerNode()));

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

                    /*float maxVal = fmax(fmax(fabs(renderValues[0]), fabs(renderValues[1])), 
                        fabs(renderValues[2]));
                    if (maxVal > maxRadius) maxRadius = maxVal;
                    // Calculate how many stars are outside of different thresholds.
                    if (maxVal > 10) starsOutside10++;
                    if (maxVal > 25) starsOutside25++;
                    if (maxVal > 50) starsOutside50++;
                    if (maxVal > 75) starsOutside75++;
                    if (maxVal > 100) starsOutside100++;
                    if (maxVal > 200) starsOutside200++;
                    if (maxVal > 300) starsOutside300++;
                    if (maxVal > 400) starsOutside400++;
                    if (maxVal > 500) starsOutside500++;
                    if (maxVal > 750) starsOutside750++;
                    if (maxVal > 1000) starsOutside1000++;
                    if (maxVal > 1500) starsOutside1500++;
                    if (maxVal > 2000) starsOutside2000++;
                    if (maxVal > 5000) starsOutside5000++;*/
                }
            }
            inFileStream.close();
        }
        else {
            LERROR(fmt::format("Error opening file '{}' for loading preprocessed file!"
                , inFilePath));
        }

        // Slice LOD data.
        LINFO("Slicing LOD data!");
        _indexOctreeManager->sliceLodData(idx);

        progressCallback((idx + 1) * processOneFile);
        nStars += nStarsInfile;

        LINFO(fmt::format("Writing {} stars to octree files!", nStarsInfile));
        LINFO("Number of leaf nodes: " + std::to_string(_indexOctreeManager->numLeafNodes()) +
            "\n Number of inner nodes: " + std::to_string(_indexOctreeManager->numInnerNodes()) +
            "\n Total depth of tree: " + std::to_string(_indexOctreeManager->totalDepth()));

        // Write to 8 separate files in a separate thread. Data will be cleared after it 
        // has been written. Store joinable thread for later sync.
        std::thread t(&OctreeManager::writeToMultipleFiles, _indexOctreeManager,
            _outFileOrFolderPath, idx);
        writeThreads[idx] = std::move(t);
    }

    LINFO("A total of " + std::to_string(nStars) + " stars where read from files and distributed into "
        + std::to_string(_indexOctreeManager->totalNodes()) + " total nodes!");

    /*LINFO("Max radius of dataset is: " + std::to_string(maxRadius) + "\n Number of stars outside of:" +  
        " - 10kPc is " + std::to_string(starsOutside10) + "\n" + 
        " - 25kPc is " + std::to_string(starsOutside25) + "\n" +
        " - 50kPc is " + std::to_string(starsOutside50) + "\n" + 
        " - 75kPc is " + std::to_string(starsOutside75) + "\n" +
        " - 100kPc is " + std::to_string(starsOutside100) + "\n" + 
        " - 200kPc is " + std::to_string(starsOutside200) + "\n" +
        " - 300kPc is " + std::to_string(starsOutside300) + "\n" +
        " - 400kPc is " + std::to_string(starsOutside400) + "\n" +
        " - 500kPc is " + std::to_string(starsOutside500) + "\n" +
        " - 750kPc is " + std::to_string(starsOutside750) + "\n" +
        " - 1000kPc is " + std::to_string(starsOutside1000) + "\n" +
        " - 1500kPc is " + std::to_string(starsOutside1500) + "\n" +
        " - 2000kPc is " + std::to_string(starsOutside2000) + "\n" +
        " - 5000kPc is " + std::to_string(starsOutside5000));*/

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

    // Make sure all threads are done.
    for (int i = 0; i < 8; ++i) {
        writeThreads[i].join();
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
                KeyMaxDist,
                new IntVerifier,
                Optional::Yes,
                "If set it determines what MAX_DIST to use when creating Octree."
            },
            {
                KeyMaxStarsPerNode,
                new IntVerifier,
                Optional::Yes,
                "If set it determines what MAX_STAR_PER_NODE to use when creating Octree."
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
