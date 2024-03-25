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

#include <modules/gaia/tasks/constructoctreetask.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/format.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>
#include <filesystem>
#include <fstream>
#include <thread>

namespace {
    constexpr std::string_view _loggerCat = "ConstructOctreeTask";

    struct [[codegen::Dictionary(ConstructOctreeTask)]] Parameters {
        // If SingleFileInput is set to true then this specifies the path to a single BIN
        // file containing a full dataset. Otherwise this specifies the path to a folder
        // with multiple BIN files containing subsets of sorted star data
        std::string inFileOrFolderPath;

        // If SingleFileInput is set to true then this specifies the output file name
        // (including full path). Otherwise this specifies the path to the folder which to
        // save all files
        std::string outFileOrFolderPath;

        // If set it determines what MAX_DIST to use when creating Octree
        std::optional<int> maxDist;

        // If set it determines what MAX_STAR_PER_NODE to use when creating Octree
        std::optional<int> maxStarsPerNode;

        // If true then task will read from a single file and output a single binary file
        // with the full Octree. If false then task will read all files in specified
        // folder and output multiple files for the Octree
        std::optional<bool> singleFileInput;

        // If defined then only stars with Position X values between [min, max] will be
        // inserted into Octree (if min is set to 0.0 it is read as -Inf, if max is set to
        // 0.0 it is read as +Inf). If min = max then all values equal min|max will be
        // filtered away
        std::optional<glm::vec2> filterPosX;

        // If defined then only stars with Position Y values between [min, max] will be
        // inserted into Octree (if min is set to 0.0 it is read as -Inf, if max is set to
        // 0.0 it is read as +Inf). If min = max then all values equal min|max will be
        // filtered away
        std::optional<glm::vec2> filterPosY;

        // If defined then only stars with Position Z values between [min, max] will be
        // inserted into Octree (if min is set to 0.0 it is read as -Inf, if max is set to
        // 0.0 it is read as +Inf). If min = max then all values equal min|max will be
        // filtered away
        std::optional<glm::vec2> filterPosZ;

        // If defined then only stars with G mean magnitude values between [min, max] will
        // be inserted into Octree (if min is set to 20.0 it is read as -Inf, if max is
        // set to 20.0 it is read as +Inf). If min = max then all values equal min|max
        // will be filtered away. Default GMag = 20.0 if no value existed
        std::optional<glm::vec2> filterGMag;

        // If defined then only stars with Bp-Rp color values between [min, max] will be
        // inserted into Octree (if min is set to 0.0 it is read as -Inf, if max is set to
        // 0.0 it is read as +Inf). If min = max then all values equal min|max will be
        // filtered away
        std::optional<glm::vec2> filterBpRp;

        // If defined then only stars with Velocity X values between [min, max] will be
        // inserted into Octree (if min is set to 0.0 it is read as -Inf, if max is set to
        // 0.0 it is read as +Inf). If min = max then all values equal min|max will be
        // filtered away
        std::optional<glm::vec2> filterVelX;

        // If defined then only stars with Velocity Y values between [min, max] will be
        // inserted into Octree (if min is set to 0.0 it is read as -Inf, if max is set to
        // 0.0 it is read as +Inf). If min = max then all values equal min|max will be
        // filtered away
        std::optional<glm::vec2> filterVelY;

        // If defined then only stars with Velocity Z values between [min, max] will be
        // inserted into Octree (if min is set to 0.0 it is read as -Inf, if max is set to
        // 0.0 it is read as +Inf). If min = max then all values equal min|max will be
        // filtered away
        std::optional<glm::vec2> filterVelZ;

        // If defined then only stars with Bp mean magnitude values between [min, max]
        // will be inserted into Octree (if min is set to 20.0 it is read as -Inf, if max
        // is set to 20.0 it is read as +Inf). If min = max then all values equal min|max
        // will be filtered away. Default BpMag = 20.0 if no value existed
        std::optional<glm::vec2> filterBpMag;

        // If defined then only stars with Rp mean magnitude values between [min, max]
        // will be inserted into Octree (if min is set to 20.0 it is read as -Inf, if max
        // is set to 20.0 it is read as +Inf). If min = max then all values equal min|max
        // will be filtered away. Default RpMag = 20.0 if no value existed
        std::optional<glm::vec2> filterRpMag;

        // If defined then only stars with Bp-G color values between [min, max] will be
        // inserted into Octree (if min is set to 0.0 it is read as -Inf, if max is set to
        // 0.0 it is read as +Inf). If min = max then all values equal min|max will be
        // filtered away
        std::optional<glm::vec2> filterBpG;

        // If defined then only stars with G-Rp color values between [min, max] will be
        // inserted into Octree (if min is set to 0.0 it is read as -Inf, if max is set to
        // 0.0 it is read as +Inf). If min = max then all values equal min|max will be
        // filtered away
        std::optional<glm::vec2> filterGRp;

        // If defined then only stars with RA values between [min, max] will be inserted
        // into Octree (if min is set to 0.0 it is read as -Inf, if max is set to 0.0 it
        // is read as +Inf). If min = max then all values equal min|max will be filtered
        // away
        std::optional<glm::vec2> filterRa;

        // If defined then only stars with RA Error values between [min, max] will be
        // inserted into Octree (if min is set to 0.0 it is read as -Inf, if max is set to
        // 0.0 it is read as +Inf). If min = max then all values equal min|max will be
        // filtered away
        std::optional<glm::vec2> filterRaError;

        // If defined then only stars with DEC values between [min, max] will be inserted
        // into Octree (if min is set to 0.0 it is read as -Inf, if max is set to 0.0 it
        // is read as +Inf). If min = max then all values equal min|max will be filtered
        // away
        std::optional<glm::vec2> filterDec;

        // If defined then only stars with DEC Error values between [min, max] will be
        // inserted into Octree (if min is set to 0.0 it is read as -Inf, if max is set to
        // 0.0 it is read as +Inf). If min = max then all values equal min|max will be
        // filtered away
        std::optional<glm::vec2> filterDecError;

        // If defined then only stars with Parallax values between [min, max] will be
        // inserted into Octree (if min is set to 0.0 it is read as -Inf, if max is set to
        // 0.0 it is read as +Inf). If min = max then all values equal min|max will be
        // filtered away
        std::optional<glm::vec2> filterParallax;

        // If defined then only stars with Parallax Error values between [min, max] will
        // be inserted into Octree (if min is set to 0.0 it is read as -Inf, if max is set
        // to 0.0 it is read as +Inf). If min = max then all values equal min|max will be
        // filtered away
        std::optional<glm::vec2> filterParallaxError;

        // If defined then only stars with Proper Motion RA values between [min, max] will
        // be inserted into Octree (if min is set to 0.0 it is read as -Inf, if max is set
        // to 0.0 it is read as +Inf). If min = max then all values equal min|max will be
        // filtered away
        std::optional<glm::vec2> filterPmra;

        // If defined then only stars with Proper Motion RA Error values between
        // [min, max] will be inserted into Octree (if min is set to 0.0 it is read as
        // -Inf, if max is set to 0.0 it is read as +Inf). If min = max then all values
        // equal min|max will be filtered away
        std::optional<glm::vec2> filterPmraError;

        // If defined then only stars with Proper Motion DEC values between [min, max]
        // will be inserted into Octree (if min is set to 0.0 it is read as -Inf, if max
        // is set to 0.0 it is read as +Inf). If min = max then all values equal min|max
        // will be filtered away
        std::optional<glm::vec2> filterPmdec;

        // If defined then only stars with Proper Motion DEC Error values between
        // [min, max] will be inserted into Octree (if min is set to 0.0 it is read as
        // -Inf, if max is set to 0.0 it is read as +Inf). If min = max then all values
        // equal min|max will be filtered away
        std::optional<glm::vec2> filterPmdecError;

        // If defined then only stars with Radial Velocity values between [min, max] will
        // be inserted into Octree (if min is set to 0.0 it is read as -Inf, if max is set
        // to 0.0 it is read as +Inf). If min = max then all values equal min|max will be
        // filtered away
        std::optional<glm::vec2> filterRv;

        // If defined then only stars with Radial Velocity Error values between [min, max]
        // will be inserted into Octree (if min is set to 0.0 it is read as -Inf, if max
        // is set to 0.0 it is read as +Inf). If min = max then all values equal min|max
        // will be filtered away
        std::optional<glm::vec2> filterRvError;
    };
#include "constructoctreetask_codegen.cpp"
} // namespace

namespace openspace {

ConstructOctreeTask::ConstructOctreeTask(const ghoul::Dictionary& dictionary) {
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _inFileOrFolderPath = absPath(p.inFileOrFolderPath);
    _outFileOrFolderPath = absPath(p.outFileOrFolderPath);
    _maxDist = p.maxDist.value_or(_maxDist);
    _maxStarsPerNode = p.maxStarsPerNode.value_or(_maxStarsPerNode);
    _singleFileInput = p.singleFileInput.value_or(_singleFileInput);

    _octreeManager = std::make_shared<OctreeManager>();
    _indexOctreeManager = std::make_shared<OctreeManager>();


    _posX = p.filterPosX.value_or(_posX);
    _filterPosX = p.filterPosX.has_value();

    _posY = p.filterPosY.value_or(_posY);
    _filterPosY = p.filterPosY.has_value();

    _posZ = p.filterPosZ.value_or(_posZ);
    _filterPosZ = p.filterPosZ.has_value();

    _gMag = p.filterGMag.value_or(_gMag);
    _filterGMag = p.filterGMag.has_value();

    _bpRp = p.filterBpRp.value_or(_bpRp);
    _filterBpRp = p.filterBpRp.has_value();

    _velX = p.filterVelX.value_or(_velX);
    _filterVelX = p.filterVelX.has_value();

    _velY = p.filterVelY.value_or(_velY);
    _filterVelY = p.filterVelY.has_value();

    _velZ = p.filterVelZ.value_or(_velZ);
    _filterVelZ = p.filterVelZ.has_value();

    _bpMag = p.filterBpMag.value_or(_bpMag);
    _filterBpMag = p.filterBpMag.has_value();

    _rpMag = p.filterRpMag.value_or(_rpMag);
    _filterRpMag = p.filterRpMag.has_value();

    _bpG = p.filterBpG.value_or(_bpG);
    _filterBpG = p.filterBpG.has_value();

    _gRp = p.filterGRp.value_or(_gRp);
    _filterGRp = p.filterGRp.has_value();

    _ra = p.filterRa.value_or(_ra);
    _filterRa = p.filterRa.has_value();

    _raError = p.filterRaError.value_or(_raError);
    _filterRaError = p.filterRaError.has_value();

    _dec = p.filterDec.value_or(_dec);
    _filterDec = p.filterDec.has_value();

    _decError = p.filterDecError.value_or(_decError);
    _filterDecError = p.filterDecError.has_value();

    _parallax = p.filterParallax.value_or(_parallax);
    _filterParallax = p.filterParallax.has_value();

    _parallaxError = p.filterParallaxError.value_or(_parallaxError);
    _filterParallaxError = p.filterParallaxError.has_value();

    _pmra = p.filterPmra.value_or(_pmra);
    _filterPmra = p.filterPmra.has_value();

    _pmraError = p.filterPmraError.value_or(_pmraError);
    _filterPmraError = p.filterPmraError.has_value();

    _pmdec = p.filterPmdec.value_or(_pmdec);
    _filterPmdec = p.filterPmdec.has_value();

    _pmdecError = p.filterPmdecError.value_or(_pmdecError);
    _filterPmdecError = p.filterPmdecError.has_value();

    _rv = p.filterRv.value_or(_rv);
    _filterRv = p.filterRv.has_value();

    _rvError = p.filterRvError.value_or(_rvError);
    _filterRvError = p.filterRvError.has_value();
}

std::string ConstructOctreeTask::description() {
    return std::format(
        "Read bin file (or files in folder) '{}' and write octree data file (or files) "
        "into '{}'", _inFileOrFolderPath, _outFileOrFolderPath
    );
}

void ConstructOctreeTask::perform(const Task::ProgressCallback& onProgress) {
    onProgress(0.f);

    if (_singleFileInput) {
        constructOctreeFromSingleFile(onProgress);
    }
    else {
        constructOctreeFromFolder(onProgress);
    }

    onProgress(1.f);
}

void ConstructOctreeTask::constructOctreeFromSingleFile(
                                           const Task::ProgressCallback& progressCallback)
{
    std::vector<float> fullData;
    int32_t nValues = 0;
    int32_t nValuesPerStar = 0;
    size_t nFilteredStars = 0;
    int nTotalStars = 0;

    _octreeManager->initOctree(0, _maxDist, _maxStarsPerNode);

    LINFO(std::format("Reading data file '{}'", _inFileOrFolderPath));

    LINFO(std::format(
        "MAX DIST: {} - MAX STARS PER NODE: {}",
        _octreeManager->maxDist(), _octreeManager->maxStarsPerNode()
    ));

    // Use to generate a synthetic dataset
    /*for (float z = -1.0; z < 1.0; z += 0.05) {
        for (float y = -1.0; y < 1.0; y += 0.05) {
            for (float x = -1.0; x < 1.0; x += 0.05) {
                float r = static_cast <float> (rand()) / static_cast <float> (RAND_MAX);
                std::vector<float> renderValues(8);
                renderValues[0] = x; // + x * r;
                renderValues[2] = z; // + y * r;
                renderValues[1] = y; // + z * r;
                renderValues[3] = 5.0; // + 10 * r;
                renderValues[4] = 2.0; // + 10 * r;
                renderValues[5] = r;
                renderValues[6] = r;
                renderValues[7] = r;
                _octreeManager->insert(renderValues);
                nTotalStars++;
                nValues+=8;
            }
        }
    }

   for (float phi = -180.0; phi < 180.0; phi += 10.0) {
        for (float theta = -90.0; theta <= 90.0; theta += 10.0) {
            float r = 1.0;
            std::vector<float> renderValues(8);
            renderValues[0] = r * sin(glm::radians(theta)) * cos(glm::radians(phi));
            renderValues[2] = r * sin(glm::radians(theta)) * sin(glm::radians(phi));
            renderValues[1] = r * cos(glm::radians(theta));
            renderValues[3] = 5.0;
            renderValues[4] = 2.0;
            renderValues[5] = r;
            renderValues[6] = r;
            renderValues[7] = r;
            _octreeManager->insert(renderValues);
            nTotalStars++;
            nValues += 8;
        }
    }*/

    std::ifstream inFileStream(_inFileOrFolderPath, std::ifstream::binary);
    if (inFileStream.good()) {
        inFileStream.read(reinterpret_cast<char*>(&nValues), sizeof(int32_t));
        inFileStream.read(reinterpret_cast<char*>(&nValuesPerStar), sizeof(int32_t));

        fullData.resize(nValues);
        inFileStream.read(
            reinterpret_cast<char*>(fullData.data()),
            nValues * sizeof(fullData[0])
        );
        nTotalStars = nValues / nValuesPerStar;

        progressCallback(0.3f);
        LINFO("Constructing Octree");

        // Insert star into octree. We assume the data already is in correct order.
        for (size_t i = 0; i < fullData.size(); i += nValuesPerStar) {
            auto first = fullData.begin() + i;
            auto last = fullData.begin() + i + nValuesPerStar;
            const std::vector<float> filterValues(first, last);
            const std::vector<float> renderValues(first, first + RENDER_VALUES);

            // Filter data by parameters.
            if (checkAllFilters(filterValues)) {
                nFilteredStars++;
                continue;
            }

            // If all filters passed then insert render values into Octree.
            _octreeManager->insert(renderValues);
        }
        inFileStream.close();
    }
    else {
        LERROR(std::format(
            "Error opening file '{}' for loading preprocessed file", _inFileOrFolderPath
        ));
    }
    LINFO(std::format("{} of {} read stars were filtered", nFilteredStars, nTotalStars));

    // Slice LOD data before writing to files.
    _octreeManager->sliceLodData();

    LINFO(std::format("Writing octree to '{}'", _outFileOrFolderPath));
    std::ofstream outFileStream(_outFileOrFolderPath, std::ofstream::binary);
    if (outFileStream.good()) {
        if (nValues == 0) {
            LERROR("Error writing file - No values were read from file");
        }
        _octreeManager->writeToFile(outFileStream, true);

        outFileStream.close();
    }
    else {
        LERROR(std::format(
            "Error opening file '{}' as output data file", _outFileOrFolderPath
        ));
    }
}

void ConstructOctreeTask::constructOctreeFromFolder(
                                           const Task::ProgressCallback& progressCallback)
{
    int32_t nStars = 0;
    int32_t nValuesPerStar = 0;
    size_t nFilteredStars = 0;
    //float maxRadius = 0.0;
    //int starsOutside10 = 0;
    //int starsOutside25 = 0;
    //int starsOutside50 = 0;
    //int starsOutside75 = 0;
    //int starsOutside100 = 0;
    //int starsOutside200 = 0;
    //int starsOutside300 = 0;
    //int starsOutside400 = 0;
    //int starsOutside500 = 0;
    //int starsOutside750 = 0;
    //int starsOutside1000 = 0;
    //int starsOutside1500 = 0;
    //int starsOutside2000 = 0;
    //int starsOutside5000 = 0;

    std::vector<std::filesystem::path> allInputFiles;
    if (std::filesystem::is_directory(_inFileOrFolderPath)) {
        namespace fs = std::filesystem;
        for (const fs::directory_entry& e : fs::directory_iterator(_inFileOrFolderPath)) {
            if (!e.is_regular_file()) {
                allInputFiles.push_back(e.path());
            }
        }
    }

    std::vector<float> filterValues;
    auto writeThreads = std::vector<std::thread>(8);

    _indexOctreeManager->initOctree(0, _maxDist, _maxStarsPerNode);

    const float processOneFile = 1.f / allInputFiles.size();

    LINFO(std::format(
        "MAX DIST: {} - MAX STARS PER NODE: {}",
        _indexOctreeManager->maxDist(), _indexOctreeManager->maxStarsPerNode()
    ));

    for (size_t idx = 0; idx < allInputFiles.size(); idx++) {
        std::filesystem::path inFilePath = allInputFiles[idx];
        int nStarsInfile = 0;

        LINFO(std::format("Reading data file '{}'", inFilePath));

        std::ifstream inFileStream(inFilePath, std::ifstream::binary);
        if (inFileStream.good()) {
            inFileStream.read(reinterpret_cast<char*>(&nValuesPerStar), sizeof(int32_t));
            filterValues.resize(nValuesPerStar, 0.f);

            while (inFileStream.read(
                reinterpret_cast<char*>(filterValues.data()),
                nValuesPerStar * sizeof(filterValues[0])
            ))
            {
                // Filter data by parameters.
                if (checkAllFilters(filterValues)) {
                    nFilteredStars++;
                    continue;
                }
                // Generate a 50/12,5 dataset (gMag <=13/>13).
                //if ((filterStar(glm::vec2(20.0), filterValues[3], 20.f)) ||
                //    (filterStar(glm::vec2(0.0), filterValues[16])) ||
                //    (filterValues[3] > 13.0 && filterValues[17] > 0.125) ||
                //    (filterValues[3] <= 13.0 && filterValues[17] > 0.5)) {
                //    nFilteredStars++;
                //    continue;
                //}

                // If all filters passed then insert render values into Octree.
                const std::vector<float> renderValues(
                    filterValues.begin(),
                    filterValues.begin() + RENDER_VALUES
                );

                _indexOctreeManager->insert(renderValues);
                nStarsInfile++;

                //float maxVal = fmax(fmax(fabs(renderValues[0]), fabs(renderValues[1])),
                //    fabs(renderValues[2]));
                //if (maxVal > maxRadius) maxRadius = maxVal;
                //// Calculate how many stars are outside of different thresholds.
                //if (maxVal > 10) starsOutside10++;
                //if (maxVal > 25) starsOutside25++;
                //if (maxVal > 50) starsOutside50++;
                //if (maxVal > 75) starsOutside75++;
                //if (maxVal > 100) starsOutside100++;
                //if (maxVal > 200) starsOutside200++;
                //if (maxVal > 300) starsOutside300++;
                //if (maxVal > 400) starsOutside400++;
                //if (maxVal > 500) starsOutside500++;
                //if (maxVal > 750) starsOutside750++;
                //if (maxVal > 1000) starsOutside1000++;
                //if (maxVal > 1500) starsOutside1500++;
                //if (maxVal > 2000) starsOutside2000++;
                //if (maxVal > 5000) starsOutside5000++;
            }
            inFileStream.close();
        }
        else {
            LERROR(std::format(
                "Error opening file '{}' for loading preprocessed file", inFilePath
            ));
        }

        // Slice LOD data.
        LINFO("Slicing LOD data");
        _indexOctreeManager->sliceLodData(idx);

        progressCallback((idx + 1) * processOneFile);
        nStars += nStarsInfile;

        LINFO(std::format("Writing {} stars to octree files", nStarsInfile));
        LINFO(std::format(
            "Number leaf nodes: {}\n Number inner nodes: {}\n Total depth of tree: {}",
            _indexOctreeManager->numLeafNodes(),
            _indexOctreeManager->numInnerNodes(),
            _indexOctreeManager->totalDepth()
        ));

        // Write to 8 separate files in a separate thread. Data will be cleared after it
        // has been written. Store joinable thread for later sync.
        std::thread t(
            &OctreeManager::writeToMultipleFiles,
            _indexOctreeManager,
            _outFileOrFolderPath.string(),
            idx
        );
        writeThreads[idx] = std::move(t);
    }

    LINFO(std::format(
        "A total of {} stars were read from files and distributed into {} total nodes",
        nStars, _indexOctreeManager->totalNodes()
    ));
    LINFO(std::to_string(nFilteredStars) + " stars were filtered");

    //LINFO("Max radius of dataset is: " + std::to_string(maxRadius) +
    //    "\n Number of stars outside of:" +
    //    " - 10kPc is " + std::to_string(starsOutside10) + "\n" +
    //    " - 25kPc is " + std::to_string(starsOutside25) + "\n" +
    //    " - 50kPc is " + std::to_string(starsOutside50) + "\n" +
    //    " - 75kPc is " + std::to_string(starsOutside75) + "\n" +
    //    " - 100kPc is " + std::to_string(starsOutside100) + "\n" +
    //    " - 200kPc is " + std::to_string(starsOutside200) + "\n" +
    //    " - 300kPc is " + std::to_string(starsOutside300) + "\n" +
    //    " - 400kPc is " + std::to_string(starsOutside400) + "\n" +
    //    " - 500kPc is " + std::to_string(starsOutside500) + "\n" +
    //    " - 750kPc is " + std::to_string(starsOutside750) + "\n" +
    //    " - 1000kPc is " + std::to_string(starsOutside1000) + "\n" +
    //    " - 1500kPc is " + std::to_string(starsOutside1500) + "\n" +
    //    " - 2000kPc is " + std::to_string(starsOutside2000) + "\n" +
    //    " - 5000kPc is " + std::to_string(starsOutside5000));

    // Write index file of Octree structure.
    std::filesystem::path indexFileOutPath = std::format(
        "{}/index.bin", _outFileOrFolderPath.string()
    );
    std::ofstream outFileStream(indexFileOutPath, std::ofstream::binary);
    if (outFileStream.good()) {
        LINFO("Writing index file");
        _indexOctreeManager->writeToFile(outFileStream, false);

        outFileStream.close();
    }
    else {
        LERROR(std::format(
            "Error opening file '{}' as index output file", indexFileOutPath
        ));
    }

    // Make sure all threads are done.
    for (int i = 0; i < 8; i++) {
        writeThreads[i].join();
    }
}

bool ConstructOctreeTask::checkAllFilters(const std::vector<float>& filterValues) {
    // Return true if star is caught in any filter.
    return (_filterPosX && filterStar(_posX, filterValues[0])) ||
        (_filterPosY && filterStar(_posY, filterValues[1])) ||
        (_filterPosZ && filterStar(_posZ, filterValues[2])) ||
        (_filterGMag && filterStar(_gMag, filterValues[3], 20.f)) ||
        (_filterBpRp && filterStar(_bpRp, filterValues[4])) ||
        (_filterVelX && filterStar(_velX, filterValues[5])) ||
        (_filterVelY && filterStar(_velY, filterValues[6])) ||
        (_filterVelZ && filterStar(_velZ, filterValues[7])) ||
        (_filterBpMag && filterStar(_bpMag, filterValues[8], 20.f)) ||
        (_filterRpMag && filterStar(_rpMag, filterValues[9], 20.f)) ||
        (_filterBpG && filterStar(_bpG, filterValues[10])) ||
        (_filterGRp && filterStar(_gRp, filterValues[11])) ||
        (_filterRa && filterStar(_ra, filterValues[12])) ||
        (_filterRaError && filterStar(_raError, filterValues[13])) ||
        (_filterDec && filterStar(_dec, filterValues[14])) ||
        (_filterDecError && filterStar(_decError, filterValues[15])) ||
        (_filterParallax && filterStar(_parallax, filterValues[16])) ||
        (_filterParallaxError && filterStar(_parallaxError, filterValues[17])) ||
        (_filterPmra && filterStar(_pmra, filterValues[18])) ||
        (_filterPmraError && filterStar(_pmraError, filterValues[19])) ||
        (_filterPmdec && filterStar(_pmdec, filterValues[20])) ||
        (_filterPmdecError && filterStar(_pmdecError, filterValues[21])) ||
        (_filterRv && filterStar(_rv, filterValues[22])) ||
        (_filterRvError && filterStar(_rvError, filterValues[23]));
}

bool ConstructOctreeTask::filterStar(const glm::vec2& range, float filterValue,
                                     float normValue)
{
    // Return true if star should be filtered away, i.e. if min = max = filterValue or
    // if filterValue < min (when min != 0.0) or filterValue > max (when max != 0.0).
    constexpr float eps = std::numeric_limits<float>::epsilon();
    return (std::abs(range.x - range.y) < eps &&
        std::abs(range.x - filterValue) < eps) ||
        (std::abs(range.x - normValue) > eps && filterValue < range.x) ||
        (std::abs(range.y - normValue) > eps && filterValue > range.y);
}

documentation::Documentation ConstructOctreeTask::Documentation() {
    return codegen::doc<Parameters>("gaiamission_constructoctreefrombin");
}

} // namespace openspace
