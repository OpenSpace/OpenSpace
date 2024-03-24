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

#include <modules/gaia/tasks/readfitstask.h>

#include <modules/gaia/tasks/readfilejob.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>

#include <ghoul/misc/dictionary.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/fmt.h>
#include <filesystem>
#include <fstream>
#include <set>
#include <optional>

namespace {
    constexpr std::string_view KeyFilterColumnNames = "FilterColumnNames";

    constexpr std::string_view _loggerCat = "ReadFitsTask";

    struct [[codegen::Dictionary(ReadFitsTask)]] Parameters {
        // If SingleFileProcess is set to true then this specifies the path to a single
        // FITS file that will be read. Otherwise it specifies the path to a folder with
        // multiple FITS files that are to be read
        std::string inFileOrFolderPath;

        // If SingleFileProcess is set to true then this specifies the name (including
        // entire path) to the output file. Otherwise it specifies the path to the output
        // folder which to export binary star data to
        std::string outFileOrFolderPath;

        // If true then task will read from a single FITS file and output a single binary
        // file. If false then task will read all files in specified folder and output
        // multiple files sorted by location
        std::optional<bool> singleFileProcess;

        // Defines how many threads to use when reading from multiple files
        std::optional<int> threadsToUse [[codegen::greater(1)]];

        // Defines the first row that will be read from the specified FITS file(s). If not
        // defined then reading will start at first row
        std::optional<int> firstRow;

        // Defines the last row that will be read from the specified FITS file(s). If not
        // defined (or less than FirstRow) then full file(s) will be read
        std::optional<int> lastRow;

        // A list of strings with the names of all the additional columns that are to be
        // read from the specified FITS file(s). These columns can be used for filtering
        // while constructing Octree later
        std::optional<std::vector<std::string>> filterColumnNames;
    };
#include "readfitstask_codegen.cpp"
} // namespace

namespace openspace {

ReadFitsTask::ReadFitsTask(const ghoul::Dictionary& dictionary) {
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _inFileOrFolderPath = absPath(p.inFileOrFolderPath);
    _outFileOrFolderPath = absPath(p.outFileOrFolderPath);
    _singleFileProcess = p.singleFileProcess.value_or(_singleFileProcess);
    _threadsToUse = p.threadsToUse.value_or(_threadsToUse);
    _firstRow = p.firstRow.value_or(_firstRow);
    _lastRow = p.lastRow.value_or(_lastRow);

    if (p.filterColumnNames.has_value()) {
        const ghoul::Dictionary d =
            dictionary.value<ghoul::Dictionary>(KeyFilterColumnNames);

        // Ugly fix for ASCII sorting when there are more columns read than 10.
        std::set<int> intKeys;
        for (const std::string_view key : d.keys()) {
            intKeys.insert(std::stoi(std::string(key)));
        }

        for (const int key : intKeys) {
            _filterColumnNames.push_back(d.value<std::string>(std::to_string(key)));
        }
    }
}

std::string ReadFitsTask::description() {
    return std::format(
        "Read the specified fits file (or all fits files in specified folder): '{}'\n "
        "and write raw star data into: '{}'\nAll columns required for default rendering "
        "and filtering parameters will always be read but user can define additional "
        "filter columns to read", _inFileOrFolderPath, _outFileOrFolderPath
    );
}

void ReadFitsTask::perform(const Task::ProgressCallback& onProgress) {
    onProgress(0.f);

    if (_singleFileProcess) {
        readSingleFitsFile(onProgress);
    }
    else {
        readAllFitsFilesFromFolder(onProgress);
    }

    onProgress(1.f);
}

void ReadFitsTask::readSingleFitsFile(const Task::ProgressCallback& progressCallback) {
    int32_t nValuesPerStar = 0;

    FitsFileReader fileReader(false);
    std::vector<float> fullData = fileReader.readFitsFile(
        _inFileOrFolderPath.string(),
        nValuesPerStar,
        _firstRow,
        _lastRow,
        _filterColumnNames
    );

    progressCallback(0.8f);

    std::ofstream outFileStream(_outFileOrFolderPath, std::ofstream::binary);
    if (outFileStream.good()) {
        int32_t nValues = static_cast<int32_t>(fullData.size());
        LINFO(std::format(
            "Writing {} values to file '{}'", nValues, _outFileOrFolderPath
        ));
        LINFO("Number of values per star: " + std::to_string(nValuesPerStar));

        if (nValues == 0) {
            LERROR("Error writing file - No values were read from file");
        }
        outFileStream.write(
            reinterpret_cast<const char*>(&nValues),
            sizeof(int32_t)
        );
        outFileStream.write(
            reinterpret_cast<const char*>(&nValuesPerStar),
            sizeof(int32_t)
        );

        const size_t nBytes = nValues * sizeof(fullData[0]);
        outFileStream.write(reinterpret_cast<const char*>(fullData.data()), nBytes);

        outFileStream.close();
    }
    else {
        LERROR(std::format(
            "Error opening file '{}' as output data file", _outFileOrFolderPath
        ));
    }
}

void ReadFitsTask::readAllFitsFilesFromFolder(const Task::ProgressCallback&) {
    std::vector<std::vector<float>> octants(8);
    std::vector<bool> isFirstWrite(8, true);
    size_t finishedJobs = 0;
    int totalStars = 0;

    _firstRow = std::max(_firstRow, 1);

    // Create Threadpool and JobManager
    LINFO("Threads in pool: " + std::to_string(_threadsToUse));
    ThreadPool threadPool(_threadsToUse);
    ConcurrentJobManager<std::vector<std::vector<float>>> jobManager(
        std::move(threadPool)
    );

    // Get all files in specified folder
    std::vector<std::filesystem::path> allInputFiles;
    if (std::filesystem::is_directory(_inFileOrFolderPath)) {
        namespace fs = std::filesystem;
        for (const fs::directory_entry& e : fs::directory_iterator(_inFileOrFolderPath)) {
            if (e.is_regular_file()) {
                allInputFiles.push_back(e.path());
            }
        }
    }

    const size_t nInputFiles = allInputFiles.size();
    LINFO("Files to read: " + std::to_string(nInputFiles));

    // Define what columns to read
    _allColumnNames.clear();
    // Read in the order of table in file
    std::vector<std::string> defaultColumnNames = {
        "ra",
        "ra_error",
        "dec",
        "dec_error",
        "parallax",
        "parallax_error",
        "pmra",
        "pmra_error",
        "pmdec",
        "pmdec_error",
        "phot_g_mean_mag",
        "phot_bp_mean_mag",
        "phot_rp_mean_mag",
        "bp_rp",
        "bp_g",
        "g_rp",
        "radial_velocity",
        "radial_velocity_error",
    };
    _allColumnNames.insert(
        _allColumnNames.end(),
        defaultColumnNames.begin(),
        defaultColumnNames.end()
    );
    // Append additional filter parameters to default rendering parameters.
    _allColumnNames.insert(
        _allColumnNames.end(),
        _filterColumnNames.begin(),
        _filterColumnNames.end()
    );

    std::string allNames = "Columns to read: \n";
    for (const std::string& colName : _allColumnNames) {
        allNames += colName + "\n";
    }
    LINFO(allNames);

    // Declare how many values to save for each star.
    constexpr int32_t NValuesPerStar = 24;
    const size_t nDefaultColumns = defaultColumnNames.size();
    auto fitsFileReader = std::make_shared<FitsFileReader>(false);

    // Divide all files into ReadFilejobs and then delegate them onto several threads!
    while (!allInputFiles.empty()) {
        const std::filesystem::path fileToRead = allInputFiles.back();
        allInputFiles.erase(allInputFiles.end() - 1);

        // Add reading of file to jobmanager, which will distribute it to our threadpool.
        auto readFileJob = std::make_shared<gaia::ReadFileJob>(
            fileToRead.string(),
            _allColumnNames,
            _firstRow,
            _lastRow,
            nDefaultColumns,
            NValuesPerStar,
            fitsFileReader
        );
        jobManager.enqueueJob(readFileJob);
    }

    LINFO("All files added to queue");

    // Check for finished jobs.
    while (finishedJobs < nInputFiles) {
        if (jobManager.numFinishedJobs() > 0) {
            std::vector<std::vector<float>> newOctant =
                jobManager.popFinishedJob()->product();

            finishedJobs++;

            for (int i = 0; i < 8; i++) {
                // Add read values to global octant and check if it's time to write!
                octants[i].insert(
                    octants[i].end(),
                    newOctant[i].begin(),
                    newOctant[i].end()
                );
                if ((octants[i].size() > MAX_SIZE_BEFORE_WRITE) ||
                    (finishedJobs == nInputFiles))
                {
                    // Write to file!
                    totalStars += writeOctantToFile(
                        octants[i],
                        i,
                        isFirstWrite,
                        NValuesPerStar
                    );

                    octants[i].clear();
                    octants[i].shrink_to_fit();
                }
            }
        }
    }
    LINFO(std::format("A total of {} stars were written to binary files", totalStars));
}

int ReadFitsTask::writeOctantToFile(const std::vector<float>& octantData, int index,
                                    std::vector<bool>& isFirstWrite, int nValuesPerStar)
{
    std::string outPath = std::format("{}octant_{}.bin", _outFileOrFolderPath, index);
    std::ofstream fileStream(outPath, std::ofstream::binary | std::ofstream::app);
    if (fileStream.good()) {
        int32_t nValues = static_cast<int32_t>(octantData.size());
        LINFO(std::format("Write {} values to {}", nValues, outPath));

        if (nValues == 0) {
            LERROR("Error writing file - No values were read from file");
        }
        // If this is the first write then write number of values per star!
        if (isFirstWrite[index]) {
            LINFO(std::format("First write for Octant_{}", index));
            fileStream.write(
                reinterpret_cast<const char*>(&nValuesPerStar),
                sizeof(int32_t)
            );
            isFirstWrite[index] = false;
        }

        const size_t nBytes = nValues * sizeof(octantData[0]);
        fileStream.write(reinterpret_cast<const char*>(octantData.data()), nBytes);

        fileStream.close();

        // Return number of stars written.
        return nValues / nValuesPerStar;
    }
    else {
        LERROR(std::format("Error opening file '{}' as output data file", outPath));
        return 0;
    }
}

documentation::Documentation ReadFitsTask::Documentation() {
    return codegen::doc<Parameters>("gaiamission_fitsfiletorawdata");
}

} // namespace openspace
