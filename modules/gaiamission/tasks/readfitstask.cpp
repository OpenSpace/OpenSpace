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

#include <modules/gaiamission/tasks/readfitstask.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/util/distanceconversion.h>

#include <ghoul/misc/dictionary.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/directory.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/fmt.h>

#include <fstream>
#include <set>

namespace {
    const char* KeyInFileOrFolderPath = "InFileOrFolderPath";
    const char* KeyOutFileOrFolderPath = "OutFileOrFolderPath";
    const char* KeySingleFileProcess = "SingleFileProcess";
    const char* KeyFirstRow = "FirstRow";
    const char* KeyLastRow = "LastRow";
    const char* KeyFilterColumnNames = "FilterColumnNames";

    constexpr const char* _loggerCat = "ReadFitsTask";
} // namespace

namespace openspace {

ReadFitsTask::ReadFitsTask(const ghoul::Dictionary& dictionary)
    : _inFileOrFolderPath("")
    , _outFileOrFolderPath("")
    , _singleFileProcess(false)
    , _firstRow(0)
    , _lastRow(0)
{
    
    openspace::documentation::testSpecificationAndThrow(
        documentation(),
        dictionary,
        "ReadFitsTask"
    );

    _inFileOrFolderPath = absPath(dictionary.value<std::string>(KeyInFileOrFolderPath));
    _outFileOrFolderPath = absPath(dictionary.value<std::string>(KeyOutFileOrFolderPath));

    if (dictionary.hasKey(KeySingleFileProcess)) {
        _singleFileProcess = dictionary.value<bool>(KeySingleFileProcess);
    }

    if (dictionary.hasKey(KeyFirstRow)) {
        _firstRow = static_cast<int>(dictionary.value<double>(KeyFirstRow));
    }

    if (dictionary.hasKey(KeyLastRow)) {
        _lastRow = static_cast<int>(dictionary.value<double>(KeyLastRow));
    }
    

    if (dictionary.hasKey(KeyFilterColumnNames)) {
        auto tmpDict = dictionary.value<ghoul::Dictionary>(KeyFilterColumnNames);
        auto stringKeys = tmpDict.keys();
        auto intKeys = std::set<int>();

        // Ugly fix for ASCII sorting when there are more columns read than 10.
        for (auto key : stringKeys) {
            intKeys.insert(std::stoi(key));
        }

        for (auto key : intKeys) {
            _filterColumnNames.push_back(tmpDict.value<std::string>(std::to_string(key)));
        }
    }
}

ReadFitsTask::~ReadFitsTask() {}

std::string ReadFitsTask::description() {
    return "Read the specified fits file (or all fits files in specified folder): " + 
        _inFileOrFolderPath + "\n and write raw star data into: " + _outFileOrFolderPath + 
        "\n. All columns required for default rendering parameters will always be read but user can "
        "define which columns to read for filtering in ConstructOctreeTask later. Threshold for "
        "filtering are defined in 'gaia_octree.task'.";
}

void ReadFitsTask::perform(const Task::ProgressCallback& progressCallback) {
    
    progressCallback(0.0f);

    if (_singleFileProcess) {
        readSingleFitsFile(progressCallback);
    }
    else {
        readAllFitsFilesFromFolder(progressCallback);
    }

    progressCallback(1.0f);
}

void ReadFitsTask::readSingleFitsFile(const Task::ProgressCallback& progressCallback) {
    
    int32_t nValuesPerStar = 0;

    FitsFileReader fileReader(false);
    std::vector<float> fullData = fileReader.readFitsFile(_inFileOrFolderPath, nValuesPerStar, _firstRow,
        _lastRow, _filterColumnNames);

    progressCallback(0.8f);

    std::ofstream outFileStream(_outFileOrFolderPath, std::ofstream::binary);
    if (outFileStream.good()) {

        int32_t nValues = static_cast<int32_t>(fullData.size());
        LINFO("Writing " + std::to_string(nValues) + " values to file: " + _outFileOrFolderPath);
        LINFO("Number of values per star: " + std::to_string(nValuesPerStar));

        if (nValues == 0) {
            LERROR("Error writing file - No values were read from file.");
        }
        outFileStream.write(reinterpret_cast<const char*>(&nValues), sizeof(int32_t));
        outFileStream.write(reinterpret_cast<const char*>(&nValuesPerStar), sizeof(int32_t));

        size_t nBytes = nValues * sizeof(fullData[0]);
        outFileStream.write(reinterpret_cast<const char*>(fullData.data()), nBytes);

        outFileStream.close();
    }
    else {
        LERROR(fmt::format("Error opening file: {} as output data file.", _outFileOrFolderPath));
    }
}

void ReadFitsTask::readAllFitsFilesFromFolder(const Task::ProgressCallback& progressCallback) {
    std::vector<std::vector<float>> octants(8);
    std::vector<bool> isFirstWrite(8, true);

    if (_firstRow <= 0) _firstRow = 1;

    ghoul::filesystem::Directory currentDir(_inFileOrFolderPath);
    std::vector<std::string> allInputFiles = currentDir.readFiles();

    // TODO: Move to function after DR2!?
    // Define what columns to read. Append additional filter parameters to default rendering parameters.
    _allColumnNames = std::vector<std::string>({
        "l",
        "b",
        "parallax",
        "pmra",
        "pmdec",
        "phot_g_mean_mag"
        });
    _allColumnNames.insert(_allColumnNames.end(), _filterColumnNames.begin(), _filterColumnNames.end());
    //size_t nFilterColumns = _filterColumnNames.size();

    std::string allNames = "Columns to read: \n";
    for (auto colName : _allColumnNames) {
        allNames += colName + "\n";
    }
    LINFO(allNames);

    FitsFileReader fitsFileReader(false);

    // TODO: Divide into several threads!
    while (!allInputFiles.empty()) {
        std::string fileToRead = allInputFiles.back();
        allInputFiles.erase(allInputFiles.end() - 1);

        // Read columns from FITS file. If rows aren't specified then full table will be read.
        std::shared_ptr<TableData<float>> table = fitsFileReader.readTable<float>(fileToRead,
            _allColumnNames, _firstRow, _lastRow);

        if (!table) {
            throw ghoul::RuntimeError(fmt::format("Failed to open Fits file '{}'", fileToRead));
        }

        int nStars = table->readRows - _firstRow + 1;

        int nNullArr = 0;
        size_t nColumnsRead = _allColumnNames.size();
        size_t defaultCols = 11; // Number of columns that are copied by predefined code. 
        if (nColumnsRead != defaultCols) {
            LINFO("Additional columns will be read! Consider add column in code for significant speedup!");
        }

        // Declare how many values to save for each star.
        int32_t nValuesPerStar = 16;


        // Copy columns to local variables.
        std::unordered_map<std::string, std::vector<float>>& tableContent = table->contents;

        // Default rendering parameters.
        std::vector<float> l_longitude = std::move(tableContent[_allColumnNames[0]]);
        std::vector<float> b_latitude = std::move(tableContent[_allColumnNames[1]]);
        std::vector<float> parallax = std::move(tableContent[_allColumnNames[2]]);
        std::vector<float> pr_mot_ra = std::move(tableContent[_allColumnNames[3]]);
        std::vector<float> pr_mot_dec = std::move(tableContent[_allColumnNames[4]]);
        std::vector<float> magCol = std::move(tableContent[_allColumnNames[5]]);

        // Default filter parameters
        std::vector<float> ra_err = std::move(tableContent[_allColumnNames[6]]);
        std::vector<float> dec_err = std::move(tableContent[_allColumnNames[7]]);
        std::vector<float> parallax_err = std::move(tableContent[_allColumnNames[8]]);
        std::vector<float> pr_mot_ra_err = std::move(tableContent[_allColumnNames[9]]);
        std::vector<float> pr_mot_dec_err = std::move(tableContent[_allColumnNames[10]]);


        // Construct data array. OBS: ORDERING IS IMPORTANT! This is where slicing happens.
        for (int i = 0; i < nStars; ++i) {
            std::vector<float> values(nValuesPerStar);
            size_t idx = 0;

            // Default order for rendering:
            // Position [X, Y, Z]
            // Absolute Magnitude
            // B-V Color
            // Velocity [X, Y, Z]

            // Return early if star doesn't have a measured position.
            if (std::isnan(b_latitude[i]) || std::isnan(l_longitude[i])) {
                nNullArr++;
                continue;
            }

            // Store positions.
            float radiusInKiloParsec = 9.0;
            if (!std::isnan(parallax[i])) {
                //LINFO("Parallax: " + std::to_string(parallax[i]));
                radiusInKiloParsec = 1.0 / parallax[i];
            }
            // Convert to Galactic Coordinates from Galactic Lon & Lat.
            values[idx++] = radiusInKiloParsec * sin(b_latitude[i]) * cos(l_longitude[i]); // Pos X
            values[idx++] = radiusInKiloParsec * sin(b_latitude[i]) * sin(l_longitude[i]); // Pos Y
            values[idx++] = radiusInKiloParsec * cos(b_latitude[i]); // Pos Z


            // Store color values.
            values[idx++] = std::isnan(magCol[i]) ? 0.f : magCol[i]; // AbsMag
            values[idx++] = std::isnan(magCol[i]) ? 0.f : magCol[i]; // B-V color TODO!


            // Store velocity. 
            if (std::isnan(pr_mot_ra[i])) pr_mot_ra[i] = 0.f;
            if (std::isnan(pr_mot_dec[i])) pr_mot_dec[i] = 0.f;
            // Convert to mas/year from Proper Motion
            float velXcol = radiusInKiloParsec * sin(pr_mot_ra[i]) * cos(pr_mot_dec[i]);
            float velYcol = radiusInKiloParsec * sin(pr_mot_ra[i]) * sin(pr_mot_dec[i]);
            float velZcol = radiusInKiloParsec * cos(pr_mot_ra[i]);
            // Convert it to m / s with parallax.
            values[idx++] = convertMasPerYearToMeterPerSecond(velXcol, parallax[i]); // Vel X
            values[idx++] = convertMasPerYearToMeterPerSecond(velYcol, parallax[i]); // Vel Y
            values[idx++] = convertMasPerYearToMeterPerSecond(velZcol, parallax[i]); // Vel Z

            // Store additional parameters to filter by.
            values[idx++] = ra_err[i];
            values[idx++] = dec_err[i];
            values[idx++] = parallax[i];
            values[idx++] = parallax_err[i];
            values[idx++] = pr_mot_ra[i];
            values[idx++] = pr_mot_ra_err[i];
            values[idx++] = pr_mot_dec[i];
            values[idx++] = pr_mot_dec_err[i];
            

            // Read extra columns, if any. This will slow down the sorting tremendously!
            for (size_t col = defaultCols; col < nColumnsRead; ++col) {
                std::vector<float> vecData = std::move(tableContent[_allColumnNames[col]]);
                values[idx++] = vecData[i];
            }

            for (size_t j = 0; j < nValuesPerStar; ++j) {
                // Change NaNs to to 0.0 floats.
                if (std::isnan(values[j])) {
                    values[j] = 0.f;
                }
            }

            size_t index = 0;
            if (values[0] < 0.0) index += 1;
            if (values[1] < 0.0) index += 2;
            if (values[2] < 0.0) index += 4;

            octants[index].insert(octants[index].end(), values.begin(), values.end());
        }

        LINFO(std::to_string(nNullArr) + " out of " +
            std::to_string(nStars) + " read stars were nullArrays.");

        for (int i = 0; i < 8; ++i) {
            if (octants[i].size() > MAX_SIZE_BEFORE_WRITE) {
                // TODO: Parallelize writes!?
                // Write to file! TODO: Move to function!
                std::string outPath = _outFileOrFolderPath + "octant_" + std::to_string(i) + ".bin";
                std::ofstream fileStream(outPath, std::ofstream::binary | std::ofstream::app);
                if (fileStream.good()) {

                    int32_t nValues = static_cast<int32_t>(octants[i].size());
                    LINFO("Write " + std::to_string(nValues) + " values to " + outPath);

                    if (nValues == 0) {
                        LERROR("Error writing file - No values were read from file.");
                    }
                    // If this is the first write then write number of values per star!
                    if (isFirstWrite[i]) {
                        LINFO("First write for Octant_" + std::to_string(i));
                        fileStream.write(reinterpret_cast<const char*>(&nValuesPerStar), sizeof(int32_t));
                        isFirstWrite[i] = false;
                    }

                    size_t nBytes = nValues * sizeof(octants[i][0]);
                    fileStream.write(reinterpret_cast<const char*>(octants[i].data()), nBytes);

                    fileStream.close();
                }
                else {
                    LERROR(fmt::format("Error opening file: {} as output data file.", outPath));
                }

                octants[i].clear();
            }
        }
    }
}

documentation::Documentation ReadFitsTask::Documentation() {
    using namespace documentation;
    return {
        "ReadFitsFile",
        "gaiamission_fitsfiletorawdata",
        {
            {
                "Type",
                new StringEqualVerifier("ReadFitsTask"),
                Optional::No
            },
            {
                KeyInFileOrFolderPath,
                new StringVerifier,
                Optional::No,
                "If SingleFileProcess is set to true then this specifies the path to a single FITS "
                "file that will be read. Otherwise it specifies the path to a folder with multiple "
                "FITS files that are to be read.",
            },
            {
                KeyOutFileOrFolderPath,
                new StringVerifier,
                Optional::No,
                "If SingleFileProcess is set to true then this specifies the name (including entire "
                "path) to the output file. Otherwise it specifies the path to the output folder "
                "which to export binary star data to.",
            },
            {
                KeySingleFileProcess,
                new BoolVerifier,
                Optional::Yes,
                "If true then task will read from a single FITS file and output a single binary file. "
                "If false then task will read all files in specified folder and output multiple files "
                "sorted by location."
            },
            {
                KeyFirstRow,
                new IntVerifier,
                Optional::Yes,
                "Defines the first row that will be read from the specified FITS file(s). "
                "If not defined then reading will start at first row.",
            },
            {
                KeyLastRow,
                new IntVerifier,
                Optional::Yes,
                "Defines the last row that will be read from the specified FITS file(s). "
                "If not defined (or less than FirstRow) then full file(s) will be read.",
            },
            {
                KeyFilterColumnNames,
                new StringListVerifier,
                Optional::Yes,
                "A list of strings with the names of all the additional columns that are to be "
                "read from the specified FITS file(s). These columns can be used for filtering "
                "while constructing Octree later.",
            },

        }
    };
}

} // namespace openspace
