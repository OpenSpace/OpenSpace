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
    const char* KeyInFolderPath = "InFolderPath";
    const char* KeyOutFolderPath = "OutFolderPath";
    const char* KeyFirstRow = "FirstRow";
    const char* KeyLastRow = "LastRow";
    const char* KeyColumnNames = "ColumnNames";

    constexpr const char* _loggerCat = "ReadFitsTask";
} // namespace

namespace openspace {

ReadFitsTask::ReadFitsTask(const ghoul::Dictionary& dictionary)
    : _inFolderPath("")
    , _outFolderPath("")
    , _firstRow(0)
    , _lastRow(0)
{
    
    openspace::documentation::testSpecificationAndThrow(
        documentation(),
        dictionary,
        "ReadFitsTask"
    );

    _inFolderPath = absPath(dictionary.value<std::string>(KeyInFolderPath));
    _outFolderPath = absPath(dictionary.value<std::string>(KeyOutFolderPath));

    if (dictionary.hasKey(KeyFirstRow)) {
        _firstRow = static_cast<int>(dictionary.value<double>(KeyFirstRow));
    }

    if (dictionary.hasKey(KeyLastRow)) {
        _lastRow = static_cast<int>(dictionary.value<double>(KeyLastRow));
    }
    

    if (dictionary.hasKey(KeyColumnNames)) {
        auto tmpDict = dictionary.value<ghoul::Dictionary>(KeyColumnNames);
        auto stringKeys = tmpDict.keys();
        auto intKeys = std::set<int>();

        // Ugly fix for ASCII sorting when there are more columns read than 10.
        for (auto key : stringKeys) {
            intKeys.insert(std::stoi(key));
        }

        for (auto key : intKeys) {
            _columnNames.push_back(tmpDict.value<std::string>(std::to_string(key)));
        }
    }
}

ReadFitsTask::~ReadFitsTask() {}

std::string ReadFitsTask::description() {
    return "Read all fits files in folder: " + _inFolderPath + "\n and write raw star data into: "
        + _outFolderPath + "\n. Which columns and rows to read can be defined by user.";
}

void ReadFitsTask::perform(const Task::ProgressCallback& progressCallback) {
    
    std::vector<std::vector<float>> octants(8);
    std::vector<bool> isFirstWrite(8, true);

    progressCallback(0.0f);
    srand(1234567890);
    if (_firstRow <= 0) _firstRow = 1;

    ghoul::filesystem::Directory currentDir(_inFolderPath);
    std::vector<std::string> allInputFiles = currentDir.readFiles();

    FitsFileReader fitsInFile(false);

    // TODO: Divide into several threads!
    while (!allInputFiles.empty()) {
        std::string fileToRead = allInputFiles.back();
        allInputFiles.erase(allInputFiles.end() - 1);

        // Read columns from FITS file. If rows aren't specified then full table will be read.
        std::shared_ptr<TableData<float>> table = fitsInFile.readTable<float>(fileToRead,
            _columnNames, _firstRow, _lastRow);

        if (!table) {
            throw ghoul::RuntimeError(fmt::format("Failed to open Fits file '{}'", fileToRead));
        }
        
        int nStars = table->readRows - _firstRow + 1;

        int32_t nValuesPerStar = 8; //_columnNames.size() + 1; // +1 for B-V color value.
        int nNullArr = 0;
        size_t defaultCols = nValuesPerStar;
        int multiplier = 1;

        // Copy columns to local variables.
        std::unordered_map<std::string, std::vector<float>>& tableContent = table->contents;
        std::vector<float> l_longitude = std::move(tableContent[_columnNames[0]]);
        std::vector<float> ra_err = std::move(tableContent[_columnNames[1]]);
        std::vector<float> b_latitude = std::move(tableContent[_columnNames[2]]);
        std::vector<float> dec_err = std::move(tableContent[_columnNames[3]]);
        std::vector<float> parallax = std::move(tableContent[_columnNames[4]]);
        std::vector<float> parallax_err = std::move(tableContent[_columnNames[5]]);
        std::vector<float> pr_mot_ra = std::move(tableContent[_columnNames[6]]);
        std::vector<float> pr_mot_ra_err = std::move(tableContent[_columnNames[7]]);
        std::vector<float> pr_mot_dec = std::move(tableContent[_columnNames[8]]);
        std::vector<float> pr_mot_dec_err = std::move(tableContent[_columnNames[9]]);
        std::vector<float> magCol = tableContent[_columnNames[10]];

        /*std::vector<float> posXcol = tableContent[_columnNames[0]];
        std::vector<float> posYcol = tableContent[_columnNames[1]];
        std::vector<float> posZcol = tableContent[_columnNames[2]];
        std::vector<float> velXcol = tableContent[_columnNames[3]];
        std::vector<float> velYcol = tableContent[_columnNames[4]];
        std::vector<float> velZcol = tableContent[_columnNames[5]];
        std::vector<float> magCol = tableContent[_columnNames[6]];
        std::vector<float> parallax = tableContent[_columnNames[7]];
        std::vector<float> parallax_err = tableContent[_columnNames[8]];
        std::vector<float> pr_mot_ra = tableContent[_columnNames[9]];
        std::vector<float> pr_mot_ra_err = tableContent[_columnNames[10]];
        std::vector<float> pr_mot_dec = tableContent[_columnNames[11]];
        std::vector<float> pr_mot_dec_err = tableContent[_columnNames[12]];
        std::vector<float> tycho_b = tableContent[_columnNames[13]];
        std::vector<float> tycho_b_err = tableContent[_columnNames[14]];
        std::vector<float> tycho_v = tableContent[_columnNames[15]];
        std::vector<float> tycho_v_err = tableContent[_columnNames[16]];*/

        // Construct data array. OBS: ORDERING IS IMPORTANT! This is where slicing happens.
        for (int i = 0; i < nStars * multiplier; ++i) {
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
            float radiusInKiloParsec = 10.0;
            if (!std::isnan(parallax[i])) {
                //LINFO("Parallax: " + std::to_string(parallax[i]));
                radiusInKiloParsec = 1.0 / parallax[i];
            }
            values[idx++] = radiusInKiloParsec * sin(b_latitude[i]) * cos(l_longitude[i]);
            values[idx++] = radiusInKiloParsec * sin(b_latitude[i]) * sin(l_longitude[i]);
            values[idx++] = radiusInKiloParsec * cos(b_latitude[i]);
            //values[idx++] = posXcol[i%nStars];
            //values[idx++] = posYcol[i%nStars];
            //values[idx++] = posZcol[i%nStars];

            // Return early if star doesn't have a measured position.
            //if (values[0] == -999 && values[1] == -999 && values[2] == -999) {
            //    nNullArr++;
            //    continue;
            //}


            // Store color values.
            values[idx++] = std::isnan(magCol[i]) ? 0.f : magCol[i];
            values[idx++] = std::isnan(magCol[i]) ? 0.f : magCol[i];
            //values[idx++] = magCol[i%nStars];
            //values[idx++] = tycho_b[i%nStars] - tycho_v[i%nStars];

            if (std::isnan(pr_mot_ra[i])) pr_mot_ra[i] = 0.f;
            if(std::isnan(pr_mot_dec[i])) pr_mot_dec[i] = 0.f;

            // Store velocity convert it with parallax.
            float velXcol = radiusInKiloParsec * sin(pr_mot_ra[i]) * cos(pr_mot_dec[i]);
            float velYcol = radiusInKiloParsec * sin(pr_mot_ra[i]) * sin(pr_mot_dec[i]);
            float velZcol = radiusInKiloParsec * cos(pr_mot_ra[i]);
            values[idx++] = convertMasPerYearToMeterPerSecond(velXcol, parallax[i]);
            values[idx++] = convertMasPerYearToMeterPerSecond(velYcol, parallax[i]);
            values[idx++] = convertMasPerYearToMeterPerSecond(velZcol, parallax[i]);
            //values[idx++] = convertMasPerYearToMeterPerSecond(velXcol[i%nStars], parallax[i%nStars]);
            //values[idx++] = convertMasPerYearToMeterPerSecond(velYcol[i%nStars], parallax[i%nStars]);
            //values[idx++] = convertMasPerYearToMeterPerSecond(velZcol[i%nStars], parallax[i%nStars]);

            // TODO: Store additional parameters to filter by.
            //values[idx++] = parallax[i%nStars];
            //values[idx++] = parallax_err[i%nStars];
            //values[idx++] = pr_mot_ra[i%nStars];
            //values[idx++] = pr_mot_ra_err[i%nStars];
            //values[idx++] = pr_mot_dec[i%nStars];
            //values[idx++] = pr_mot_dec_err[i%nStars];
            //values[idx++] = tycho_b[i%nStars];
            //values[idx++] = tycho_b_err[i%nStars];
            //values[idx++] = tycho_v[i%nStars];
            //values[idx++] = tycho_v_err[i%nStars];

            // Read extra columns, if any. This will slow down the sorting tremendously!
            //for (size_t col = defaultCols; col < nValuesPerStar; ++col) {
            //    std::vector<float> vecData = tableContent[_columnNames[col]];
            //    values[idx++] = vecData[i];
            //}

            for (size_t j = 0; j < nValuesPerStar; ++j) {
                // The astronomers in Vienna use -999 as default value. Change it to 0.
                if (std::isnan(values[j])) {
                    values[j] = 0.f;
                }
                //if (values[j] == -999) {
                //    values[j] = 0.f;
                //}
                else if (multiplier > 1) {
                    values[j] *= static_cast<float>(rand()) / static_cast<float>(RAND_MAX);
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
        //LINFO("Multiplier: " + std::to_string(multiplier));

        for (int i = 0; i < 8; ++i) {
            if (octants[i].size() > MAX_SIZE_BEFORE_WRITE) {
                // TODO: Serialize writes!
                // Write to file! TODO: Move to function!
                std::string outPath = _outFolderPath + "octant_" + std::to_string(i) + ".bin";
                std::ofstream fileStream(outPath, std::ofstream::binary | std::ofstream::app);
                if (fileStream.good()) {

                    int32_t nValues = static_cast<int32_t>(octants[i].size());
                    LINFO("Write " + std::to_string(nValues) + " values to " + outPath);

                    if (nValues == 0) {
                        LERROR("Error writing file - No values were read from file.");
                    }
                    // We don't know how many values there are!
                    //fileStream.write(reinterpret_cast<const char*>(&nValues), sizeof(int32_t));
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

    progressCallback(1.0f);
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
                KeyInFolderPath,
                new StringVerifier,
                Optional::No,
                "The path to the folder with all FITS files that are to be read.",
            },
            {
                KeyOutFolderPath,
                new StringVerifier,
                Optional::No,
                "The path to the folder to export raw star data to.",
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
                KeyColumnNames,
                new StringListVerifier,
                Optional::No,
                "A list of strings with the names of all the columns that are to be "
                "read from the specified FITS file(s).",
            },

        }
    };
}

} // namespace openspace
