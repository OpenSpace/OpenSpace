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

#include <ghoul/misc/dictionary.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/fmt.h>

#include <fstream>

namespace {
    const char* KeyInFilePath = "InFilePath";
    const char* KeyOutFilePath = "OutFilePath";
    const char* KeyFirstRow = "FirstRow";
    const char* KeyLastRow = "LastRow";
    const char* KeyColumnNames = "ColumnNames";

    constexpr const char* _loggerCat = "ReadFitsTask";
    constexpr int8_t CurrentFileVersion = 1;
} // namespace

namespace openspace {

ReadFitsTask::ReadFitsTask(const ghoul::Dictionary& dictionary) {
    
    openspace::documentation::testSpecificationAndThrow(
        documentation(),
        dictionary,
        "ReadFitsTask"
    );

    _inFilePath = absPath(dictionary.value<std::string>(KeyInFilePath));
    _outFilePath = absPath(dictionary.value<std::string>(KeyOutFilePath));
    _firstRow = static_cast<int>(dictionary.value<double>(KeyFirstRow));
    _lastRow = static_cast<int>(dictionary.value<double>(KeyLastRow));

    if (dictionary.hasKey(KeyColumnNames)) {
        auto tmpDict = dictionary.value<ghoul::Dictionary>(KeyColumnNames);
        auto tmpKeys = tmpDict.keys();

        for (auto key : tmpKeys) {
            _columnNames.push_back(tmpDict.value<std::string>(key));
        }
    }
}

ReadFitsTask::~ReadFitsTask() {}

std::string ReadFitsTask::description() {
    return "Read fits file: " + _inFilePath + "\n and write raw star data into: "
        + _outFilePath + "\n. Which columns and rows to read is defined by user.";
}

void ReadFitsTask::perform(const Task::ProgressCallback& progressCallback) {
    std::vector<float> fullData;
    int nStars = _lastRow - _firstRow + 1;
    progressCallback(0.0f);

    FitsFileReader fitsInFile(false);
    std::shared_ptr<TableData<float>> table = fitsInFile.readTable<float>(_inFilePath, 
        _columnNames, _firstRow, _lastRow);

    if (!table) {
        LERROR(fmt::format("Failed to open Fits file '{}'", _inFilePath));
    }
    progressCallback(0.5f);

    int32_t nValuesPerStar = _columnNames.size();
    int nNullArr = 0;
    size_t defaultCols = 8;

    std::unordered_map<string, std::vector<float>>& tableContent = table->contents;
    std::vector<float> posXcol = tableContent[_columnNames[0]];
    std::vector<float> posYcol = tableContent[_columnNames[1]];
    std::vector<float> posZcol = tableContent[_columnNames[2]];
    std::vector<float> velXcol = tableContent[_columnNames[3]];
    std::vector<float> velYcol = tableContent[_columnNames[4]];
    std::vector<float> velZcol = tableContent[_columnNames[5]];
    std::vector<float> magCol = tableContent[_columnNames[6]];
    std::vector<float> parallax = tableContent[_columnNames[7]];
    //std::vector<float> parallax_err = tableContent[_columnNames[8]];
    //std::vector<float> pr_mot_ra = tableContent[_columnNames[9]];
    //std::vector<float> pr_mot_ra_err = tableContent[_columnNames[10]];
    //std::vector<float> pr_mot_dec = tableContent[_columnNames[11]];
    //std::vector<float> pr_mot_dec_err = tableContent[_columnNames[12]];
    //std::vector<float> tycho_b = tableContent[_columnNames[13]];
    //std::vector<float> tycho_b_err = tableContent[_columnNames[14]];
    //std::vector<float> tycho_v = tableContent[_columnNames[15]];
    //std::vector<float> tycho_v_err = tableContent[_columnNames[16]];

    for (int i = 0; i < nStars; ++i) {
        std::vector<float> values(nValuesPerStar);
        size_t idx = 0;

        // Read positions.
        values[idx++] = posXcol[i];
        values[idx++] = posYcol[i];
        values[idx++] = posZcol[i];

        // Return early if star doesn't have a measured position.
        if (values[0] == -999 && values[1] == -999 && values[2] == -999) {
            nNullArr++;
            continue;
        }

        // Read the rest of the default values.
        values[idx++] = velXcol[i];
        values[idx++] = velYcol[i];
        values[idx++] = velZcol[i];
        values[idx++] = magCol[i];
        values[idx++] = parallax[i];
        //values[idx++] = parallax_err[i];
        //values[idx++] = pr_mot_ra[i];
        //values[idx++] = pr_mot_ra_err[i];
        //values[idx++] = pr_mot_dec[i];
        //values[idx++] = pr_mot_dec_err[i];
        //values[idx++] = tycho_b[i];
        //values[idx++] = tycho_b_err[i];
        //values[idx++] = tycho_v[i];
        //values[idx++] = tycho_v_err[i];

        // Read extra columns, if any. This will slow down the sorting tremendously!
        for (size_t col = defaultCols; col < nValuesPerStar; ++col) {
            std::vector<float> vecData = tableContent[_columnNames[col]];
            values[idx++] = vecData[i];
        }

        for (size_t j = 0; j < nValuesPerStar; ++j) {
            // The astronomers in Vienna use -999 as default value. Change it to 0.
            if (values[j] == -999) {
                values[j] = 0.f;
            }
        }

        fullData.insert(fullData.end(), values.begin(), values.end());
    }

    LINFO(std::to_string(nNullArr) + " out of " + std::to_string(nStars) +
        " read stars were nullArrays");
    
    progressCallback(0.9f);

    std::ofstream fileStream(_outFilePath, std::ofstream::binary);
    if (fileStream.good()) {

        int32_t nValues = static_cast<int32_t>(fullData.size());
        if (nValues == 0) {
            LERROR("Error writing file - No values were read from file.");
        }
        fileStream.write(reinterpret_cast<const char*>(&nValues), sizeof(int32_t));
        fileStream.write(reinterpret_cast<const char*>(&nValuesPerStar), sizeof(int32_t));

        size_t nBytes = nValues * sizeof(fullData[0]);
        fileStream.write(reinterpret_cast<const char*>(&fullData[0]), nBytes);

        fileStream.close();
    }
    else {
        LERROR(fmt::format("Error opening file: {} as output data file.", _outFilePath));
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
                KeyInFilePath,
                new StringVerifier,
                Optional::No,
                "The path to the FITS file that are to be read.",
            },
            {
                KeyOutFilePath,
                new StringVerifier,
                Optional::No,
                "The path to the file to export raw VBO data to.",
            },
            {
                KeyFirstRow,
                new IntVerifier,
                Optional::No,
                "Defines the first row that will be read from the specified FITS file.",
            },
            {
                KeyLastRow,
                new IntVerifier,
                Optional::No,
                "Defines the last row that will be read from the specified FITS file."
                "Has to be equal to or greater than FirstRow.",
            },
            {
                KeyColumnNames,
                new StringListVerifier,
                Optional::No,
                "A list of strings with the names of all the columns that are to be "
                "read from the specified FITS file.",
            },

        }
    };
}

} // namespace openspace
