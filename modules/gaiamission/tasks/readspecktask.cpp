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
    constexpr int8_t CurrentFileVersion = 1;
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
    std::vector<float> fullData;

    progressCallback(0.0f);

    std::ifstream file(_inFilePath);
    if (!file.good()) {
        LERROR(fmt::format("Failed to open Speck file '{}'", _inFilePath));
        return;
    }

    int32_t nValuesPerStar = 0;
    int nNullArr = 0;
    size_t nStars = 0;

    // The beginning of the speck file has a header that either contains comments
    // (signaled by a preceding '#') or information about the structure of the file
    // (signaled by the keywords 'datavar', 'texturevar', 'texture' and 'maxcomment')
    std::string line = "";
    while (true) {
        std::streampos position = file.tellg();
        std::getline(file, line);

        if (line[0] == '#' || line.empty()) {
            continue;
        }

        if (line.substr(0, 7) != "datavar" &&
            line.substr(0, 10) != "texturevar" &&
            line.substr(0, 7) != "texture" && 
            line.substr(0, 10) != "maxcomment")
        {
            // We read a line that doesn't belong to the header, so we have to jump back
            // before the beginning of the current line.
            file.seekg(position);
            break;
        }

        if (line.substr(0, 7) == "datavar") {
            // datavar lines are structured as follows:
            // datavar # description
            // where # is the index of the data variable; so if we repeatedly overwrite
            // the 'nValues' variable with the latest index, we will end up with the total
            // number of values (+3 since X Y Z are not counted in the Speck file index)
            std::stringstream str(line);

            std::string dummy;
            str >> dummy;
            str >> nValuesPerStar;
            nValuesPerStar += 1; // We want the number, but the index is 0 based
        }
    }

    nValuesPerStar += 3; // X Y Z are not counted in the Speck file indices

    do {
        std::vector<float> values(nValuesPerStar);
        nStars++;

        std::getline(file, line);
        std::stringstream str(line);

        // Read values. 
        for (int i = 0; i < nValuesPerStar; ++i) {
            str >> values[i];
        }

        // Check if star is a nullArray.
        bool nullArray = true;
        for (size_t i = 0; i < values.size(); ++i) {
            if (values[i] != 0.0) {
                nullArray = false;
                break;
            }
        }

        // Insert to data if we found some values.
        if (!nullArray) {

            // Gaia DR1 data from AMNH measures positions in Parsec, but RenderableGaiaStars
            // expects kiloParsec (because fits file from Vienna had in kPc).
            // Thus we need to convert positions twice atm.
            values[0] /= 1000.0; // PosX
            values[1] /= 1000.0; // PosY
            values[2] /= 1000.0; // PosZ

            fullData.insert(fullData.end(), values.begin(), values.end());
        }
        else {
            nNullArr++;
        }

    } while (!file.eof());

    LINFO(std::to_string(nNullArr) + " out of " + std::to_string(nStars) +
        " read stars were nullArrays");

    
    progressCallback(0.9f);

    std::ofstream fileStream(_outFilePath, std::ofstream::binary);
    if (fileStream.good()) {

        int32_t nValues = static_cast<int32_t>(fullData.size());
        LINFO("nValues: " + std::to_string(nValues));

        if (nValues == 0) {
            LERROR("Error writing file - No values were read from file.");
        }
        fileStream.write(reinterpret_cast<const char*>(&nValues), sizeof(int32_t));
        fileStream.write(reinterpret_cast<const char*>(&nValuesPerStar), sizeof(int32_t));

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
