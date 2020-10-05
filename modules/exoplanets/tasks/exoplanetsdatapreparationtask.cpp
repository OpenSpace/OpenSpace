/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#include <modules/exoplanets/tasks/exoplanetsdatapreparationtask.h>

#include <modules/exoplanets/exoplanetshelper.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/json.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/fmt.h>
#include <ghoul/glm.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>
#include <charconv>
#include <fstream>

 // TEST: do not optimise for now
#pragma optimize("", off)

namespace {
    constexpr const char* KeyInputDataFile = "InputDataFile";
    constexpr const char* KeyInputSpeck = "InputSPECK";
    constexpr const char* KeyOutputBin = "OutputBIN";
    constexpr const char* KeyOutputLut = "OutputLUT";
    constexpr const char* KeyTeffToBv = "TeffToBvFile";

    constexpr const char* _loggerCat = "ExoplanetsDataPreparationTask";
} // namespace

namespace openspace::exoplanets {

using json = nlohmann::json;

ExoplanetsDataPreparationTask::ExoplanetsDataPreparationTask(const ghoul::Dictionary& dictionary) {
    openspace::documentation::testSpecificationAndThrow(
        documentation(),
        dictionary,
        "ExoplanetsDataPreparationTask"
    );

    _inputDataPath = absPath(dictionary.value<std::string>(KeyInputDataFile));
    _inputSpeckPath = absPath(dictionary.value<std::string>(KeyInputSpeck));
    _outputBinPath = absPath(dictionary.value<std::string>(KeyOutputBin));
    _outputLutPath = absPath(dictionary.value<std::string>(KeyOutputLut));
    _teffToBvFilePath = absPath(dictionary.value<std::string>(KeyTeffToBv));
}

std::string ExoplanetsDataPreparationTask::description() {
    return fmt::format(
        "Extract metadata from file '{}' and write as bin to '{}'", // TODO: update description
        _inputDataPath,
        _outputBinPath
    );
}

void ExoplanetsDataPreparationTask::perform(const Task::ProgressCallback& progressCallback) {
    std::ifstream inputDataFile(_inputDataPath);
    if (!inputDataFile.good()) {
        LERROR(fmt::format("Failed to open input file '{}'", _inputDataPath));
        return;
    }

    std::ofstream binFile(_outputBinPath, std::ios::out | std::ios::binary);
    std::ofstream lutFile(_outputLutPath);

    int version = 1;
    binFile.write(reinterpret_cast<char*>(&version), sizeof(int));

    std::string planetRow;
    getline(inputDataFile, planetRow); // The first line, containing the data names

    // read column names into a vector, for access later
    std::vector<std::string> columnNames;
    std::stringstream sStream(planetRow);
    std::string colName;
    while (getline(sStream, colName, ',')) {
        columnNames.push_back(colName);
    }

    // read total number of items
    int total = 0;
    while (getline(inputDataFile, planetRow)) {
        ++total;
    }
    inputDataFile.clear();
    inputDataFile.seekg(0);
    getline(inputDataFile, planetRow); // The first line, containing the data names

    LINFO(fmt::format("Loading {} exoplanets", total));

    auto readFloatData = [](const std::string& str) -> float {
        float result;
        auto [p, ec] = std::from_chars(str.data(), str.data() + str.size(), result);
        if (ec == std::errc()) {
            return result;
        }
        return NAN;
    };

    auto readDoubleData = [](const std::string& str) -> double {
        double result;
        auto [p, ec] = std::from_chars(str.data(), str.data() + str.size(), result);
        if (ec == std::errc()) {
            return result;
        }
        return NAN;
    };

    auto readIntegerData = [](const std::string& str) -> int {
        int result;
        auto [p, ec] = std::from_chars(str.data(), str.data() + str.size(), result);
        if (ec == std::errc()) {
            return result;
        }
        return -1;
    };

    auto readStringData = [](const std::string& str) -> std::string {
        std::string result = str;
        // remove quotes, if any
        result.erase(std::remove(result.begin(), result.end(), '\"'), result.end());
        return result;
    };

    Exoplanet p;
    std::string data;
    int exoplanetCount = 0;
    while (getline(inputDataFile, planetRow)) {
        ++exoplanetCount;
        progressCallback(static_cast<float>(exoplanetCount) / static_cast<float>(total));

        std::string component;
        std::string speckStarname;

        std::istringstream lineStream(planetRow);
        int columnIndex = 0;
        while (getline(lineStream, data, ',')) {
            const std::string& column = columnNames[columnIndex];
            columnIndex++;

            if (column == "pl_letter") {
                component = readStringData(data);
            }
            // Orbital semi-major axis
            else if (column == "pl_orbsmax") {
                p.a = readFloatData(data);
            }
            else if (column == "pl_orbsmaxerr1") {
                p.aUpper = readDoubleData(data);
            }
            else if (column == "pl_orbsmaxerr2") {
                p.aLower = -readDoubleData(data);
            }
            // Orbital eccentricity
            else if (column == "pl_orbeccen") {
                p.ecc = readFloatData(data);
            }
            else if (column == "pl_orbeccenerr1") {
                p.eccUpper = readFloatData(data);
            }
            else if (column == "pl_orbeccenerr2") {
                p.eccLower = -readFloatData(data);
            }
            // Orbital inclination
            else if (column == "pl_orbincl") {
                p.i = readFloatData(data);
            }
            else if (column == "pl_orbinclerr1") {
                p.iUpper = readFloatData(data);
            }
            else if (column == "pl_orbinclerr2") {
                p.iLower = -readFloatData(data);
            }
            // Argument of periastron
            else if (column == "pl_orblper") {
                p.omega = readFloatData(data);
            }
            else if (column == "pl_orblpererr1") {
                p.omegaUpper = readFloatData(data);
            }
            else if (column == "pl_orblpererr2") {
                p.omegaLower = -readFloatData(data);
            }
            // Orbital period
            else if (column == "pl_orbper") {
                p.per = readDoubleData(data);
            }
            else if (column == "pl_orbpererr1") {
                p.perUpper = readFloatData(data);
            }
            else if (column == "pl_orbpererr2") {
                p.perLower = -readFloatData(data);
            }
            // Radius of the planet (Jupiter radii)
            else if (column == "pl_radj") {
                p.r = readDoubleData(data);
            }
            else if (column == "pl_radjerr1") {
                p.rUpper = readDoubleData(data);
            }
            else if (column == "pl_radjerr2") {
                p.rLower = -readDoubleData(data);
            }
            // Time of transit midpoint
            else if (column == "pl_tranmid") {
                p.tt = readDoubleData(data);
            }
            else if (column == "pl_tranmiderr1") {
                p.ttUpper = readFloatData(data);
            }
            else if (column == "pl_tranmiderr2") {
                p.ttLower = -readFloatData(data);
            }
            // Star - name and position
            else if (column == "hostname") {
                std::string name = readStringData(data);
                speckStarname = std::string(speckStarName(name));
                glm::vec3 position = starPosition(speckStarname);
                p.positionX = position[0];
                p.positionY = position[1];
                p.positionZ = position[2];
            }
            // Star radius
            else if (column == "st_rad") {
                p.rStar = readFloatData(data);
            }
            else if (column == "st_raderr1") {
                p.rStarUpper = readFloatData(data);
            }
            else if (column == "st_raderr2") {
                p.rStarLower = -readFloatData(data);
            }
            // Color of star (B-V color index computed from star's effective temperature)
            else if (column == "st_teff") {
                float teff = readFloatData(data);
                p.bmv = bvFromTeff(teff);
            }
            // Is the planet orbiting a binary system?
            else if (column == "cb_flag") {
                p.binary = static_cast<bool>(readIntegerData(data));
            }
            // Number of planets in the system
            else if (column == "sy_pnum") {
                p.nComp = readIntegerData(data);
            }
        }

        // @TODO (emmbr 2020-10-05) Currently, the dataset has no information about the
        // longitude of the ascending node, but maybe it might in the future
        p.bigOmega = NAN;
        p.bigOmegaUpper = NAN;
        p.bigOmegaLower = NAN;

        // create look-up table
        long pos = static_cast<long>(binFile.tellp());
        std::string planetName = speckStarname + " " + component;
        lutFile << planetName << "," << pos << std::endl;

        // Write to binary data file
        binFile.write(reinterpret_cast<char*>(&p), sizeof(Exoplanet));
    }

    progressCallback(1.f);
}

glm::vec3 ExoplanetsDataPreparationTask::starPosition(const std::string& starName) {
    std::ifstream exoplanetsFile(_inputSpeckPath);
    if (!exoplanetsFile) {
        LERROR(fmt::format("Error opening file expl.speck"));
    }

    glm::vec3 position{ NAN };
    std::string line;

    while (getline(exoplanetsFile, line)) {
        bool shouldSkipLine = (
            line.empty() || line[0] == '#' || line.substr(0, 7) == "datavar" ||
            line.substr(0, 10) == "texturevar" || line.substr(0, 7) == "texture"
        );

        if (shouldSkipLine) {
            continue;
        }

        std::string data;
        std::string name;
        std::istringstream linestream(line);
        getline(linestream, data, '#');
        getline(linestream, name);
        name.erase(0, 1);

        std::string coord;
        if (name == starName) {
            std::stringstream dataStream(data);
            getline(dataStream, coord, ' ');
            position[0] = std::stof(coord.c_str(), nullptr);
            getline(dataStream, coord, ' ');
            position[1] = std::stof(coord.c_str(), nullptr);
            getline(dataStream, coord, ' ');
            position[2] = std::stof(coord.c_str(), nullptr);
            break;
        }
    }

    return position;
}

float ExoplanetsDataPreparationTask::bvFromTeff(float teff) {
    if (std::isnan(teff)) {
        return NAN;
    }

    std::ifstream teffToBvFile(_teffToBvFilePath);
    if (!teffToBvFile.good()) {
        LERROR(fmt::format("Failed to open teff_bv.txt file"));
        return NAN;
    }

    float bv = 0.f;
    float bvUpper = 0.f;
    float bvLower = 0.f;
    float teffLower, teffUpper;
    std::string row, teffString, bvString;
    while (getline(teffToBvFile, row)) {
        std::istringstream lineStream(row);
        getline(lineStream, teffString, ',');
        getline(lineStream, bvString);

        float teffCurrent = std::stof(teffString.c_str(), nullptr);
        float bvCurrent = std::stof(bvString.c_str(), nullptr);

        if (teff > teffCurrent) {
            teffLower = teffCurrent;
            bvLower = bvCurrent;
        }
        else {
            teffUpper = teffCurrent;
            bvUpper = bvCurrent;
            if (bvLower == 0.f) {
                bv = 2.f;
            }
            else {
                float bvDiff = (bvUpper - bvLower);
                float teffDiff = (teffUpper - teffLower);
                bv = ((bvDiff * (teff - teffLower)) / teffDiff) + bvLower;
            }
            break;
        }
    }
    return bv;
}

documentation::Documentation ExoplanetsDataPreparationTask::documentation() {
    using namespace documentation;
    return {
        "ExoplanetsDataPreparationTask",
        "exoplanets_data_preparation_task",
        {
            {
                "Type",
                new StringEqualVerifier("ExoplanetsDataPreparationTask"),
                Optional::No,
                ""
            },
            {
                KeyInputDataFile,
                new StringAnnotationVerifier("A valid filepath"),
                Optional::No,
                "The csv file to extract data from"
            },
            {
                KeyInputSpeck,
                new StringAnnotationVerifier("A file path to a speck file"),
                Optional::No,
                "The speck file with star locations"
            },
            {
                KeyOutputBin,
                new StringAnnotationVerifier("A valid filepath"),
                Optional::No,
                "The bin file to export data into"
            },
            {
                KeyOutputLut,
                new StringAnnotationVerifier("A valid filepath"),
                Optional::No,
                "The txt file to write look-up table into"
            },
            {
                KeyTeffToBv,
                new StringAnnotationVerifier("A valid filepath"),
                Optional::No,
                "The path to a teff to bv conversion file"
            }
        }
    };
}

} // namespace openspace::exoplanets
