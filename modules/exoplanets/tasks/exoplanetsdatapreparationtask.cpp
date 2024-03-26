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

#include <modules/exoplanets/tasks/exoplanetsdatapreparationtask.h>

#include <modules/exoplanets/exoplanetshelper.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/util/coordinateconversion.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/format.h>
#include <ghoul/glm.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/stringhelper.h>
#include <charconv>
#include <filesystem>
#include <fstream>

namespace {
    constexpr std::string_view _loggerCat = "ExoplanetsDataPreparationTask";

    // This task is used for generating the binary data files that are used for the
    // exoplanet system loading in OpenSpace. Using this binary file allows efficient
    // data loading of an arbitrary exoplanet system during runtime, without keeping all
    // data in memory.
    //
    // Two output files are generated, whose paths have to be specified: One binary with
    // the data for the exoplanets (OutputBIN) and one look-up table that is used to
    // find where in the binary file a particular system is located (OutputLUT).
    //
    // Additionally, the task uses three different files as input: 1) a CSV file with the
    // data from the NASA Exoplanet Archive, 2) A SPECK file that contains star positions,
    // and 3) a TXT file that is used for the conversion from the stars' effective
    // temperature to a B-V color index. The paths for all these paths have to be
    // specified. The SPECK file (2) will be used for the positions of the host stars, to
    // make sure that they line up with the stars in that dataset. The cross-matching is
    // done by star name, as given by the comment in the SPECK file and the host star
    // column in the exoplanet dataset.
    //
    // Note that the CSV (1) has to include a certain set of columns for the rendering to
    // be correct. Use the accompanying python script to download the datafile, or make
    // sure to include all columns in your download.
    struct [[codegen::Dictionary(ExoplanetsDataPreparationTask)]] Parameters {
        // The csv file to extract data from
        std::string inputDataFile;

        // The speck file with star locations
        std::string inputSPECK;

        // The bin file to export data into
        std::string outputBIN [[codegen::annotation("A valid filepath")]];

        // The txt file to write look-up table into
        std::string outputLUT [[codegen::annotation("A valid filepath")]];

        // The path to a teff to bv conversion file. Should be a txt file where each line
        // has the format 'teff,bv'
        std::string teffToBvFile;
    };
#include "exoplanetsdatapreparationtask_codegen.cpp"
} // namespace

namespace openspace::exoplanets {

documentation::Documentation ExoplanetsDataPreparationTask::documentation() {
    return codegen::doc<Parameters>("exoplanets_data_preparation_task");
}

ExoplanetsDataPreparationTask::ExoplanetsDataPreparationTask(
                                                      const ghoul::Dictionary& dictionary)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _inputDataPath = absPath(p.inputDataFile);
    _inputSpeckPath = absPath(p.inputSPECK);
    _outputBinPath = absPath(p.outputBIN);
    _outputLutPath = absPath(p.outputLUT);
    _teffToBvFilePath = absPath(p.teffToBvFile);
}

std::string ExoplanetsDataPreparationTask::description() {
    return std::format(
        "Extract data about exoplanets from file '{}' and write as bin to '{}'. The data "
        "file should be a csv version of the Planetary Systems Composite Data from the "
        "NASA exoplanets archive (https://exoplanetarchive.ipac.caltech.edu/)",
        _inputDataPath, _outputBinPath
    );
}

void ExoplanetsDataPreparationTask::perform(
                                           const Task::ProgressCallback& progressCallback)
{
    std::ifstream inputDataFile(_inputDataPath);
    if (!inputDataFile.good()) {
        LERROR(std::format("Failed to open input file '{}'", _inputDataPath));
        return;
    }

    std::ofstream binFile(_outputBinPath, std::ios::out | std::ios::binary);
    std::ofstream lutFile(_outputLutPath);

    if (!binFile.good()) {
        LERROR(std::format("Error when writing to '{}'",_outputBinPath));
        if (!std::filesystem::is_directory(_outputBinPath.parent_path())) {
            LERROR("Output directory does not exist");
        }
        return;
    }

    if (!lutFile.good()) {
        LERROR(std::format("Error when writing to '{}'", _outputLutPath));
        if (!std::filesystem::is_directory(_outputLutPath.parent_path())) {
            LERROR("Output directory does not exist");
        }
        return;
    }

    int version = 1;
    binFile.write(reinterpret_cast<char*>(&version), sizeof(int));

    // Read until the first line contaning the column names, and save them for
    // later access
    const std::vector<std::string> columnNames = readFirstDataRow(inputDataFile);

    // Read total number of items
    int total = 0;
    std::string row;
    while (ghoul::getline(inputDataFile, row)) {
        ++total;
    }
    inputDataFile.clear();
    inputDataFile.seekg(0);

    // The reading is restarted, so we need to read past the first line,
    // containing the data names, again
    readFirstDataRow(inputDataFile);

    LINFO(std::format("Loading {} exoplanets", total));

    int exoplanetCount = 0;
    while (ghoul::getline(inputDataFile, row)) {
        ++exoplanetCount;
        progressCallback(static_cast<float>(exoplanetCount) / static_cast<float>(total));

        PlanetData planetData = parseDataRow(
            row,
            columnNames,
            _inputSpeckPath,
            _teffToBvFilePath
        );

        // Create look-up table
        const long pos = static_cast<long>(binFile.tellp());
        const std::string planetName = planetData.host + " " + planetData.component;
        lutFile << planetName << "," << pos << '\n';

        binFile.write(
            reinterpret_cast<char*>(&planetData.dataEntry),
            sizeof(ExoplanetDataEntry)
        );
    }

    progressCallback(1.f);
}

std::vector<std::string>
ExoplanetsDataPreparationTask::readFirstDataRow(std::ifstream& file)
{
    std::string line;

    // Read past any comments and empty lines
    while (ghoul::getline(file, line)) {
        const bool shouldSkip = line.empty() || line[0] == '#';
        if (!shouldSkip) {
            break;
        }
    }

    // The identified line should contain the column names. Return them!
    std::vector<std::string> columnNames;
    std::stringstream sStream(line);
    std::string colName;
    while (ghoul::getline(sStream, colName, ',')) {
        columnNames.push_back(colName);
    }

    return columnNames;
}

ExoplanetsDataPreparationTask::PlanetData
ExoplanetsDataPreparationTask::parseDataRow(const std::string& row,
                                            const std::vector<std::string>& columnNames,
                                          const std::filesystem::path& positionSourceFile,
                                    const std::filesystem::path& bvFromTeffConversionFile)
{
    auto readFloatData = [](const std::string& str) -> float {
#ifdef WIN32
        float result;
        auto [p, ec] = std::from_chars(str.data(), str.data() + str.size(), result);
        if (ec == std::errc()) {
            return result;
        }
        return std::numeric_limits<float>::quiet_NaN();
#else
        // clang is missing float support for std::from_chars
        return !str.empty() ? std::stof(str, nullptr) : NAN;
#endif
};

    auto readDoubleData = [](const std::string& str) -> double {
#ifdef WIN32
        double result;
        auto [p, ec] = std::from_chars(str.data(), str.data() + str.size(), result);
        if (ec == std::errc()) {
            return result;
        }
        return std::numeric_limits<double>::quiet_NaN();
#else
        // clang is missing double support for std::from_chars
        return !str.empty() ? std::stod(str, nullptr) : NAN;
#endif
    };

    auto readIntegerData = [](const std::string& str) -> int {
        int result = 0;
        auto [p, ec] = std::from_chars(str.data(), str.data() + str.size(), result);
        if (ec == std::errc()) {
            return result;
        }
        return -1;
    };

    auto readStringData = [](const std::string& str) -> std::string {
        std::string result = str;
        result.erase(std::remove(result.begin(), result.end(), '\"'), result.end());
        return result;
    };

    float ra = std::numeric_limits<float>::quiet_NaN(); // decimal degrees
    float dec = std::numeric_limits<float>::quiet_NaN(); // decimal degrees
    float distanceInParsec = std::numeric_limits<float>::quiet_NaN();

    std::istringstream lineStream(row);
    int columnIndex = 0;

    ExoplanetDataEntry p;
    std::string component;
    std::string starName;
    std::string name;

    std::string data;
    while (ghoul::getline(lineStream, data, ',')) {
        const std::string& column = columnNames[columnIndex];
        columnIndex++;

        if (column == "pl_letter") {
            component = readStringData(data);
        }
        else if (column == "pl_name") {
            name = readStringData(data);
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
            starName = readStringData(data);
            glm::vec3 position = starPosition(starName, positionSourceFile);
            p.positionX = position[0];
            p.positionY = position[1];
            p.positionZ = position[2];
        }
        else if (column == "ra") {
            ra = readFloatData(data);
        }
        else if (column == "dec") {
            dec = readFloatData(data);
        }
        else if (column == "sy_dist") {
            distanceInParsec = readFloatData(data);
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
        // Effective temperature and color of star
        // (B-V color index computed from star's effective temperature)
        else if (column == "st_teff") {
            p.teff = readFloatData(data);
            p.bmv = bvFromTeff(p.teff, bvFromTeffConversionFile);
        }
        else if (column == "st_tefferr1") {
            p.teffUpper = readFloatData(data);
        }
        else if (column == "st_tefferr2") {
            p.teffLower = -readFloatData(data);
        }
        // Star luminosity
        else if (column == "st_lum") {
            const float dataInLogSolar = readFloatData(data);
            p.luminosity = static_cast<float>(std::pow(10, dataInLogSolar));
        }
        else if (column == "st_lumerr1") {
            const float dataInLogSolar = readFloatData(data);
            p.luminosityUpper = static_cast<float>(std::pow(10, dataInLogSolar));
        }
        else if (column == "st_lumerr2") {
            const float dataInLogSolar = readFloatData(data);
            p.luminosityLower = static_cast<float>(-std::pow(10, dataInLogSolar));
        }
        // Is the planet orbiting a binary system?
        else if (column == "cb_flag") {
            p.binary = readIntegerData(data) != 0;
        }
        // Number of stars in the system
        else if (column == "sy_snum") {
            p.nStars = readIntegerData(data);
        }
        // Number of planets in the system
        else if (column == "sy_pnum") {
            p.nPlanets = readIntegerData(data);
        }
    }

    // @TODO (emmbr 2020-10-05) Currently, the dataset has no information about the
    // longitude of the ascending node, but maybe it might in the future
    p.bigOmega = std::numeric_limits<float>::quiet_NaN();
    p.bigOmegaUpper = std::numeric_limits<float>::quiet_NaN();
    p.bigOmegaLower = std::numeric_limits<float>::quiet_NaN();

    const bool foundPositionFromSpeck = !std::isnan(p.positionX);
    const bool hasDistance = !std::isnan(distanceInParsec);
    const bool hasIcrsCoords = !std::isnan(ra) && !std::isnan(dec) && hasDistance;

    if (!foundPositionFromSpeck && hasIcrsCoords) {
        const glm::dvec3 pos = icrsToGalacticCartesian(ra, dec, distanceInParsec);
        p.positionX = static_cast<float>(pos.x);
        p.positionY = static_cast<float>(pos.y);
        p.positionZ = static_cast<float>(pos.z);
    }

    return {
        .host = starName,
        .name = name,
        .component = component,
        .dataEntry = p
    };
}

glm::vec3 ExoplanetsDataPreparationTask::starPosition(const std::string& starName,
                                                  const std::filesystem::path& sourceFile)
{
    glm::vec3 position = glm::vec3(std::numeric_limits<float>::quiet_NaN());

    if (sourceFile.empty()) {
        // No file specified => return NaN position
        return position;
    }

    std::ifstream exoplanetsFile(sourceFile);
    if (!exoplanetsFile) {
        LERROR(std::format("Error opening file '{}'", sourceFile));
    }

    std::string line;
    while (ghoul::getline(exoplanetsFile, line)) {
        const bool shouldSkipLine = (
            line.empty() || line[0] == '#' || line.substr(0, 7) == "datavar" ||
            line.substr(0, 10) == "texturevar" || line.substr(0, 7) == "texture"
        );

        if (shouldSkipLine) {
            continue;
        }

        std::string data;
        std::string name;
        std::istringstream linestream(line);
        ghoul::getline(linestream, data, '#');
        ghoul::getline(linestream, name);
        name.erase(0, 1);

        std::string coord;
        if (name == starName) {
            std::stringstream dataStream(data);
            ghoul::getline(dataStream, coord, ' ');
            position[0] = std::stof(coord, nullptr);
            ghoul::getline(dataStream, coord, ' ');
            position[1] = std::stof(coord, nullptr);
            ghoul::getline(dataStream, coord, ' ');
            position[2] = std::stof(coord, nullptr);
            break;
        }
    }

    return position;
}

float ExoplanetsDataPreparationTask::bvFromTeff(float teff,
                                              const std::filesystem::path& conversionFile)
{
    if (std::isnan(teff)) {
        return std::numeric_limits<float>::quiet_NaN();
    }

    std::ifstream teffToBvFile(conversionFile);
    if (!teffToBvFile.good()) {
        LERROR(std::format("Failed to open file '{}'", conversionFile));
        return std::numeric_limits<float>::quiet_NaN();
    }

    // Find the line in the file that most closely corresponds to the specified teff,
    // and finally interpolate the value
    float bv = 0.f;
    float bvUpper = 0.f;
    float bvLower = 0.f;
    float teffLower = 0.f;
    float teffUpper = 0.f;
    std::string row;
    while (ghoul::getline(teffToBvFile, row)) {
        std::istringstream lineStream(row);
        std::string teffString;
        ghoul::getline(lineStream, teffString, ',');
        std::string bvString;
        ghoul::getline(lineStream, bvString);

        const float teffCurrent = std::stof(teffString, nullptr);
        const float bvCurrent = std::stof(bvString, nullptr);

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
                const float bvDiff = (bvUpper - bvLower);
                const float teffDiff = (teffUpper - teffLower);
                bv = ((bvDiff * (teff - teffLower)) / teffDiff) + bvLower;
            }
            break;
        }
    }
    return bv;
}

} // namespace openspace::exoplanets
