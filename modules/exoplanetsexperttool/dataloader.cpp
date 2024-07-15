/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#include <modules/exoplanetsexperttool/dataloader.h>

#include <modules/exoplanetsexperttool/datahelper.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/util/coordinateconversion.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/csvreader.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/stringhelper.h>
#include <ghoul/logging/logmanager.h>
#include <json/json.hpp>
#include <scn/scan.h>
#include <charconv>
#include <cmath>
#include <fstream>
#include <limits>
#include <string>
#include <string_view>

namespace {
    constexpr const char _loggerCat[] = "ExoplanetsDataLoader";

    // @TODO: naturally, this path should not be hardcoded
    std::string DataPath = "${MODULES}/exoplanetsexperttool/data/aggregated_data.csv";

    constexpr const double EarthMass = 5.972e24; // kg
    constexpr const double EarthRadius = 6.3781e6; // meter

    bool hasEnding(std::string const& fullString, std::string const& ending) {
        if (fullString.length() >= ending.length()) {
            int comp = fullString.compare(
                fullString.length() - ending.length(),
                ending.length(),
                ending
            );
            return (0 == comp);
        }
        else {
            return false;
        }
    }

    bool isNumber(const std::string& s) {
        return !s.empty() && std::find_if(s.begin(),
            s.end(), [](unsigned char c) { return !std::isdigit(c); }) == s.end();
    }

} // namespace

namespace openspace::exoplanets {

DataLoader::DataLoader() : _inExoplanetsCsvPath(absPath(DataPath).string()) {}

std::vector<ExoplanetItem> DataLoader::loadData() {
    std::ifstream exoplanetsCsvFile(_inExoplanetsCsvPath);
    if (!exoplanetsCsvFile.good()) {
        LERROR(std::format("Failed to open input file '{}'", _inExoplanetsCsvPath));
        return std::vector<ExoplanetItem>();
    }

    LINFO("Reading Exoplanets CSV");

    std::vector<std::vector<std::string>> csvContent = ghoul::loadCSVFile(
        _inExoplanetsCsvPath,
        true
    );

    if (csvContent.empty()) {
        LERROR(
            std::format("Could not read CSV data from file '{}'", _inExoplanetsCsvPath)
        );
        return std::vector<ExoplanetItem>();
    }

    auto isPositiveErrorCol = [](const std::string& c) {
        return c.find("err1") != std::string::npos;
    };

    auto isNegativeErrorCol = [](const std::string& c) {
        return c.find("err2") != std::string::npos;
    };

    // Write exoplanet records to file
    std::vector<std::string> columns = csvContent[0];

    const int nRows = static_cast<int>(csvContent.size());

    std::vector<ExoplanetItem> planets;
    planets.reserve(nRows);

    int nOtherColumns = -1;

    for (int row = 1; row < nRows; row++) {
        ExoplanetItem p;
        std::string name;
        std::string component;
        std::string hostStar;

        p.id = row - 1;

        for (int col = 0; col < columns.size(); col++) {
            const std::string& column = columns[col];
            const std::string& data = csvContent[row][col];

            if (column == "pl_name") {
                p.planetName = data;
                // TODO: create identifier matching exoplanets module?
            }
            else if (column == "hostname") {
                p.hostName = data;
                // TODO: create identifier matching exoplanets module?
            }
            else if (column == "pl_letter") {
                if (data.size() == 1) {
                    p.component = data.at(0);
                }
                else {
                    LWARNING(std::format(
                        "Could not read planet letter from data: '{}'", data
                    ));
                }
            }
            // Discovery
            else if (column == "disc_year") {
                p.discoveryYear = static_cast<int>(data::parseFloatData(data));
            }
            else if (column == "discoverymethod") {
                p.discoveryMethod = data;
            }
            else if (column == "disc_telescope") {
                p.discoveryTelescope = data;
            }
            else if (column == "disc_instrument") {
                p.discoveryInstrument = data;
            }
            // Planet properties
            else if (column == "pl_rade") {
                p.radius.value = data::parseFloatData(data);
            }
            else if (column.rfind("pl_bmasse") != std::string::npos) {
                // TODO: generalize
                if (isPositiveErrorCol(column)) {
                    p.mass.errorUpper = data::parseFloatData(data);
                }
                else if (isNegativeErrorCol(column)) {
                    p.mass.errorLower = data::parseFloatData(data);
                }
                else if (column == "pl_bmasse") {
                    p.mass.value = data::parseFloatData(data);
                }
            }
            // Orbital properties
            else if (column == "pl_orbsmax") {
                p.semiMajorAxis.value = data::parseFloatData(data);
            }
            else if (column == "pl_orbeccen") {
                p.eccentricity.value = data::parseFloatData(data);
            }
            else if (column == "pl_orbper") {
                p.period.value = data::parseFloatData(data);
            }
            else if (column == "pl_orbincl") {
                p.inclination.value = data::parseFloatData(data);
            }
            else if (column == "pl_Teq") {
                p.eqilibriumTemp.value = data::parseFloatData(data);
            }
            // Star properties
            else if (column == "st_teff") {
                p.starEffectiveTemp.value = data::parseFloatData(data);
            }
            else if (column == "st_rad") {
                p.starRadius.value = data::parseFloatData(data);
            }
            else if (column == "st_age") {
                p.starAge.value = data::parseFloatData(data);
            }
            else if (column == "st_met") {
                p.starMetallicity.value = data::parseFloatData(data);
            }
            else if (column == "st_metratio") {
                p.starMetallicityRatio = data;
            }
            else if (column == "sy_jmag") {
                p.magnitudeJ.value = data::parseFloatData(data);
            }
            else if (column == "sy_kmag") {
                p.magnitudeK.value = data::parseFloatData(data);
            }
            // System properties
            else if (column == "sy_snum") {
                p.nStars = static_cast<int>(data::parseFloatData(data));
            }
            else if (column == "sy_pnum") {
                p.nPlanets = static_cast<int>(data::parseFloatData(data));
            }
            // Position
            else if (column == "ra") {
                p.ra.value = data::parseFloatData(data);
            }
            else if (column == "dec") {
                p.dec.value = data::parseFloatData(data);
            }
            else if (column == "sy_dist") {
                p.distance.value = data::parseFloatData(data);
            }
            else if (column == "ESM") {
                p.esm = data::parseFloatData(data);
            }
            else if (column == "TSM") {
                p.tsm = data::parseFloatData(data);
            }
            // Molecules in atmosphere
            // Note that molecules are separated with '&' signs. We replace those
            else if (column == "molecule_detection") {
                auto molecules = ghoul::tokenizeString(data, '&');
                p.moleculesDetection = ghoul::join(molecules, ", ");
            }
            else if (column == "molecule_upperLimit") {
                auto molecules = ghoul::tokenizeString(data, '&');
                p.moleculesUpperLimit = ghoul::join(molecules, ", ");
            }
            else if (column == "molecule_noDetection") {
                auto molecules = ghoul::tokenizeString(data, '&');
                p.moleculesNoDetection = ghoul::join(molecules, ", ");
            }
            // Data reference
            else if (column == "pl_refname") {
                // Parse reference string
                //
                // TODO: try loading name like this instead
                // scn::scan(data, "<a refstr={} href={} target=ref>{}</a>"
                //
                // For some reason we fail reading the name above. Try and see if splitting up the string helps
                std::vector<std::string> list = ghoul::tokenizeString(data, '>');
                list.pop_back(); // Last is always empty();

                if (list.size() != 2) {
                    LERROR(std::format(
                        "Failed reading reference: '{}' (wrong format)", data)
                    );
                }

                // Read url
                auto res = scn::scan<std::string, std::string>(list[0], "<a refstr={} href={} target=ref");
                if (res) {
                    auto [refstr, href] = res->values();

                }

                // Remove final tag from name
                std::string name = ghoul::replaceAll(list[1], "</a", "");
                ghoul::trimWhitespace(name);
                p.referenceName = name;
            }
            // Any other columns that might be in the datset
            else {
                std::string key = column;

                // For now, ignore any empty, limit and error columns
                if (hasEnding(key, "err1") ||
                    hasEnding(key, "err2") ||
                    hasEnding(key, "lim"))
                {
                    continue;
                }


                float parsedNumeric = data::parseFloatData(data);

                // All columns should have empty string for missing values
                if (data.empty()) {
                    p.otherColumns[key] = ""; // Empty string
                }
                else if (!std::isnan(parsedNumeric)) {
                    p.otherColumns[key] = parsedNumeric;
                }
                else {
                    // Non empty string value
                    p.otherColumns[key] = data;
                }

            }
        }

        p.multiSystemFlag = (p.nPlanets > 1);

        // Compute galactic position of system
        bool hasPos = p.ra.hasValue() && p.dec.hasValue() && p.distance.hasValue();
        if (hasPos) {
            const float ra = p.ra.value;
            const float dec = p.dec.value;
            p.position = icrsToGalacticCartesian(ra, dec, p.distance.value);
        }

        // Check if water has been detected
        // TODO: move to python
        // 1 = yes, 0 = maybe, -1 = no
        constexpr const char WaterKey[] = "H2O";
        if (p.moleculesDetection.find(WaterKey) != std::string::npos) {
            p.waterDetection = 1.f;
        }
        else if (p.moleculesUpperLimit.find(WaterKey) != std::string::npos) {
            p.waterDetection = 0.f;
        }
        else if (p.moleculesNoDetection.find(WaterKey) != std::string::npos) {
            p.waterDetection = -1.f;
        }

        //// If unknown, compute planet mass
        //// TODO: move to python
        //if ((!p.mass.hasValue()) && p.radius.hasValue()) {
        //    float r = p.radius.value;

        //    // Mass radius relationship from Chen & Kipping (2017)
        //    // See eq. (2) in https://arxiv.org/pdf/1805.03671.pdf

        //    if (r < 1.23f) { // Terran
        //        p.mass.value = 0.9718f * glm::pow(r, 3.58f);
        //    }
        //    else if (r < 14.26) { // Neptunian
        //        p.mass.value = 1.436f * glm::pow(r, 1.70f);
        //    }
        //    // TODO: constant for larger planets (Jovian & Stellar)
        //    // Use their python package!
        //    // Their paper: https://iopscience.iop.org/article/10.3847/1538-4357/834/1/17
        //}

        // TODO: move to python
        if (p.radius.hasValue() && p.mass.hasValue()) {
            constexpr const double G = 6.67430e-11;
            const double r = static_cast<double>(p.radius.value) * EarthRadius;
            const double M = static_cast<double>(p.mass.value) * EarthMass;
            p.surfaceGravity.value = static_cast<float>((G * M) / (r * r));
        }

        // Virification related to "other columns"
        if (nOtherColumns == -1) {
            nOtherColumns = static_cast<int>(p.otherColumns.size());
        }
        else {
            if (p.otherColumns.size() != nOtherColumns) {
                throw; // TODO: throw something meaningful
            }
        }

        planets.push_back(p);
    }
    planets.shrink_to_fit();

    if (planets.empty()) {
        return planets;
    }

    // Handle missing values in "other columns"

    std::map<std::string, bool> colIsNumeric;
    auto first = planets.front().otherColumns;

    // Determine the type of all other columns
    for (auto iter = first.begin(); iter != first.end(); ++iter) {
        const std::string key = iter->first;

        // Default value if we have no vlaue for any planet is false
        colIsNumeric[key] = false;

        // Find first entry with a value and use that to determine category
        for (auto p : planets) {
            std::variant<std::string, float> value = p.otherColumns[key];
            if (std::holds_alternative<std::string>(value) &&
                std::get<std::string>(value).empty())
            {
                continue; // do nothing
            }
            else {
                colIsNumeric[key] = std::holds_alternative<float>(value);
                break;
            }
        }
    }

    // Replace all the missing values in the numeric columns with something meaningful
    for (auto iter = first.begin(); iter != first.end(); ++iter) {
        const std::string key = iter->first;

        if (!colIsNumeric[key]) {
            continue;
        }

        // Find first entry with a value and use that to determine category
        for (ExoplanetItem& p : planets) {
            std::variant<std::string, float> value = p.otherColumns[key];
            if (std::holds_alternative<std::string>(value) &&
                std::get<std::string>(value).empty())
            {
                p.otherColumns[key] = std::numeric_limits<float>::quiet_NaN();
            }
        }
    }

    // At this stage all values in the other columns should have the correct type

    return planets;
}

} // namespace openspace::exoplanets
