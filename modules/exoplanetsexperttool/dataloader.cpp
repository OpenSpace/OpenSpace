/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>
#include <charconv>
#include <cmath>
#include <fstream>

namespace {
    constexpr const char _loggerCat[] = "ExoplanetsDataLoader";

    // @TODO: naturally, this path should not be hardcoded
    std::string DataPath = "${MODULES}/exoplanetsexperttool/data/exoplanetsdata.csv";

    constexpr const double EarthMass = 5.972e24; // kg
    constexpr const double EarthRadius = 6.3781e6; // meter
} // namespace

namespace openspace::exoplanets {

DataLoader::DataLoader() : _inExoplanetsCsvPath(absPath(DataPath).string()) {}

std::vector<ExoplanetItem> DataLoader::loadData() {
    std::ifstream exoplanetsCsvFile(_inExoplanetsCsvPath);
    if (!exoplanetsCsvFile.good()) {
        LERROR(fmt::format("Failed to open input file '{}'", _inExoplanetsCsvPath));
        return std::vector<ExoplanetItem>();
    }

    LINFO("Reading Exoplanets CSV");

    bool includeFirstLine = true;
    std::vector<std::vector<std::string>> csvContent = ghoul::loadCSVFile(
        _inExoplanetsCsvPath,
        includeFirstLine
    );

    if (csvContent.empty()) {
        LERROR(
            fmt::format("Could not read CSV data from file '{}'", _inExoplanetsCsvPath)
        );
        return std::vector<ExoplanetItem>();
    }

    // Write exoplanet records to file
   std::vector<std::string> columns = csvContent[0];

   const int nRows = static_cast<int>(csvContent.size());

   std::vector<ExoplanetItem> planets;
   planets.reserve(nRows);

   int idCounter = 0;

   for (int row = 1; row < nRows; row++) {
       ExoplanetItem p;
       std::string name;
       std::string component;
       std::string hostStar;

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
           // Planet properties
           else if (column == "pl_rade") {
               p.radius.value = data::parseFloatData(data);
           }
           else if (column == "pl_masse") {
               p.mass.value = data::parseFloatData(data);
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
           else if (column == "sy_jmag") {
               p.magnitudeJ.value = data::parseFloatData(data);
           }
           else if (column == "sy_kmag") {
               p.magnitudeK.value = data::parseFloatData(data);
           }
           // System properties
           else if (column == "sy_snum") {
               p.nStars = data::parseFloatData(data);
           }
           else if (column == "sy_pnum") {
               p.nPlanets = data::parseFloatData(data);
           }
           else if (column == "disc_year") {
               p.discoveryYear = data::parseFloatData(data);
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
       }

       p.multiSystemFlag = (p.nPlanets > 1);

       // Compute galactic position of system
       bool hasPos = p.ra.hasValue() && p.dec.hasValue() && p.distance.hasValue();
       if (hasPos) {
           const float ra = p.ra.value;
           const float dec = p.dec.value;
           p.position = icrsToGalacticCartesian(ra, dec, p.distance.value);
       }

       // If uknown, compute planet mass
       if ((!p.mass.hasValue()) && p.radius.hasValue()) {
           float r = p.radius.value;

           // Mass radius relationship from Chen & Kipping (2017)
           // See eq. (2) in https://arxiv.org/pdf/1805.03671.pdf

           if (r < 1.23f) { // Terran
               p.mass.value = 0.9718f * glm::pow(r, 3.58f);
           }
           else if (r < 14.26) { // Neptunian
               p.mass.value = 1.436f * glm::pow(r, 1.70f);
           }
           // TODO: constant for larger planets (Jovian & Stellar)
           // Use their python package!
           // Their paper: https://iopscience.iop.org/article/10.3847/1538-4357/834/1/17
       }

       // Compute planet equilibrium temperature according to eq. (3) in
       // https://arxiv.org/pdf/1805.03671.pdf
       bool hasStarTempInfo = p.starEffectiveTemp.hasValue() && p.starRadius.hasValue();
       if (hasStarTempInfo && p.semiMajorAxis.hasValue()) {
           float tempStar = p.starEffectiveTemp.value;
           float rStar = p.starRadius.value;
           float a = p.semiMajorAxis.value;
           a *= 214.93946938362f; // convert to Solar radii (same as star)

           const float c = glm::pow(0.25, 0.25);
           p.eqilibriumTemp.value = c * tempStar * glm::sqrt((rStar / a));
       }

       // @TODO: compute transmission and emission metrics (TSM and ESM)
       // (eq. 1 and 4 in https://arxiv.org/pdf/1805.03671.pdf)
       p.tsm = computeTSM(p);
       p.esm = computeESM(p);

       if (p.radius.hasValue() && p.mass.hasValue()) {
           constexpr const double G = 6.67430e-11;
           const double r = static_cast<double>(p.radius.value) * EarthRadius;
           const double M = static_cast<double>(p.mass.value) * EarthMass;
           p.surfaceGravity.value = static_cast<float>((G * M) / (r * r));
       }

       p.id = idCounter;
       idCounter++;

       planets.push_back(p);
   }

   planets.shrink_to_fit();
   return planets;
}

float DataLoader::computeTSM(const ExoplanetItem& p) {
    bool hasMissingValue = !(p.radius.hasValue()
        && p.mass.hasValue() && p.eqilibriumTemp.hasValue()
        && p.starRadius.hasValue() && p.magnitudeJ.hasValue());

    if (hasMissingValue) {
        return std::numeric_limits<float>::quiet_NaN();
    }

    // Scale factor based on table 1 in Kempton et al. (2018)
    // planetRadius is in Earth radii
    auto scaleFactor = [](double planetRadius) {
        if (planetRadius < 1.5) {
            return 0.19;
        }
        else if (planetRadius < 2.75) {
            return 1.26;
        }
        else if (planetRadius < 4.0) {
            return 1.28;
        }
        else {
            // 4.0 < r  < 10 EarthRadius
            // but we use the same scale factor for larger planets
            return 1.15;
        }
    };

    const double rPlanet = static_cast<double>(p.radius.value);
    const double mass = static_cast<double>(p.mass.value);
    const double temp = static_cast<double>(p.eqilibriumTemp.value);
    const double rStar = static_cast<double>(p.starRadius.value);
    const double mJ = static_cast<double>(p.magnitudeJ.value);

    const double rPlanet3 = rPlanet * rPlanet * rPlanet;
    const double rStar2 = rStar * rStar;

    double tsm = (rPlanet3 * temp) / (mass * rStar2);
    tsm *= glm::pow(10.0, -mJ / 5.0);
    tsm *= scaleFactor(rPlanet);

    return static_cast<float>(tsm);
}

float DataLoader::computeESM(const ExoplanetItem& p) {
    bool hasMissingValue = !(p.radius.hasValue()
        && p.eqilibriumTemp.hasValue() && p.starEffectiveTemp.hasValue()
        && p.starRadius.hasValue() && p.magnitudeK.hasValue());

    if (hasMissingValue) {
        return std::numeric_limits<float>::quiet_NaN();
    }

    const double rPlanet = static_cast<double>(p.radius.value);
    const double tempPlanetDay = 1.10 * static_cast<double>(p.eqilibriumTemp.value);
    const double rStar = static_cast<double>(p.starRadius.value);
    const double teffStar = static_cast<double>(p.starEffectiveTemp.value);
    const double mK = static_cast<double>(p.magnitudeK.value);

    constexpr const double earthToSolar = 0.0091577;
    const double normalizedPlanetRadius = (rPlanet * earthToSolar) / rStar;

    // Plack's function computes the energy emitted per second per unit lambda wavelength
    // per steradian from one square meter of a perfect blackbody at temperature T
    // http://spiff.rit.edu/classes/phys317/lectures/planck.html
    auto plancksFunction = [](double T, double lambda) {
        constexpr const double h = 6.62607015e-34; // Planck's constant
        constexpr const double c = 299792458; // Speed of light
        constexpr const double k = 1.38064852e-23; // Boltzmann's constant

        const double nom = 2.0 * h * c * c / std::pow(lambda, 5.0);
        const double denom = std::exp((h * c) / (lambda * k * T)) - 1.0;
        return nom / denom;
    };

    const double lambda = 7.5 * 10e-6; // micrometer

    double esm = 4.29 * 10e6;
    esm *= plancksFunction(tempPlanetDay, lambda) / plancksFunction(teffStar, lambda);
    esm *= std::pow(normalizedPlanetRadius, 2.0);
    esm *= std::pow(10.0, -mK / 5.0);
    return static_cast<float>(esm);
}

} // namespace openspace::exoplanets
