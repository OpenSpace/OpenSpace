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

#include <modules/exoplanets/exoplanetshelper.h>

#include <openspace/util/spicemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/fmt.h>
#include <ghoul/logging/logmanager.h>
#include <glm/gtx/quaternion.hpp>
#include <glm/gtx/transform.hpp>
#include <string_view>
#include <fstream>
#include <sstream>

namespace {
    constexpr const char* _loggerCat = "ExoplanetsModule";

    constexpr const char* BvColormapPath = "${SYNC}/http/stars_colormap/2/colorbv.cmap";
}

namespace openspace::exoplanets {

std::string_view speckStarName(std::string_view csvName) {
    if (csvName == "MOA-2009-BLG-387L") { return "MOA 2009-BLG-387L"; }
    if (csvName == "OGLE-2007-BLG-368L") { return "OGLE 2007-BLG-368L"; }
    if (csvName == "OGLE-2005-BLG-169L") { return "OGLE 2005-BLG-169L"; }
    if (csvName == "OGLE-2005-BLG-071L") { return "OGLE 2005-BLG-71L"; }
    if (csvName == "OGLE-2003-BLG-235L") { return "OGLE 2003-BLG-235L"; }
    if (csvName == "MOA-2008-BLG-310L") { return "MOA 2008-BLG-310L"; }
    if (csvName == "OGLE-2006-BLG-109L") { return "OGLE 2006-BLG-109L"; }
    if (csvName == "HD 137388") { return "HD 137388 A"; }
    if (csvName == "MOA-2010-BLG-477L") { return "MOA 2010-BLG-477L"; }
    if (csvName == "MOA-2009-BLG-266L") { return "MOA 2009-BLG-266L"; }
    if (csvName == "iot Dra") { return "HIP 75458"; }
    if (csvName == "MOA-2007-BLG-400L") { return "MOA 2007-BLG-400L"; }
    if (csvName == "OGLE-2011-BLG-0251L") { return "OGLE 2011-BLG-251L"; }
    if (csvName == "OGLE-2005-BLG-390L") { return "OGLE 2005-BLG-390L"; }
    if (csvName == "MOA-2007-BLG-192L") { return "MOA 2007-BLG-192L"; }
    if (csvName == "MOA-2009-BLG-319L") { return "MOA 2009-BLG-319L"; }
    return csvName;
}

std::string_view csvStarName(std::string_view name) {
    if (name == "MOA 2009-BLG-387L") { return "MOA-2009-BLG-387L"; }
    if (name == "OGLE 2007-BLG-368L") { return "OGLE-2007-BLG-368L"; }
    if (name == "OGLE 2005-BLG-169L") { return "OGLE-2005-BLG-169L"; }
    if (name == "OGLE 2005-BLG-71L") { return "OGLE-2005-BLG-071L"; }
    if (name == "OGLE 2003-BLG-235L") { return "OGLE-2003-BLG-235L"; }
    if (name == "MOA 2008-BLG-310L") { return "MOA-2008-BLG-310L"; }
    if (name == "OGLE 2006-BLG-109L") { return "OGLE-2006-BLG-109L"; }
    if (name == "HD 137388 A") { return "HD 137388"; }
    if (name == "MOA 2010-BLG-477L") { return "MOA-2010-BLG-477L"; }
    if (name == "MOA 2009-BLG-266L") { return "MOA-2009-BLG-266L"; }
    if (name == "HIP 75458") { return "iot Dra"; }
    if (name == "MOA 2007-BLG-400L") { return "MOA-2007-BLG-400L"; }
    if (name == "OGLE 2011-BLG-251L") { return "OGLE-2011-BLG-0251L"; }
    if (name == "OGLE 2005-BLG-390L") { return "OGLE-2005-BLG-390L"; }
    if (name == "MOA 2007-BLG-192L") { return "MOA-2007-BLG-192L"; }
    if (name == "MOA 2009-BLG-319L") { return "MOA-2009-BLG-319L"; }
    return name;
}

bool hasSufficientData(const Exoplanet& p) {
    bool invalidPos = std::isnan(p.positionX)
        || std::isnan(p.positionY)
        || std::isnan(p.positionZ);

    bool hasSemiMajorAxis = !std::isnan(p.a);
    bool hasOrbitalPeriod = !std::isnan(p.per);

    return !invalidPos && hasSemiMajorAxis && hasOrbitalPeriod;
}

std::string starColor(float bv) {
    std::ifstream colorMap(absPath(BvColormapPath), std::ios::in);

    if (!colorMap.good()) {
        LERROR(fmt::format(
            "Failed to open colormap data file: '{}'",
            absPath(BvColormapPath)
        ));
        return "";
    }

    const int t = static_cast<int>(round(((bv + 0.4) / (2.0 + 0.4)) * 255));
    std::string color;
    for (int i = 0; i < t + 12; i++) {
        getline(colorMap, color);
    }
    colorMap.close();

    std::istringstream colorStream(color);
    std::string r, g, b;
    getline(colorStream, r, ' ');
    getline(colorStream, g, ' ');
    getline(colorStream, b, ' ');

    return fmt::format("{{ {}, {}, {} }}", r, g, b);
}

glm::dmat4 computeOrbitPlaneRotationMatrix(float i, float bigom, float omega) {
    // Exoplanet defined inclination changed to be used as Kepler defined inclination
    const glm::dvec3 ascendingNodeAxisRot = glm::dvec3(0.0, 0.0, 1.0);
    const glm::dvec3 inclinationAxisRot = glm::dvec3(1.0, 0.0, 0.0);
    const glm::dvec3 argPeriapsisAxisRot = glm::dvec3(0.0, 0.0, 1.0);

    const double asc = glm::radians(bigom);
    const double inc = glm::radians(i);
    const double per = glm::radians(omega);

    const glm::dmat4 orbitPlaneRotation =
        glm::rotate(asc, glm::dvec3(ascendingNodeAxisRot)) *
        glm::rotate(inc, glm::dvec3(inclinationAxisRot)) *
        glm::rotate(per, glm::dvec3(argPeriapsisAxisRot));

    return orbitPlaneRotation;
}

glm::dmat3 computeSystemRotation(glm::dvec3 starPosition) {
    const glm::dvec3 sunPosition = glm::dvec3(0.0, 0.0, 0.0);
    const glm::dvec3 starToSunVec = glm::normalize(sunPosition - starPosition);
    const glm::dvec3 galacticNorth = glm::dvec3(0.0, 0.0, 1.0);

    const glm::dmat3 galacticToCelestialMatrix =
        SpiceManager::ref().positionTransformMatrix("GALACTIC", "J2000", 0.0);

    const glm::dvec3 celestialNorth = glm::normalize(
        galacticToCelestialMatrix * galacticNorth
    );

    // Earth's north vector projected onto the skyplane, the plane perpendicular to the
    // viewing vector (starToSunVec)
    const float celestialAngle = static_cast<float>(glm::dot(
        celestialNorth,
        starToSunVec
    ));
    glm::dvec3 northProjected = glm::normalize(
        celestialNorth - (celestialAngle / glm::length(starToSunVec)) * starToSunVec
    );

    const glm::dvec3 beta = glm::normalize(glm::cross(starToSunVec, northProjected));

    return glm::dmat3(
        northProjected.x,
        northProjected.y,
        northProjected.z,
        beta.x,
        beta.y,
        beta.z,
        starToSunVec.x,
        starToSunVec.y,
        starToSunVec.z
    );
}

std::string createIdentifier(std::string name) {
    std::replace(name.begin(), name.end(), ' ', '_');
    std::replace(name.begin(), name.end(), '.', '-');
    sanitizeNameString(name);
    return name;
}

void sanitizeNameString(std::string& s) {
    // We want to avoid quotes and apostrophes in names, since they cause problems
    // when a string is translated to a script call
    s.erase(remove(s.begin(), s.end(), '\"'), s.end());
    s.erase(remove(s.begin(), s.end(), '\''), s.end());
}

} // namespace openspace::exoplanets
