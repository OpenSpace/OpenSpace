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

#include "kepler.h"

namespace {

/**
 * Returns the cartesian world position of a ra dec coordinate with distance. If the
 * coordinate is given as strings the format should be ra 'XhYmZs' and dec 'XdYmZs'. If
 * the coordinate is given as numbers the values should be in degrees.
 */
[[codegen::luawrap]] glm::dvec3 convertFromRaDec(
                                         std::variant<double, std::string> rightAscension,
                                            std::variant<double, std::string> declination,
                                                                          double distance)
{
    using namespace openspace;

    glm::dvec2 degrees = glm::dvec2(0.0);
    if (std::holds_alternative<double>(rightAscension) &&
        std::holds_alternative<double>(declination))
    {
        degrees = glm::dvec2(
            std::get<double>(rightAscension),
            std::get<double>(declination)
        );
    }
    else if (std::holds_alternative<std::string>(rightAscension) &&
             std::holds_alternative<std::string>(declination))
    {
        degrees = icrsToDecimalDegrees(
            std::get<std::string>(rightAscension),
            std::get<std::string>(declination)
        );
    }
    else {
        throw ghoul::lua::LuaError(
            "Ra and Dec have to be of the same type, either String or Number"
        );
    }

    glm::dvec3 pos = icrsToGalacticCartesian(degrees.x, degrees.y, distance);
    return pos;
}

/**
 * Returns the formatted ra, dec strings and distance for a given cartesian world
 * coordinate.
 */
[[codegen::luawrap]] std::tuple<std::string, std::string, double> convertToRaDec(double x,
                                                                                 double y,
                                                                                 double z)
{
    using namespace openspace;
    glm::dvec3 deg = galacticCartesianToIcrs(x, y, z);
    std::pair<std::string, std::string> raDecPair = decimalDegreesToIcrs(deg.x, deg.y);
    return { raDecPair.first, raDecPair.second, deg.z };
}

[[codegen::luawrap]]
std::vector<ghoul::Dictionary> readKeplerFile(std::filesystem::path p, std::string type)
{
    openspace::kepler::Format f;
    if (type == "TLE") {
        f = openspace::kepler::Format::TLE;
    }
    else if (type == "OMM") {
        f = openspace::kepler::Format::OMM;
    }
    else if (type == "SBDB") {
        f = openspace::kepler::Format::SBDB;
    }
    else {
        throw ghoul::lua::LuaError(std::format("Unsupported format '{}'", type));
    }

    std::vector<openspace::kepler::Parameters> params = openspace::kepler::readFile(p, f);
    std::vector<ghoul::Dictionary> res;
    res.reserve(params.size());
    for (const openspace::kepler::Parameters& param : params) {
        ghoul::Dictionary d;
        d.setValue("Name", param.name);
        d.setValue("ID", param.id);
        d.setValue("inclination", param.inclination);
        d.setValue("SemiMajorAxis", param.semiMajorAxis);
        d.setValue("AscendingNode", param.ascendingNode);
        d.setValue("Eccentricity", param.eccentricity);
        d.setValue("ArgumentOfPeriapsis", param.argumentOfPeriapsis);
        d.setValue("MeanAnomaly", param.meanAnomaly);
        d.setValue("Epoch", param.epoch);
        d.setValue("Period", param.period);
        res.push_back(d);
    }
    return res;
}

#include "spacemodule_lua_codegen.cpp"

} // namespace
