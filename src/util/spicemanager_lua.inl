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

#include <ghoul/misc/stringhelper.h>

namespace {

/**
 * Loads the provided SPICE kernel by name. The name can contain path tokens, which are
 * automatically resolved.
 */
[[codegen::luawrap]] void loadKernel(
                               std::variant<std::string, std::vector<std::string>> kernel)
{
    if (std::holds_alternative<std::string>(kernel)) {
        std::string k = std::get<std::string>(kernel);
        if (!std::filesystem::is_regular_file(k)) {
            throw ghoul::lua::LuaError(std::format("Kernel file '{}' did not exist", k));
        }
        openspace::SpiceManager::ref().loadKernel(k);
    }
    else {
        std::vector<std::string> ks = std::get<std::vector<std::string>>(kernel);
        for (const std::string& k : ks) {
            if (!std::filesystem::is_regular_file(k)) {
                throw ghoul::lua::LuaError(std::format(
                    "Kernel file '{}' did not exist", k
                ));
            }
            openspace::SpiceManager::ref().loadKernel(k);
        }
    }
}

/**
 * Unloads the provided SPICE kernel. The name can contain path tokens, which are
 * automatically resolved.
 */
[[codegen::luawrap]] void unloadKernel(
                               std::variant<std::string, std::vector<std::string>> kernel)
{
    if (std::holds_alternative<std::string>(kernel)) {
        openspace::SpiceManager::ref().unloadKernel(std::get<std::string>(kernel));
    }
    else {
        for (const std::string& k : std::get<std::vector<std::string>>(kernel)) {
            openspace::SpiceManager::ref().unloadKernel(k);
        }
    }
}

/**
 * Returns a list of all loaded kernels
 */
[[codegen::luawrap]] std::vector<std::string> kernels() {
    return openspace::SpiceManager::ref().loadedKernels();
}

/**
 * Returns a list of Spice Bodies loaded into the system. Returns SPICE built in frames if
 * builtInFrames. Returns User loaded frames if !builtInFrames.
 */
[[codegen::luawrap]] std::map<std::string, std::string> spiceBodies(bool includeBuiltIn) {
    std::vector<std::pair<int, std::string>> bodies =
        openspace::SpiceManager::ref().spiceBodies(includeBuiltIn);

    std::map<std::string, std::string> res;
    for (const std::pair<int, std::string>& p : bodies) {
        res[std::to_string(p.first)] = p.second;
    }
    return res;
}

/**
 * Returns the rotationMatrix for a given body in a frame of reference at a specific time.
 * Example:
 * openspace.spice.rotationMatrix('INSIGHT_LANDER_CRUISE','MARS', '2018 NOV 26 19:45:34')
 */
[[codegen::luawrap]] glm::dmat3 rotationMatrix(std::string body, std::string frame,
                                               std::string date)
{
    using namespace openspace;

    const double ephemerisTime = SpiceManager::ref().ephemerisTimeFromDate(date);
    glm::dmat3 rotationMatrix = SpiceManager::ref().frameTransformationMatrix(
        body,
        frame,
        ephemerisTime
    );
    return rotationMatrix;
}

/**
 * Returns the position for a given body relative to another body, in a given frame of
 * reference, at a specific time.
 * Example:
 * openspace.spice.position('INSIGHT', 'MARS',' GALACTIC', '2018 NOV 26 19:45:34')
 */
[[codegen::luawrap]] glm::dvec3 position(std::string target, std::string observer,
                                         std::string frame, std::string date)
{
    using namespace openspace;

    const double ephemerisTime = SpiceManager::ref().ephemerisTimeFromDate(date);
    glm::dvec3 position = SpiceManager::ref().targetPosition(
        target,
        observer,
        frame,
        {},
        ephemerisTime
    );
    return position;
}

/**
 * This function converts a TLE file into SPK format and saves it at the provided path.
 * The last parameter is only used if there are multiple craft specified in the provided
 * TLE file and is selecting which (0-based index) of the list to create a kernel from.
 *
 * This function returns the SPICE ID of the object for which the kernel was created
 */
[[codegen::luawrap]] int convertTLEtoSPK(std::filesystem::path tle,
                                         std::filesystem::path spk,
                                         int elementToExtract = 0)
{
    // Code adopted from
    // https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/getelm_c.html
    SpiceInt n = 0;

    //
    // First exact the constants required by a type 10 SPK kernel.
    //

    std::array<double, 8> constants;
    // J2 gravitational harmonic for Earth
    bodvcd_c(399, "J2", 1, &n, &constants[0]);

    // J3 gravitational harmonic for Earth
    bodvcd_c(399, "J3", 1, &n, &constants[1]);

    // J4 gravitational harmonic for Earth
    bodvcd_c(399, "J4", 1, &n, &constants[2]);

    // Square root of the GM for Earth
    bodvcd_c(399, "KE", 1, &n, &constants[3]);

    // High altitude bound for atmospheric model
    bodvcd_c(399, "QO", 1, &n, &constants[4]);

    // Low altitude bound for atmospheric model
    bodvcd_c(399, "SO", 1, &n, &constants[5]);

    // Equatorial radius of the Earth
    bodvcd_c(399, "ER", 1, &n, &constants[6]);

    // Distance units/earth radius
    bodvcd_c(399, "AE", 1, &n, &constants[7]);

    //
    // Load the TLE file
    //
    std::ifstream f = std::ifstream(tle);
    std::string contents = std::string(
        std::istreambuf_iterator<char>(f),
        std::istreambuf_iterator<char>()
    );

    // The TLE files returned by Celestrak are of the 3-line variant where the first line
    // contains a human-readable name for the spacecraft

    std::vector<std::string> lines = ghoul::tokenizeString(contents, '\n');
    const size_t nElements = lines.size() / 3;
    if (elementToExtract > nElements) {
        throw ghoul::RuntimeError(std::format(
            "Error loading {}. Element number {} requested, but only {} found",
            tle, nElements, elementToExtract
        ));
    }

    constexpr int TLEColumnWidth = 70;

    // It should be 70, but we're removing the \n character at the end in the tokenization
    std::string line1 = lines[3 * elementToExtract + 1];
    if (line1.size() != TLEColumnWidth - 1) {
        throw ghoul::RuntimeError(std::format(
            "Illformed TLE file {}, expected {} characters per line, got {}",
            tle, TLEColumnWidth, line1.size()
        ));
    }
    std::string line2 = lines[3 * elementToExtract + 2];
    if (line2.size() != TLEColumnWidth - 1) {
        throw ghoul::RuntimeError(std::format(
            "Illformed TLE file {}, expected {} characters per line, got {}",
            tle, TLEColumnWidth, line2.size()
        ));
    }

    // Copy the lines into a format that SPICE understands
    SpiceChar spiceLines[2][TLEColumnWidth];
    std::strcpy(spiceLines[0], line1.c_str());
    std::strcpy(spiceLines[1], line2.c_str());


    // Convert the Two Line Elements lines to the element sets
    SpiceDouble epoch;
    std::array<SpiceDouble, 10> elems;
    getelm_c(1950, TLEColumnWidth, spiceLines, &epoch, elems.data());

    // The size of a type SPK10 spice kernel is not affected by the time validity, so we
    // just pick the greatest one
    SpiceDouble first = -std::numeric_limits<double>::max();
    SpiceDouble last = std::numeric_limits<double>::max();

    // Extract the body id
    std::vector<std::string> tokens = ghoul::tokenizeString(line2, ' ');
    if (tokens.size() < 2) {
        throw ghoul::RuntimeError(std::format(
            "Error parsing TLE file {}. Expected 8-9 elements in the second row, got {}",
            tle, tokens.size()
        ));
    }
    // Earth-orbiting spacecraft usually lack a DSN identification code, so the NAIF ID
    // is derived from the tracking ID assigned to it by NORAD via:
    //   NAIF ID = -100000 - NORAD ID
    int bodyId = -100000 - std::stoi(tokens[1]);


    // Write the elements to a new SPK file
    const SpiceInt nCommentCharacters = 0;
    std::string internalFileName = std::format("Type 10 SPK for {}", tle);
    std::string segmentId = "Segment";

    if (std::filesystem::exists(spk)) {
        std::filesystem::remove(spk);
    }

    std::string outFile = spk.string();
    SpiceInt handle;
    spkopn_c(outFile.c_str(), internalFileName.c_str(), nCommentCharacters, &handle);

    spkw10_c(
        handle,
        bodyId,
        399,
        "J2000",
        first,
        last,
        segmentId.c_str(),
        constants.data(),
        1,
        elems.data(),
        &epoch
    );

    spkcls_c(handle);

    return bodyId;
}

#include "spicemanager_lua_codegen.cpp"

} // namespace
