/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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
            throw ghoul::lua::LuaError(fmt::format("Kernel file '{}' did not exist", k));
        }
        openspace::SpiceManager::ref().loadKernel(k);
    }
    else {
        std::vector<std::string> ks = std::get<std::vector<std::string>>(kernel);
        for (const std::string& k : ks) {
            if (!std::filesystem::is_regular_file(k)) {
                throw ghoul::lua::LuaError(fmt::format(
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

#include "spicemanager_lua_codegen.cpp"

} // namespace
