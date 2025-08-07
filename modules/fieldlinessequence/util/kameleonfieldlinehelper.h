/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#ifndef __OPENSPACE_MODULE_FIELDLINESSEQUENCE___KAMELEONFIELDLINEHELPER___H__
#define __OPENSPACE_MODULE_FIELDLINESSEQUENCE___KAMELEONFIELDLINEHELPER___H__

#include <ghoul/glm.h>
#include <string>
#include <unordered_map>
#include <vector>
#include <ccmc/Kameleon.h>

namespace openspace {

class FieldlinesState;

namespace fls {
std::unordered_map<std::string, std::vector<glm::vec3>>
    extractSeedPointsFromFiles(std::filesystem::path);
std::vector<std::string>
    extractMagnitudeVarsFromStrings(std::vector<std::string> extrVars);
void addExtraQuantities(ccmc::Kameleon* kameleon,
    std::vector<std::string>& extraScalarVars, std::vector<std::string>& extraMagVars,
    FieldlinesState& state);
/**
 * Extract seedpoints from a text file. This function is used both in
 * RenderableFieldlinesSequence and the .cdf to .osfls converter task.
 *
 * \param path The path to a directory with files containing list of seedpoints
 * \param nth Is 1 on default to incluse every seedpoint. nth can be used to reduce the
          amount of data produced be only including every nth seed point
 * \return A list of seedpoints, mapped by their corresponding time step
 */
std::unordered_map<std::string, std::vector<glm::vec3>> extractSeedPointsFromFiles(
    std::filesystem::path path, size_t nth = 1);
/**
 * Traces field lines from the provided cdf file using kameleon and stores the data in the
 * provided FieldlinesState. Returns `false` if it fails to create a valid state. Requires
 * the kameleon module to be activated.
 *
 * \param state FieldlineState which should hold the extracted data
 * \param cdfPath `std::string` of the absolute path to a .cdf file
 * \param seedMap Vector of seed points from which to trace field lines
 * \param manualTimeOffset An offset that is applied to all fieldlines
 * \param tracingVar Which quantity to trace lines from. Typically "b" for magnetic field
 *        lines and "u" for velocity flow lines
 * \param extraVars Extra scalar quantities to be stored in the FieldlinesState; e.g. "T"
 *        for temperature, "rho" for density or "P" for pressure
 * \param extraMagVars Variables which should be used for extracting magnitudes, must be a
 *        multiple of 3; e.g. "ux", "uy" & "uz" to get the magnitude of the velocity
 *        vector at each line vertex
 */
bool convertCdfToFieldlinesState(FieldlinesState& state, const std::string& cdfPath,
    const std::unordered_map<std::string, std::vector<glm::vec3>>& seedMap,
    double manualTimeOffset, const std::string& tracingVar,
    std::vector<std::string>& extraVars, std::vector<std::string>& extraMagVars);

bool traceFromListOfPoints(FieldlinesState& state, const std::string& cdfPath,
    std::vector<glm::vec3>& seedpoints, const std::string& tracingVar,
    std::vector<std::string>& extraVars, std::vector<std::string>& extraMagVars);

} // namespace fls
} // namespace openspace

#endif // __OPENSPACE_MODULE_FIELDLINESSEQUENCE___KAMELEONFIELDLINEHELPER___H__
