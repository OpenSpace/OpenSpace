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

#include <modules/space/translation/tletranslation.h>

#include <modules/space/kepler.h>
#include <openspace/documentation/verifier.h>
#include <filesystem>
#include <optional>

namespace {
    struct [[codegen::Dictionary(TLETranslation)]] Parameters {
        // Specifies the filename of the Two-Line-Element file
        std::filesystem::path file;

        // Specifies the line number within the file where the group of 3 TLE lines begins
        // (1-based). Defaults to 1
        std::optional<int> lineNumber [[codegen::greater(0)]];
    };
#include "tletranslation_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation TLETranslation::Documentation() {
    return codegen::doc<Parameters>("space_transform_tle");
}

TLETranslation::TLETranslation(const ghoul::Dictionary& dictionary) {
    const Parameters p = codegen::bake<Parameters>(dictionary);
    if (!std::filesystem::is_regular_file(p.file)) {
        throw ghoul::RuntimeError("The provided TLE file must exist");
    }

    int lineNum = p.lineNumber.value_or(1);

    std::vector<SatelliteKeplerParameters> parameters = readTleFile(p.file);
    if (parameters.size() < lineNum) {
        throw ghoul::RuntimeError(fmt::format(
            "Requested line {} but only {} are available", lineNum, parameters.size()
        ));
    }

    SatelliteKeplerParameters param = parameters[lineNum - 1];
    setKeplerElements(
        param.eccentricity,
        param.semiMajorAxis,
        param.inclination,
        param.ascendingNode,
        param.argumentOfPeriapsis,
        param.meanAnomaly,
        param.period,
        param.epoch
    );
}

} // namespace openspace
