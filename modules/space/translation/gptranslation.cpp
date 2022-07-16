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

#include <modules/space/translation/gptranslation.h>

#include <modules/space/kepler.h>
#include <openspace/documentation/verifier.h>
#include <filesystem>
#include <optional>

namespace {
    struct [[codegen::Dictionary(GPTranslation)]] Parameters {
        // Specifies the filename of the general pertubation file
        std::filesystem::path file;

        enum class Format {
            TLE,
            OMM
        };
        Format format;

        // Specifies the element within the file that should be used in case the file
        // provides multiple general pertubation elements. Defaults to 1.
        std::optional<int> element [[codegen::greater(0)]];
    };
#include "gptranslation_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation GPTranslation::Documentation() {
    return codegen::doc<Parameters>("space_transform_gp");
}

GPTranslation::GPTranslation(const ghoul::Dictionary& dictionary) {
    const Parameters p = codegen::bake<Parameters>(dictionary);
    if (!std::filesystem::is_regular_file(p.file)) {
        throw ghoul::RuntimeError("The provided TLE file must exist");
    }

    int element = p.element.value_or(1);

    std::function<std::vector<SatelliteKeplerParameters>(std::filesystem::path)> func;
    switch (p.format) {
        case Parameters::Format::TLE:
            func = &readTleFile;
            break;
        case Parameters::Format::OMM:
            func = &readOmmFile;
            break;
        default:
            throw ghoul::MissingCaseException();
    }

    std::vector<SatelliteKeplerParameters> parameters = func(p.file);
    if (parameters.size() < element) {
        throw ghoul::RuntimeError(fmt::format(
            "Requested element {} but only {} are available", element, parameters.size()
        ));
    }

    SatelliteKeplerParameters param = parameters[element - 1];
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
