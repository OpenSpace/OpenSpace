/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#include <modules/space/timeframe/timeframekernel.h>

#include <openspace/documentation/documentation.h>
#include <openspace/logging/logmanager.h>
#include <openspace/format.h>
#include <openspace/misc/assert.h>
#include <openspace/misc/exception.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/time.h>
#include "SpiceUsr.h"
#include <algorithm>
#include <array>
#include <cstring>
#include <filesystem>
#include <optional>
#include <variant>

namespace {
    // This `TimeFrame` class determines its time ranges based on the set of loaded
    // SPICE kernels.  more information about Spice kernels, windows, or IDs, see the
    // required reading documentation from NAIF:
    //   - https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/req/kernel.html
    //   - https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/req/spk.html
    //   - https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/req/ck.html
    //   - https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/req/time.html
    //   - https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/req/windows.html
    //
    // The resulting validity of the time frame is based on the following conditions:
    //   1. If either `id` or `reference` (but not both) are specified, the time frame
    //      depends on the union of all windows within all kernels that were provided.
    //      This means that if the simulation time is within any time where the kernel has
    //      data for the provided object, the TimeFrame will be valid
    //   2. If `id` and `reference` are both specified, the time range validity for SPK
    //      and CK kernels are calculated separately, but both results must be valid to
    //      result in a valid time frame. This means that if only position data is
    //      available but not orientation data, the time frame is invalid. Only if
    //      positional and orientation data is available, then the TimeFrame will be valid
    //   3. If neither `id` nor `reference` are specified, the creation of the `TimeFrame`
    //      will fail
    struct [[codegen::Dictionary(TimeFrameKernel)]] Parameters {
        // Determines which NAIF object ID or name should be used to validate the
        // timeframe. If none is specified, the availability of positioning information is
        // not checked.
        std::optional<std::variant<std::string, int>> object;

        // Determines which NAIF object ID or name should be used to validate the
        // timeframe. If none is specified, the availability of orientation information is
        // not checked.
        std::optional<std::variant<std::string, int>> reference;
    };
} // namespace
#include "timeframekernel_codegen.cpp"

namespace openspace {

Documentation TimeFrameKernel::Documentation() {
    return codegen::doc<Parameters>(
        "space_timeframe_kernel",
        TimeFrame::Documentation()
    );
}

TimeFrameKernel::TimeFrameKernel(const Dictionary& dictionary)
    : _initialization(dictionary)
{
    // Baking the dictionary here to detect any error
    codegen::bake<Parameters>(dictionary);
}

void TimeFrameKernel::initialize() {
    const Parameters p = codegen::bake<Parameters>(_initialization);

    // Either the SPK or the CK variable must be specified
    if (!p.object.has_value() && !p.reference.has_value()) {
        throw RuntimeError(
            "Either the 'id' or the 'reference' (or both) values must be specified for "
            "the TimeFrameKernel. Neither was specified."
        );
    }

    if (p.object.has_value()) {
        if (std::holds_alternative<std::string>(*p.object)) {
            _object = SpiceManager::ref().naifId(std::get<std::string>(*p.object));
        }
        else {
            _object = std::get<int>(*p.object);
        }
    }

    if (p.reference.has_value()) {
        if (std::holds_alternative<std::string>(*p.reference)) {
            _reference = SpiceManager::ref().frameId(std::get<std::string>(*p.reference));
        }
        else {
            _reference = std::get<int>(*p.reference);
        }
    }

    _initialization = Dictionary();
}

void TimeFrameKernel::update(const Time& time) {
    assert_msg(
        _object.has_value() || _reference.has_value(),
        "Missing object or reference"
    );

    bool hasSpkCoverage = true;
    bool hasCkCoverage = true;
    const double et = time.j2000Seconds();

    if (_object.has_value()) {
        hasSpkCoverage = SpiceManager::ref().hasSpkCoverage(*_object, et);
    }

    if (_reference.has_value()) {
        hasCkCoverage = SpiceManager::ref().hasCkCoverage(*_reference, et);
    }

    _isInTimeFrame = hasSpkCoverage && hasCkCoverage;
}

} // namespace
