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

#include <modules/space/rotation/spicerotation.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/time.h>
#include <openspace/util/updatestructures.h>
#include <optional>

namespace {
    constexpr openspace::properties::Property::PropertyInfo SourceInfo = {
        "SourceFrame",
        "Source",
        "This value specifies the source frame that is used as the basis for the "
        "coordinate transformation. This has to be a valid SPICE name.",
        openspace::properties::Property::Visibility::Developer
    };

    constexpr openspace::properties::Property::PropertyInfo DestinationInfo = {
        "DestinationFrame",
        "Destination",
        "This value specifies the destination frame that is used for the coordinate "
        "transformation. This has to be a valid SPICE name.",
        openspace::properties::Property::Visibility::Developer
    };

    constexpr openspace::properties::Property::PropertyInfo FixedDateInfo = {
        "FixedDate",
        "Fixed Date",
        "A time to lock the rotation to. Setting this to an empty string will "
        "unlock the time and return to rotation based on current simulation time.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo TimeOffsetInfo = {
        "TimeOffset",
        "Time Offset",
        "A time offset, in seconds, added to the simulation time (or Fixed Date if any), "
        "at which to compute the rotation.",
        openspace::properties::Property::Visibility::User
    };


    // This `Rotation` type uses [SPICE](https://naif.jpl.nasa.gov/naif/) kernels to
    // provide rotation information for the attached scene graph node. SPICE is a library
    // used by scientists and engineers to, among other tasks, plan space missions. If you
    // are unfamiliar with SPICE, their webpage has both extensive
    // [Tutorials](https://naif.jpl.nasa.gov/naif/tutorials.html) as well as
    // [Lessions](https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/Lessons/) that explain
    // the system deeper. This class provides access to the
    // [pxform_c](https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/pxform_c.html)
    // function of the Spice library.
    struct [[codegen::Dictionary(SpiceRotation)]] Parameters {
        // [[codegen::verbatim(SourceInfo.description)]]
        std::string sourceFrame
            [[codegen::annotation("A valid SPICE NAIF name or integer")]];

        // This value specifies the destination frame that is used for the coordinate
        // transformation. This has to be a valid SPICE name. If this value is not
        // specified, a reference frame of 'GALACTIC' is used instead
        std::optional<std::string> destinationFrame;

        // [[codegen::verbatim(FixedDateInfo.description)]]
        std::optional<std::string> fixedDate [[codegen::datetime()]];

        // [[codegen::verbatim(TimeOffsetInfo.description)]]
        std::optional<float> timeOffset;
    };
#include "spicerotation_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation SpiceRotation::Documentation() {
    return codegen::doc<Parameters>("space_transform_rotation_spice");
}

SpiceRotation::SpiceRotation(const ghoul::Dictionary& dictionary)
    : Rotation(dictionary)
    , _sourceFrame(SourceInfo)
    , _destinationFrame(DestinationInfo)
    , _fixedDate(FixedDateInfo)
    , _timeOffset(TimeOffsetInfo)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _sourceFrame = p.sourceFrame;
    _destinationFrame = p.destinationFrame.value_or("GALACTIC");

    _fixedDate.onChange([this]() {
        if (_fixedDate.value().empty()) {
            _fixedEphemerisTime = std::nullopt;
        }
        else {
            _fixedEphemerisTime = SpiceManager::ref().ephemerisTimeFromDate(_fixedDate);
        }
    });
    _fixedDate = p.fixedDate.value_or(_fixedDate);
    addProperty(_fixedDate);

    _timeOffset = p.timeOffset.value_or(_timeOffset);
    addProperty(_timeOffset);

    addProperty(_sourceFrame);
    addProperty(_destinationFrame);

    _sourceFrame.onChange([this]() { requireUpdate(); });
    _destinationFrame.onChange([this]() { requireUpdate(); });
}

glm::dmat3 SpiceRotation::matrix(const UpdateData& data) const {
    return SpiceManager::ref().positionTransformMatrix(
        _sourceFrame,
        _destinationFrame,
        _fixedEphemerisTime.value_or(data.time.j2000Seconds()) + _timeOffset
    );
}

} // namespace openspace
