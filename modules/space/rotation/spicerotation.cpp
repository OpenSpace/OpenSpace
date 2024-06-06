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

    constexpr openspace::properties::Property::PropertyInfo TimeFrameInfo = {
        "TimeFrame",
        "Time Frame",
        "The time frame in which the spice kernels are valid.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo FixedDateInfo = {
        "FixedDate",
        "Fixed Date",
        "A time to lock the rotation to. Setting this to an empty string will "
        "unlock the time and return to rotation based on current simulation time.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    struct [[codegen::Dictionary(SpiceRotation)]] Parameters {
        // [[codegen::verbatim(SourceInfo.description)]]
        std::string sourceFrame
            [[codegen::annotation("A valid SPICE NAIF name or integer")]];

        // This value specifies the destination frame that is used for the coordinate
        // transformation. This has to be a valid SPICE name. If this value is not
        // specified, a reference frame of 'GALACTIC' is used instead
        std::optional<std::string> destinationFrame;

        // [[codegen::verbatim(TimeFrameInfo.description)]]
        std::optional<ghoul::Dictionary> timeFrame
            [[codegen::reference("core_time_frame")]];

        // [[codegen::verbatim(FixedDateInfo.description)]]
        std::optional<std::string> fixedDate
            [[codegen::annotation("A time to lock the rotation to")]];
    };
#include "spicerotation_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation SpiceRotation::Documentation() {
    return codegen::doc<Parameters>("space_transform_rotation_spice");
}

SpiceRotation::SpiceRotation(const ghoul::Dictionary& dictionary)
    : _sourceFrame(SourceInfo)
    , _destinationFrame(DestinationInfo)
    , _fixedDate(FixedDateInfo)
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

    if (dictionary.hasKey(TimeFrameInfo.identifier)) {
        const ghoul::Dictionary timeFrameDictionary =
            dictionary.value<ghoul::Dictionary>(TimeFrameInfo.identifier);
        _timeFrame = TimeFrame::createFromDictionary(timeFrameDictionary);
        if (_timeFrame == nullptr) {
            throw ghoul::RuntimeError("Invalid dictionary for TimeFrame");
        }
        addPropertySubOwner(_timeFrame.get());
    }

    addProperty(_sourceFrame);
    addProperty(_destinationFrame);

    _sourceFrame.onChange([this]() { requireUpdate(); });
    _destinationFrame.onChange([this]() { requireUpdate(); });
}

glm::dmat3 SpiceRotation::matrix(const UpdateData& data) const {
    if (_timeFrame && !_timeFrame->isActive(data.time)) {
        return glm::dmat3(1.0);
    }
    double time = data.time.j2000Seconds();
    if (_fixedEphemerisTime.has_value()) {
        time = *_fixedEphemerisTime;
    }
    return SpiceManager::ref().positionTransformMatrix(
        _sourceFrame,
        _destinationFrame,
        time
    );
}

} // namespace openspace
