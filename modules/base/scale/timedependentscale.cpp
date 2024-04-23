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

#include <modules/base/scale/timedependentscale.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/util/time.h>
#include <openspace/util/updatestructures.h>
#include <optional>

namespace {
    constexpr openspace::properties::Property::PropertyInfo ReferenceDateInfo = {
        "ReferenceDate",
        "Reference Date",
        "The date at which this scale will be 0. The current value of the scale is "
        "computed by taking the difference between the current time and the reference "
        "date and multiplying it by the speed. This field must be formatted as: "
        "YYYY-MM-DDThh:mm:ss.uuu  where h is a 24h clock and u microseconds.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo SpeedInfo = {
        "Speed",
        "Speed",
        "The speed at which the value grows or shrinks. The units for this are meters "
        "per second. The default value is 1 m/s.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo ClampToPositiveInfo = {
        "ClampToPositive",
        "Clamp to Positive",
        "If this value is true, the velocity computation will be clamped to a positive "
        "value if the current simulation time is before the `ReferenceDate`. This is "
        "useful for instantaneous events that only propagate forwards in time. The "
        "default value is 'true'.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    // This Scale type provides the ability to scale an object dynamically as time in the
    // simulation passes. The provided `ReferenceDate`, specifies when the total scale
    // should be equal to 0 and the scales grows by `Speed` meters for every second in the
    // simulation. If `ClampToPositive` is specified as `true`, then the resulting scale
    // will always be positive or 0 if the simulation time is before the `ReferenceDate`.
    //
    // A common use-case for this Scale type would be to represent the Radiosphere, which
    // grows at the speed of light.
    struct [[codegen::Dictionary(TimeDependentScale)]] Parameters {
        // [[codegen::verbatim(ReferenceDateInfo.description)]]
        std::string referenceDate [[codegen::datetime()]];

        // [[codegen::verbatim(SpeedInfo.description)]]
        std::optional<double> speed [[codegen::greaterequal(0.0)]];

        // [[codegen::verbatim(ClampToPositiveInfo.description)]]
        std::optional<bool> clampToPositive;
    };
#include "timedependentscale_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation TimeDependentScale::Documentation() {
    return codegen::doc<Parameters>("base_scale_timedependent");
}

TimeDependentScale::TimeDependentScale(const ghoul::Dictionary& dictionary)
    : _referenceDate(ReferenceDateInfo, "")
    , _speed(SpeedInfo, 1.0, 0.0, 1e12)
    , _clampToPositive(ClampToPositiveInfo, true)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _referenceDate = p.referenceDate;
    _referenceDate.onChange([this]() { _cachedReferenceDirty = true; });
    addProperty(_referenceDate);

    _speed = p.speed.value_or(_speed);
    addProperty(_speed);

    _clampToPositive = p.clampToPositive.value_or(_clampToPositive);
    addProperty(_clampToPositive);
}

glm::dvec3 TimeDependentScale::scaleValue(const UpdateData& data) const {
    if (_cachedReferenceDirty) {
        _cachedReference = Time::convertTime(_referenceDate);
        _cachedReferenceDirty = false;
    }

    const double now = data.time.j2000Seconds();
    const double dt = now - _cachedReference;

    if (_clampToPositive) {
        return glm::dvec3(std::max(0.0, dt) * _speed);
    }
    else {
        return glm::dvec3(dt * _speed);
    }
}

} // namespace openspace
