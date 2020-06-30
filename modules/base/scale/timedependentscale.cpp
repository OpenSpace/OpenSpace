/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

namespace {
    constexpr openspace::properties::Property::PropertyInfo ReferenceDateInfo = {
        "ReferenceDate",
        "Reference Date",
        "The date at which this scale will be 0. The current value of the scale is "
        "computed by taking the difference between the current time and the reference "
        "date and multiplying it by the speed. This field must be formatted as: "
        "YYYY-MM-DDThh:mm:ss.uuu  where h is a 24h clock and u microseconds."
    };

    constexpr openspace::properties::Property::PropertyInfo SpeedInfo = {
        "Speed",
        "Speed",
        "The speed at which the value grows or shrinks. The units for this are meters "
        "per second. The default value is 1 m/s."
    };

    constexpr openspace::properties::Property::PropertyInfo ClampToPositiveInfo = {
        "ClampToPositive",
        "Clamp to Positive",
        "If this value is true, the velocity computation will never result in any "
        "negative values. This is useful for instantaneous events that only propagate "
        "forwards. The default value is 'true'."
    };
} // namespace

namespace openspace {

documentation::Documentation TimeDependentScale::Documentation() {
    using namespace openspace::documentation;
    return {
        "Timedependent Scaling",
        "base_scale_timedependent",
        {
            {
                ReferenceDateInfo.identifier,
                new StringVerifier,
                Optional::No,
                ReferenceDateInfo.description
            },
            {
                SpeedInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                SpeedInfo.description
            },
            {
                ClampToPositiveInfo.identifier,
                new BoolVerifier,
                Optional::Yes,
                ClampToPositiveInfo.description
            }
        }
    };
}

TimeDependentScale::TimeDependentScale(const ghoul::Dictionary& dictionary)
    : _referenceDate(ReferenceDateInfo, "")
    , _speed(SpeedInfo, 1.0, 0.0, 1e12)
    , _clampToPositive(ClampToPositiveInfo, true)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "TimeDependentScale"
    );

    _referenceDate = dictionary.value<std::string>(ReferenceDateInfo.identifier);
    _referenceDate.onChange([this]() { _cachedReferenceDirty = true; });
    addProperty(_referenceDate);

    if (dictionary.value<double>(SpeedInfo.identifier)) {
        _speed = dictionary.value<double>(SpeedInfo.identifier);
    }
    addProperty(_speed);

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
