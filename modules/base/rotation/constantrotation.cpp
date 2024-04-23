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

#include <modules/base/rotation/constantrotation.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/util/updatestructures.h>
#include <glm/gtx/quaternion.hpp>
#include <optional>

namespace {
    constexpr openspace::properties::Property::PropertyInfo RotationInfo = {
        "RotationAxis",
        "Rotation Axis",
        "This value is the rotation axis around which the object will rotate.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo RotationRateInfo = {
        "RotationRate",
        "Rotation Rate",
        "This value determines the number of revolutions per in-game second.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    struct [[codegen::Dictionary(ConstantRotation)]] Parameters {
        // [[codegen::verbatim(RotationInfo.description)]]
        std::optional<glm::dvec3> rotationAxis;
        // [[codegen::verbatim(RotationRateInfo.description)]]
        std::optional<float> rotationRate;
    };
#include "constantrotation_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation ConstantRotation::Documentation() {
    return codegen::doc<Parameters>("base_transform_rotation_constant");
}

ConstantRotation::ConstantRotation(const ghoul::Dictionary& dictionary)
    : _rotationAxis(
        RotationInfo,
        glm::dvec3(0.0, 0.0, 1.0),
        glm::dvec3(-1.0),
        glm::dvec3(1.0)
    )
    , _rotationRate(RotationRateInfo, 1.f, -1000.f, 1000.f)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _rotationAxis = p.rotationAxis.value_or(_rotationAxis);
    addProperty(_rotationAxis);

    _rotationRate = p.rotationRate.value_or(_rotationRate);
    addProperty(_rotationRate);
}

glm::dmat3 ConstantRotation::matrix(const UpdateData& data) const {
    if (data.time.j2000Seconds() == data.previousFrameTime.j2000Seconds()) {
        return glm::dmat3();
    }

    const double rotPerSec = _rotationRate;
    const double secPerFrame = data.time.j2000Seconds() -
                               data.previousFrameTime.j2000Seconds();

    const double rotPerFrame = rotPerSec * secPerFrame;
    const double radPerFrame = rotPerFrame * glm::tau<double>();

    _accumulatedRotation += radPerFrame;
    // Renormalize the rotation to prevent potential overflow (which probably will never
    // happen, but whatever)
    if (_accumulatedRotation > glm::tau<double>()) {
        _accumulatedRotation -= glm::tau<double>();
    }
    if (_accumulatedRotation < -glm::tau<double>()) {
        _accumulatedRotation += glm::tau<double>();
    }

    const glm::dquat q = glm::angleAxis(_accumulatedRotation, _rotationAxis.value());
    return glm::toMat3(q);
}

} // namespace openspace
