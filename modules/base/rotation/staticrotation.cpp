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

#include <modules/base/rotation/staticrotation.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>

namespace {
    constexpr openspace::properties::Property::PropertyInfo RotationInfo = {
        "Rotation",
        "Rotation",
        "This value is the used as a 3x3 rotation matrix that is applied to the scene "
        "graph node that this transformation is attached to relative to its parent.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    // Conversion from rotation matrix to euler angles, given that the rotation is a pure
    // rotation matrix.
    // Inspired by: https://www.learnopencv.com/rotation-matrix-to-euler-angles/
    glm::dvec3 rotationMatrixToEulerAngles(glm::dmat4 mat) {
        const double sy = glm::sqrt(mat[0][0] * mat[0][0] + mat[0][1] * mat[0][1]);
        const bool singular = (sy < 1e-6);

        glm::dvec3 res;
        if (singular) {
            res.x = glm::atan(-mat[2][1], mat[1][1]);
            res.y = glm::atan(-mat[0][2], sy);
            res.z = 0;
        }
        else {
            res.x = glm::atan(mat[1][2], mat[2][2]);
            res.y = glm::atan(-mat[0][2], sy);
            res.z = glm::atan(mat[0][1], mat[0][0]);
        }
        return res;
    }

    // A StaticRotation is using a fixed and constant rotation factor that does not change
    // over time. The rotation value is provided as a property that can be changed at
    // runtime, but it will not change automatically over time.
    struct [[codegen::Dictionary(StaticRotation)]] Parameters {
        // Stores the static rotation as a vector containing Euler angles, a quaternion
        // representation, or a rotation matrix.
        //
        // For the Euler angles, the values have to be provided in radians. To convert
        // degres to radians, you can use the `math.rad` function.
        //
        // For the Quaternion representation, the values have to be provided in the order
        // (w, x, y, z).
        //
        // For the matrix form, the provided matrix will be converted into Euler angles,
        // an operation which might fail if the matrix is not a true rotation matrix. The
        // values are assumed to be in row-major order.
        std::variant<glm::dvec3, glm::dvec4, glm::dmat3x3> rotation;
    };
#include "staticrotation_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation StaticRotation::Documentation() {
    return codegen::doc<Parameters>("base_transform_rotation_static");
}

StaticRotation::StaticRotation()
    : _eulerRotation(
        RotationInfo,
        glm::vec3(0.f),
        glm::vec3(-glm::pi<float>()),
        glm::vec3(glm::pi<float>())
    )
{
    addProperty(_eulerRotation);
    _eulerRotation.onChange([this]() {
        _matrixIsDirty = true;
        requireUpdate();
    });
    _type = "StaticRotation";
}

StaticRotation::StaticRotation(const ghoul::Dictionary& dictionary) : StaticRotation() {
    const Parameters p = codegen::bake<Parameters>(dictionary);

    if (std::holds_alternative<glm::dvec3>(p.rotation)) {
        _eulerRotation = std::get<glm::dvec3>(p.rotation);
    }
    else if (std::holds_alternative<glm::dvec4>(p.rotation)) {
        const glm::dvec4 data = std::get<glm::dvec4>(p.rotation);
        _eulerRotation = rotationMatrixToEulerAngles(
            glm::mat3_cast(glm::dquat(data.w, data.x, data.y, data.z))
        );
    }
    else if (std::holds_alternative<glm::dmat3>(p.rotation)) {
        _eulerRotation = rotationMatrixToEulerAngles(std::get<glm::dmat3>(p.rotation));
    }
    _matrixIsDirty = true;
    _type = "StaticRotation";
}

glm::dmat3 StaticRotation::matrix(const UpdateData&) const {
    if (_matrixIsDirty) {
        _cachedMatrix = glm::mat3_cast(glm::quat(_eulerRotation.value()));
        _matrixIsDirty = false;
    }
    return _cachedMatrix;
}

} // namespace openspace
