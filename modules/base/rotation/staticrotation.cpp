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

#include <modules/base/rotation/staticrotation.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>

namespace {
    constexpr openspace::properties::Property::PropertyInfo RotationInfo = {
        "Rotation",
        "Rotation",
        "This value is the used as a 3x3 rotation matrix that is applied to the scene "
        "graph node that this transformation is attached to relative to its parent."
    };

    // Conversion from rotation matrix to euler angles, given that the rotation is a pure
    // rotation matrix.
    // Inspired by: https://www.learnopencv.com/rotation-matrix-to-euler-angles/
    glm::dvec3 rotationMatrixToEulerAngles(glm::dmat4 mat) {
        const double sy = glm::sqrt(mat[0][0] * mat[0][0] + mat[0][1] * mat[0][1]);
        bool singular = sy < 1e-6;

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
} // namespace

namespace openspace {

documentation::Documentation StaticRotation::Documentation() {
    using namespace openspace::documentation;
    return {
        "Static Rotation",
        "base_transform_rotation_static",
        {
            {
                "Type",
                new StringEqualVerifier("StaticRotation"),
                Optional::No
            },
            {
                RotationInfo.identifier,
                new OrVerifier({
                    new DoubleVector3Verifier(),
                    new DoubleVector4Verifier(),
                    new DoubleMatrix3Verifier()
                }),
                Optional::No,
                "Stores the static rotation as a vector containing Euler angles, "
                " a quaternion or a rotation matrix."
            }
        }
    };
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
}

StaticRotation::StaticRotation(const ghoul::Dictionary& dictionary) : StaticRotation() {
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "StaticRotation"
    );

    if (dictionary.hasKeyAndValue<glm::dvec3>(RotationInfo.identifier)) {
        _eulerRotation = static_cast<glm::vec3>(
            dictionary.value<glm::dvec3>(RotationInfo.identifier)
        );
        _matrixIsDirty = true;
    }
    else if (dictionary.hasKeyAndValue<glm::dvec4>(RotationInfo.identifier)) {
        glm::dvec4 data = dictionary.value<glm::dvec4>(RotationInfo.identifier);
        _eulerRotation = rotationMatrixToEulerAngles(
            glm::mat3_cast(glm::dquat(data.w, data.x, data.y, data.z))
        );
        _matrixIsDirty = true;
    }
    else if (dictionary.hasKeyAndValue<glm::dmat3>(RotationInfo.identifier)) {
        _eulerRotation = rotationMatrixToEulerAngles(
            dictionary.value<glm::dmat3>(RotationInfo.identifier)
        );
        _matrixIsDirty = true;
    }
}

glm::dmat3 StaticRotation::matrix(const UpdateData&) const {
    if (_matrixIsDirty) {
        _cachedMatrix = glm::mat3_cast(glm::quat(_eulerRotation.value()));
        _matrixIsDirty = false;
    }
    return _cachedMatrix;
}

} // namespace openspace
