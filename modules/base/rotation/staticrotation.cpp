/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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
                    new DoubleMatrix3Verifier()
                }),
                Optional::No,
                "Stores the static rotation as either a vector containing Euler angles "
                "or by specifiying the 3x3 rotation matrix directly"
            }
        }
    };
}

StaticRotation::StaticRotation()
    : _rotationMatrix(RotationInfo, glm::dmat3(1.0), glm::dmat3(-1.0), glm::dmat3(1.0))
{
    addProperty(_rotationMatrix);
    _rotationMatrix.onChange([this]() { requireUpdate(); });
}

StaticRotation::StaticRotation(const ghoul::Dictionary& dictionary) : StaticRotation() {
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "StaticRotation"
    );


    if (dictionary.hasKeyAndValue<glm::dvec3>(RotationInfo.identifier)) {
        _rotationMatrix = glm::mat3_cast(
            glm::dquat(dictionary.value<glm::dvec3>(RotationInfo.identifier))
        );
    }
    else {
        // Must be glm::dmat3 due to specification restriction
        _rotationMatrix = dictionary.value<glm::dmat3>(RotationInfo.identifier);
    }
}

glm::dmat3 StaticRotation::matrix(const UpdateData&) const {
    return _rotationMatrix;
}

} // namespace openspace
