/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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
    const char* KeyRotation = "Rotation";
}

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
                "",
                Optional::No
            },
            {
                KeyRotation,
                new OrVerifier(
                    new DoubleVector3Verifier(),
                    new DoubleMatrix3Verifier()
                ),
                "Stores the static rotation as either a vector containing Euler angles "
                "or by specifiying the 3x3 rotation matrix directly",
                Optional::No
            }
        },
        Exhaustive::Yes
    };
}

StaticRotation::StaticRotation()
    : _rotationMatrix("rotation", "Rotation", glm::dmat3(1.0))
{}

StaticRotation::StaticRotation(const ghoul::Dictionary& dictionary)
    : StaticRotation()
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "StaticRotation"
    );


    if (dictionary.hasKeyAndValue<glm::dvec3>(KeyRotation)) {
        _rotationMatrix = glm::mat3_cast(
            glm::dquat(dictionary.value<glm::dvec3>(KeyRotation))
        );
    }
    else {
        // Must be glm::dmat3 due to specification restriction
        _rotationMatrix = dictionary.value<glm::dmat3>(KeyRotation);
    }

    addProperty(_rotationMatrix);
    _rotationMatrix.onChange([this]() { _matrix = _rotationMatrix; });
}

} // namespace openspace
