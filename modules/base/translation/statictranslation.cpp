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

#include <modules/base/translation/statictranslation.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>

namespace {
    const char* KeyPosition = "Position";
} // namespace

namespace openspace {

documentation::Documentation StaticTranslation::Documentation() {
    using namespace documentation;
    return {
        "Static Translation",
        "base_transform_translation_static",
        {
            {
                "Type",
                new StringEqualVerifier("StaticTranslation"),
                "",
                Optional::No
            },
            {
                KeyPosition,
                new DoubleVector3Verifier,
                "Specifies the position (in meters) that this scenegraph node is located "
                "at relative to its parent",
                Optional::No
            }
        },
        Exhaustive::Yes
    };
}


StaticTranslation::StaticTranslation()
    : _position(
        "position",
        "Position",
        glm::dvec3(0.0),
        glm::dvec3(-std::numeric_limits<double>::max()),
        glm::dvec3(std::numeric_limits<double>::max())
    )
{
    addProperty(_position);
}

StaticTranslation::StaticTranslation(const ghoul::Dictionary& dictionary)
    : StaticTranslation()
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "StaticEphemeris"
    );

    _position = dictionary.value<glm::dvec3>(KeyPosition);
}

glm::dvec3 StaticTranslation::position() const {
    return _position;
}

void StaticTranslation::update(const UpdateData&) {}

} // namespace openspace
