/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#include <modules/base/ephemeris/staticephemeris.h>

#include <openspace/documentation/verifier.h>

namespace {
    const std::string KeyPosition = "Position";
}

namespace openspace {

Documentation StaticEphemeris::Documentation() {
    using namespace openspace::documentation;
    return {
        "Static Translation",
        "base_transform_translation_static",
        {
            {
                "Type",
                new StringEqualVerifier("StaticEphemeris"),
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


StaticEphemeris::StaticEphemeris()
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

StaticEphemeris::StaticEphemeris(const ghoul::Dictionary& dictionary)
    : StaticEphemeris()
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "StaticEphemeris"
    );

    _position = dictionary.value<glm::dvec3>(KeyPosition);
}

StaticEphemeris::~StaticEphemeris() {}

glm::dvec3 StaticEphemeris::position() const {
    return _position;
}

void StaticEphemeris::update(const UpdateData&) {}

} // namespace openspace
