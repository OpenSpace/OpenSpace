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

#include <modules/base/translation/statictranslation.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>

namespace {
    constexpr openspace::properties::Property::PropertyInfo PositionInfo = {
        "Position",
        "Position",
        "This value is used as a static offset (in meters) that is applied to the scene "
        "graph node that this transformation is attached to relative to its parent."
    };
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
                Optional::No
            },
            {
                PositionInfo.identifier,
                new DoubleVector3Verifier,
                Optional::No,
                PositionInfo.description
            }
        }
    };
}


StaticTranslation::StaticTranslation()
    : _position(
        PositionInfo,
        glm::dvec3(0.0),
        glm::dvec3(-std::numeric_limits<double>::max()),
        glm::dvec3(std::numeric_limits<double>::max())
    )
{
    addProperty(_position);

    _position.onChange([this]() {
        requireUpdate();
        notifyObservers();
    });
}

StaticTranslation::StaticTranslation(const ghoul::Dictionary& dictionary)
    : StaticTranslation()
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "StaticTranslation"
    );

    _position = dictionary.value<glm::dvec3>(PositionInfo.identifier);
}

glm::dvec3 StaticTranslation::position(const UpdateData&) const {
    return _position;
}

} // namespace openspace
