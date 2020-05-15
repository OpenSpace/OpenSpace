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

#include <modules/base/scale/nonuniformstaticscale.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>

namespace {
    constexpr openspace::properties::Property::PropertyInfo ScaleInfo = {
        "Scale",
        "Scale",
        "These values are used as scaling factors for the scene graph node that this "
        "transformation is attached to relative to its parent."
    };
} // namespace

namespace openspace {

documentation::Documentation NonUniformStaticScale::Documentation() {
    using namespace openspace::documentation;
    return {
        "Static Scaling",
        "base_scale_static",
        {
            {
                ScaleInfo.identifier,
                new DoubleVector3Verifier,
                Optional::No,
                ScaleInfo.description
            }
        }
    };
}

glm::dvec3 NonUniformStaticScale::scaleValue(const UpdateData&) const {
    return _scaleValue;
}

NonUniformStaticScale::NonUniformStaticScale()
    : _scaleValue(ScaleInfo, glm::dvec3(1.0), glm::dvec3(0.1), glm::dvec3(100.0))
{
    addProperty(_scaleValue);

    _scaleValue.onChange([this]() {
        requireUpdate();
    });
}

NonUniformStaticScale::NonUniformStaticScale(const ghoul::Dictionary& dictionary)
    : NonUniformStaticScale()
{
    documentation::testSpecificationAndThrow(Documentation(), dictionary, "StaticScale");

    _scaleValue = dictionary.value<glm::dvec3>(ScaleInfo.identifier);
}

} // namespace openspace
