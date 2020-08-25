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

#include <modules/base/scale/staticscale.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>

namespace {
    constexpr openspace::properties::Property::PropertyInfo ScaleInfo = {
        "Scale",
        "Scale",
        "This value is used as a scaling factor for the scene graph node that this "
        "transformation is attached to relative to its parent."
    };
} // namespace

namespace openspace {

documentation::Documentation StaticScale::Documentation() {
    using namespace openspace::documentation;
    return {
        "Static Scaling",
        "base_scale_static",
        {
            {
                ScaleInfo.identifier,
                new DoubleVerifier,
                Optional::No,
                ScaleInfo.description
            }
        }
    };
}

glm::dvec3 StaticScale::scaleValue(const UpdateData&) const {
    return glm::dvec3(_scaleValue);
}

StaticScale::StaticScale() : _scaleValue(ScaleInfo, 1.f, 0.1f, 100.f) {
    addProperty(_scaleValue);

    _scaleValue.onChange([this]() {
        requireUpdate();
    });
}

StaticScale::StaticScale(const ghoul::Dictionary& dictionary) : StaticScale() {
    documentation::testSpecificationAndThrow(Documentation(), dictionary, "StaticScale");

    _scaleValue = static_cast<float>(dictionary.value<double>(ScaleInfo.identifier));
}

} // namespace openspace
