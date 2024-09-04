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

#include <modules/base/scale/nonuniformstaticscale.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>

namespace {
    constexpr openspace::properties::Property::PropertyInfo ScaleInfo = {
        "Scale",
        "Scale",
        "These values are used as scaling factors for the scene graph node that this "
        "transformation is attached to relative to its parent.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    // This Scale type scales the scene graph node that it is attached to by a fixed
    // amount that does not change over time. It is possible to change the fixed scale
    // after starting the application, but it otherwise remains unchanged. The scaling is
    // a simple multiplication so that a `Scale` value of 10 means that the object will be
    // 10 times larger than its original size. In comparison to the StaticScale type, this
    // type has the ability to scale an object by different amounts for each direction.
    //
    // This type can be used to adjust the aspect ratio of Renderable types, for example
    // to make a RenderableSphericalGrid that is not a perfect spherical grid, but a
    // tri-axial ellipsoid instead.
    struct [[codegen::Dictionary(NonUniformStaticScale)]] Parameters {
        // [[codegen::verbatim(ScaleInfo.description)]]
        glm::dvec3 scale;
    };
#include "nonuniformstaticscale_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation NonUniformStaticScale::Documentation() {
    return codegen::doc<Parameters>("base_scale_nonuniformstatic");
}

NonUniformStaticScale::NonUniformStaticScale(const ghoul::Dictionary& dictionary)
    : _scaleValue(ScaleInfo, glm::dvec3(1.0), glm::dvec3(0.1), glm::dvec3(100.0))
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _scaleValue = p.scale;
    _scaleValue.onChange([this]() { requireUpdate(); });
    addProperty(_scaleValue);
}

glm::dvec3 NonUniformStaticScale::scaleValue(const UpdateData&) const {
    return _scaleValue;
}

} // namespace openspace
