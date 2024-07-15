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

#include <openspace/rendering/fadeable.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <optional>

namespace {
    constexpr openspace::properties::Property::PropertyInfo OpacityInfo = {
        "Opacity",
        "Opacity",
        "This value determines the opacity of this object. A value of 0 means "
        "completely transparent.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo FadeInfo = {
        "Fade",
        "Fade",
        "This value is used by the system to be able to fade out objects "
        "independently from the Opacity value selected by the user. This value should "
        "not be directly manipulated through a user interface, but instead used by other "
        "components of the system programmatically.",
        openspace::properties::Property::Visibility::Developer
    };
} // namespace

namespace openspace {

Fadeable::Fadeable()
    : _opacity(OpacityInfo, 1.f, 0.f, 1.f)
    , _fade(FadeInfo, 1.f, 0.f, 1.f)
{
    // Note that this is not a property owner. It's still up to the subclasses to
    // add the properties, assign values, etc.
}

void Fadeable::setFade(float fade) {
    _fade = fade;
}

void Fadeable::setParentFadeable(Fadeable* parent) {
    _parentFadeable = parent;
}

float Fadeable::fade() const {
    return _fade;
}

bool Fadeable::isVisible() const {
    return opacity() > 0.f;
}

float Fadeable::opacity() const noexcept {
    float fadeFromParent = 1.f;
    if (_parentFadeable) {
        // Note that we only care about the fade here, not the full opacity of the
        // parent. A subowner might still be visible even if the opacity of the
        // parent is set to zero
        fadeFromParent = _parentFadeable->fade();
    }
    return _opacity * _fade * fadeFromParent;
}

}  // namespace openspace
