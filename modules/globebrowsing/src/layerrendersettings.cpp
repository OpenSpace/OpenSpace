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

#include <modules/globebrowsing/src/layerrendersettings.h>

namespace {
    constexpr openspace::properties::Property::PropertyInfo SetDefaultInfo = {
        "SetDefault",
        "Set Default",
        "If this value is triggered it will reset all of these values to their default "
        "values",
        // @VISIBILITY(1.67)
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo GammaInfo = {
        "Gamma",
        "Gamma",
        "This value is used as an exponent to adjust the color for each tile",
        // @VISIBILITY(2.67)
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo MultiplierInfo = {
        "Multiplier",
        "Multiplier",
        "This value is used as a multiplier to adjust the color applied after taking the "
        "gamma value as an exponent",
        // @VISIBILITY(2.67)
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo OffsetInfo = {
        "Offset",
        "Offset",
        "This value is used as an additive modifier to adjust the color applied after "
        "the gamma exponent and the multiplier has been performed",
        openspace::properties::Property::Visibility::AdvancedUser
    };
} // namespace

namespace openspace::globebrowsing {

LayerRenderSettings::LayerRenderSettings()
    : properties::PropertyOwner({ "Settings" })
    , gamma(GammaInfo, 1.f, 0.f, 5.f)
    , multiplier(MultiplierInfo, 1.f, 0.f, 20.f)
    , offset(OffsetInfo, 0.f, -10000.f, 10000.f)
    , setDefault(SetDefaultInfo)
{
    addProperty(gamma);
    addProperty(multiplier);
    addProperty(offset);
    addProperty(setDefault);

    setDefault.onChange([this](){
        gamma = 1.f;
        multiplier = 1.f;
        offset = 0.f;
    });
}

void LayerRenderSettings::onChange(const std::function<void()>& callback) {
    gamma.onChange(callback);
    multiplier.onChange(callback);
    multiplier.onChange(callback);
    offset.onChange(callback);
}

float LayerRenderSettings::performLayerSettings(float value) const {
    return
        ((glm::sign(value) * glm::pow(glm::abs(value), gamma) * multiplier) + offset);
}

glm::vec4 LayerRenderSettings::performLayerSettings(const glm::vec4& currentValue) const {
    return glm::vec4(
        performLayerSettings(currentValue.r),
        performLayerSettings(currentValue.g),
        performLayerSettings(currentValue.b),
        performLayerSettings(currentValue.a)
    );
}

} // namespace openspace::globebrowsing
