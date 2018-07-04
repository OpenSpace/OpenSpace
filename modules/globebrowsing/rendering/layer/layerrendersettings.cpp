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

#include <modules/globebrowsing/rendering/layer/layerrendersettings.h>

namespace {
    constexpr const char* keyOpacity = "Opacity";
    constexpr const char* keyGamma = "Gamma";
    constexpr const char* keyMultiplier = "Multiplier";
    constexpr const char* keyOffset = "Offset";

    constexpr openspace::properties::Property::PropertyInfo SetDefaultInfo = {
        "SetDefault",
        "Set Default",
        "If this value is triggered it will reset all of these values to their default "
        "values."
    };

    constexpr openspace::properties::Property::PropertyInfo OpacityInfo = {
        "Opacity",
        "Opacity",
        "This value sets the transparency of this layer. If this value is equal to '1', "
        "the layer is completely opaque. If this value is equal to '0', the layer is "
        "completely transparent."
    };

    constexpr openspace::properties::Property::PropertyInfo GammaInfo = {
        "Gamma",
        "Gamma",
        "This value is used as an exponent to adjust the color for each tile."
    };

    constexpr openspace::properties::Property::PropertyInfo MultiplierInfo = {
        "Multiplier",
        "Multiplier",
        "This value is used as a multiplier to adjust the color applied after taking the "
        "gamma value as an exponent."
    };

    constexpr openspace::properties::Property::PropertyInfo OffsetInfo = {
        "Offset",
        "Offset",
        "This value is used as an additive modifier to adjust the color applied after "
        "the gamma exponent and the multiplier has been performed."
    };
} // namespace

namespace openspace::globebrowsing {

LayerRenderSettings::LayerRenderSettings()
    : properties::PropertyOwner({ "Settings" })
    , setDefault(SetDefaultInfo)
    , opacity(OpacityInfo, 1.f, 0.f, 1.f)
    , gamma(GammaInfo, 1.f, 0.f, 5.f)
    , multiplier(MultiplierInfo, 1.f, 0.f, 20.f)
    , offset(OffsetInfo, 0.f, -10000.f, 10000.f)
{
    addProperty(opacity);
    addProperty(gamma);
    addProperty(multiplier);
    addProperty(offset);
    addProperty(setDefault);

    setDefault.onChange([this](){
        opacity = 1.f;
        gamma = 1.f;
        multiplier = 1.f;
        offset = 0.f;
    });
}

void LayerRenderSettings::setValuesFromDictionary(
                                              const ghoul::Dictionary& renderSettingsDict)
{
    if (renderSettingsDict.hasKeyAndValue<float>(keyOpacity)) {
        opacity = renderSettingsDict.value<float>(keyOpacity);
    }

    if (renderSettingsDict.hasKeyAndValue<float>(keyGamma)) {
        gamma = renderSettingsDict.value<float>(keyGamma);
    }

    if (renderSettingsDict.hasKeyAndValue<float>(keyMultiplier)) {
        multiplier = renderSettingsDict.value<float>(keyMultiplier);
    }

    if (renderSettingsDict.hasKeyAndValue<float>(keyOffset)) {
        offset = renderSettingsDict.value<float>(keyOffset);
    }
}

float LayerRenderSettings::performLayerSettings(float value) const {
    return ((glm::sign(value) * glm::pow(glm::abs(value), gamma) * multiplier) +
            offset) *
            opacity;
}

glm::vec4 LayerRenderSettings::performLayerSettings(glm::vec4 currentValue) const {
    glm::vec4 newValue = glm::vec4(
        performLayerSettings(currentValue.r),
        performLayerSettings(currentValue.g),
        performLayerSettings(currentValue.b),
        performLayerSettings(currentValue.a)
    );

    return newValue;
}

} // namespace openspace::globebrowsing
