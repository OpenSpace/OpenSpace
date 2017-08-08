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

#include <modules/globebrowsing/rendering/layer/layerrendersettings.h>

namespace {
    const char* keyOpacity = "Opacity";
    const char* keyGamma = "Gamma";
    const char* keyMultiplier = "Multiplier";
    const char* keyOffset = "Offset";
    const char* keyValueBlending = "ValueBlending";

    static const openspace::properties::Property::PropertyInfo SetDefaultInfo = {
        "SetDefault",
        "Set Default",
        "If this value is triggered it will reset all of these values to their default "
        "values."
    };

    static const openspace::properties::Property::PropertyInfo OpacityInfo = {
        "Opacity",
        "Opacity",
        "This value sets the transparency of this layer. If this value is equal to '1', "
        "the layer is completely opaque. If this value is equal to '0', the layer is "
        "completely transparent."
    };

    static const openspace::properties::Property::PropertyInfo GammaInfo = {
        "Gamma",
        "Gamma",
        "This value is used as an exponent to adjust the color for each tile."
    };

    static const openspace::properties::Property::PropertyInfo MultiplierInfo = {
        "Multiplier",
        "Multiplier",
        "This value is used as a multiplier to adjust the color applied after taking the "
        "gamma value as an exponent."
    };

    static const openspace::properties::Property::PropertyInfo OffsetInfo = {
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
    float dictOpacity;
    float dictGamma;
    float dictMultiplier;
    float dictOffset;
    
    if (renderSettingsDict.getValue(keyOpacity, dictOpacity)) {
        opacity = dictOpacity;
    }
    if (renderSettingsDict.getValue(keyGamma, dictGamma)) {
        gamma = dictGamma;
    }
    if (renderSettingsDict.getValue(keyMultiplier, dictMultiplier)) {
        multiplier = dictMultiplier;
    }
    if (renderSettingsDict.getValue(keyOffset, dictOffset)) {
        multiplier = dictOffset;
    }
}

float LayerRenderSettings::performLayerSettings(float currentValue) const {
    float newValue = currentValue;

    newValue = glm::sign(newValue) * glm::pow(glm::abs(newValue), gamma);
    newValue = newValue * multiplier;
    newValue = newValue + offset;
    newValue = newValue * opacity;

    return newValue;
}

glm::vec4 LayerRenderSettings::performLayerSettings(glm::vec4 currentValue) const {
    glm::vec4 newValue = glm::vec4(
        performLayerSettings(currentValue.r),
        performLayerSettings(currentValue.g),
        performLayerSettings(currentValue.b),
        performLayerSettings(currentValue.a));

    return newValue;
}

} // namespace openspace::globebrowsing
