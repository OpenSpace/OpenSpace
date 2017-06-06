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

namespace openspace {
namespace globebrowsing {

namespace {
    const char* keyOpacity = "Opacity";
    const char* keyGamma = "Gamma";
    const char* keyMultiplier = "Multiplier";
    const char* keyOffset = "Offset";
    const char* keyValueBlending = "ValueBlending";
}

LayerRenderSettings::LayerRenderSettings()
    : properties::PropertyOwner("Settings")
    , opacity(properties::FloatProperty("Opacity", "opacity", 1.f, 0.f, 1.f))    
    , gamma(properties::FloatProperty("Gamma", "gamma", 1, 0, 5))
    , multiplier(properties::FloatProperty("Multiplier", "multiplier", 1.f, 0.f, 20.f))
    , offset(properties::FloatProperty("Offset", "offset", 0.f, -10000.f, 10000.f))
    , valueBlending(properties::FloatProperty("Value blending", "valueBlending",
                                               1.f, 0.f, 1.f))
    , useValueBlending(false)
{
    // Implicitly added properties (other ones are not for all layer types)
    addProperty(opacity);
    addProperty(gamma);
    addProperty(multiplier);
    addProperty(offset);
}

void LayerRenderSettings::setValuesFromDictionary(
    const ghoul::Dictionary& renderSettingsDict)
{
    float dictOpacity;
    float dictGamma;
    float dictMultiplier;
    float dictOffset;
    float dictValueBlending;
    
    if(renderSettingsDict.getValue(keyOpacity, dictOpacity)) {
        opacity.setValue(dictOpacity);
    }
    if(renderSettingsDict.getValue(keyGamma, dictGamma)) {
        gamma.setValue(dictGamma);
    }
    if(renderSettingsDict.getValue(keyMultiplier, dictMultiplier)) {
        multiplier.setValue(dictMultiplier);
    }
    if(renderSettingsDict.getValue(keyOffset, dictOffset)) {
        multiplier.setValue(dictOffset);
    }
    if(renderSettingsDict.getValue(keyValueBlending, dictValueBlending)) {
        valueBlending.setValue(dictValueBlending);
        useValueBlending = true;
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

} // namespace globebrowsing
} // namespace openspace
