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

#include <modules/globebrowsing/rendering/layer/layeradjustment.h>

namespace {
    const char* keyType = "Type";
    const char* keyChromaKeyColor = "ChromaKeyColor";
    const char* keyChromaKeyTolerance = "ChromaKeyTolerance";

    static const openspace::properties::Property::PropertyInfo ChromaKeyColorInfo = {
        "ChromaKeyColor",
        "Chroma Key Color",
        "This color is used as the chroma key for the layer that is adjusted."
    };

    static const openspace::properties::Property::PropertyInfo ChromaKeyToleranceInfo = {
        "ChromaKeyTolerance",
        "Chroma Key Tolerance",
        "This value determines the tolerance that is used to determine whether a color "
        "is matching the selected Chroma key."
    };

    static const openspace::properties::Property::PropertyInfo TypeInfo = {
        "Type",
        "Type",
        "The type of layer adjustment that is applied to the underlying layer."
    };
} // namespace

namespace openspace::globebrowsing {

LayerAdjustment::LayerAdjustment()
    : properties::PropertyOwner("adjustment")
    , chromaKeyColor(ChromaKeyColorInfo, glm::vec3(0.f), glm::vec3(0.f), glm::vec3(1.f))
    , chromaKeyTolerance(ChromaKeyToleranceInfo, 0.f, 0.f, 1.f)
    , _typeOption(TypeInfo, properties::OptionProperty::DisplayType::Dropdown)
{
    // Add options to option properties
    for (int i = 0; i < layergroupid::NUM_ADJUSTMENT_TYPES; ++i) {
        _typeOption.addOption(i, layergroupid::ADJUSTMENT_TYPE_NAMES[i]);
    }
    _typeOption.setValue(static_cast<int>(layergroupid::AdjustmentTypeID::None));
    _type = static_cast<layergroupid::AdjustmentTypeID>(_typeOption.value());

    _typeOption.onChange([&](){
        removeVisibleProperties();
        _type = static_cast<layergroupid::AdjustmentTypeID>(_typeOption.value());
        addVisibleProperties();
        if (_onChangeCallback) {
            _onChangeCallback();
        }
    });
    chromaKeyColor.setViewOption(properties::Property::ViewOptions::Color);

    addProperty(_typeOption);
    addVisibleProperties();
}

void LayerAdjustment::setValuesFromDictionary(
    const ghoul::Dictionary& adjustmentDict)
{
    std::string dictType;
    glm::vec3 dictChromaKeyColor;
    float dictChromaKeyTolerance;
    
    if (adjustmentDict.getValue(keyType, dictType)) {
        _typeOption.setValue(
            static_cast<int>(layergroupid::getAdjustmentTypeIDFromName(dictType)));
    }
    if (adjustmentDict.getValue(keyChromaKeyColor, dictChromaKeyColor)) {
        chromaKeyColor.setValue(dictChromaKeyColor);
    }
    if (adjustmentDict.getValue(keyChromaKeyTolerance, dictChromaKeyTolerance)) {
        chromaKeyTolerance.setValue(dictChromaKeyTolerance);
    }
}

layergroupid::AdjustmentTypeID LayerAdjustment::type() const {
    return _type;
}

void LayerAdjustment::addVisibleProperties() {
    switch (type()) {
        case layergroupid::AdjustmentTypeID::None:
            break;
        case layergroupid::AdjustmentTypeID::ChromaKey: {
            addProperty(chromaKeyColor);
            addProperty(chromaKeyTolerance);
            break;
        }
        case layergroupid::AdjustmentTypeID::TransferFunction:
            break;
    }
}

void LayerAdjustment::removeVisibleProperties() {
    switch (type()) {
        case layergroupid::AdjustmentTypeID::None:
            break;
        case layergroupid::AdjustmentTypeID::ChromaKey: {
            removeProperty(chromaKeyColor);
            removeProperty(chromaKeyTolerance);
            break;
        }
        case layergroupid::AdjustmentTypeID::TransferFunction:
            break;
    }
}

void LayerAdjustment::onChange(std::function<void(void)> callback) {
    _onChangeCallback = std::move(callback);
}

} // namespace openspace::globebrowsing
