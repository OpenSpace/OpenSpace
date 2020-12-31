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

#include <modules/globebrowsing/src/layeradjustment.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>

namespace {
    constexpr const char* KeyType = "Type";
    constexpr const char* KeyChromaKeyColor = "ChromaKeyColor";
    constexpr const char* KeyChromaKeyTolerance = "ChromaKeyTolerance";

    constexpr openspace::properties::Property::PropertyInfo ChromaKeyColorInfo = {
        "ChromaKeyColor",
        "Chroma Key Color",
        "This color is used as the chroma key for the layer that is adjusted."
    };

    constexpr openspace::properties::Property::PropertyInfo ChromaKeyToleranceInfo = {
        "ChromaKeyTolerance",
        "Chroma Key Tolerance",
        "This value determines the tolerance that is used to determine whether a color "
        "is matching the selected Chroma key."
    };

    constexpr openspace::properties::Property::PropertyInfo TypeInfo = {
        "Type",
        "Type",
        "The type of layer adjustment that is applied to the underlying layer."
    };
} // namespace

namespace openspace::globebrowsing {

documentation::Documentation LayerAdjustment::Documentation() {
    using namespace documentation;
    return {
        "LayerAdjustment",
        "globebrowsing_layeradjustment",
        {
            {
                KeyType,
                new StringInListVerifier({ "None", "ChromaKey", "TransferFunction" }),
                Optional::Yes,
                "Specifies the type of the adjustment that is applied"
            },
            {
                KeyChromaKeyColor,
                new DoubleVector3Verifier,
                Optional::Yes,
                "Specifies the chroma key used when selecting 'ChromaKey' for the 'Type'."
            },
            {
                KeyChromaKeyTolerance,
                new DoubleVerifier,
                Optional::Yes,
                "Specifies the tolerance to match the color to the chroma key when the "
                "'ChromaKey' type is selected for the 'Type'."
            }
        }
    };
}

LayerAdjustment::LayerAdjustment()
    : properties::PropertyOwner({ "adjustment" })
    , _chromaKeyColor(ChromaKeyColorInfo, glm::vec3(0.f), glm::vec3(0.f), glm::vec3(1.f))
    , _chromaKeyTolerance(ChromaKeyToleranceInfo, 0.f, 0.f, 1.f)
    , _typeOption(TypeInfo, properties::OptionProperty::DisplayType::Dropdown)
{
    // Add options to option properties
    for (int i = 0; i < layergroupid::NUM_ADJUSTMENT_TYPES; ++i) {
        _typeOption.addOption(i, layergroupid::ADJUSTMENT_TYPE_NAMES[i]);
    }
    _typeOption.setValue(static_cast<int>(layergroupid::AdjustmentTypeID::None));
    _type = static_cast<layergroupid::AdjustmentTypeID>(_typeOption.value());

    _typeOption.onChange([&]() {
        switch (type()) {
            case layergroupid::AdjustmentTypeID::None:
                break;
            case layergroupid::AdjustmentTypeID::ChromaKey: {
                removeProperty(_chromaKeyColor);
                removeProperty(_chromaKeyTolerance);
                break;
            }
            case layergroupid::AdjustmentTypeID::TransferFunction:
                break;
        }
        _type = static_cast<layergroupid::AdjustmentTypeID>(_typeOption.value());
        addVisibleProperties();
        if (_onChangeCallback) {
            _onChangeCallback();
        }
    });
    _chromaKeyColor.setViewOption(properties::Property::ViewOptions::Color);

    addProperty(_typeOption);
    addVisibleProperties();
}

void LayerAdjustment::setValuesFromDictionary(const ghoul::Dictionary& adjustmentDict) {
    documentation::testSpecificationAndThrow(
        Documentation(),
        adjustmentDict,
        "LayerAdjustment"
    );

    if (adjustmentDict.hasKey(KeyType) && adjustmentDict.hasValue<std::string>(KeyType)) {
        std::string dictType = adjustmentDict.value<std::string>(KeyType);
        _typeOption = static_cast<int>(
            ghoul::from_string<layergroupid::AdjustmentTypeID>(dictType)
        );
    }

    if (adjustmentDict.hasKey(KeyChromaKeyColor) &&
        adjustmentDict.hasValue<glm::dvec3>(KeyChromaKeyColor))
    {
        glm::vec3 dictChromaKeyColor =
            adjustmentDict.value<glm::dvec3>(KeyChromaKeyColor);
        _chromaKeyColor = std::move(dictChromaKeyColor);
    }

    if (adjustmentDict.hasKey(KeyChromaKeyTolerance) &&
        adjustmentDict.hasValue<double>(KeyChromaKeyTolerance))
    {
        float dictChromaKeyTolerance = static_cast<float>(
            adjustmentDict.value<double>(KeyChromaKeyTolerance)
        );
        _chromaKeyTolerance = dictChromaKeyTolerance;
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
            addProperty(_chromaKeyColor);
            addProperty(_chromaKeyTolerance);
            break;
        }
        case layergroupid::AdjustmentTypeID::TransferFunction:
            break;
    }
}

glm::vec3 LayerAdjustment::chromaKeyColor() const {
    return _chromaKeyColor;
}

float LayerAdjustment::chromaKeyTolerance() const {
    return _chromaKeyTolerance;
}

void LayerAdjustment::onChange(std::function<void(void)> callback) {
    _onChangeCallback = std::move(callback);
}

} // namespace openspace::globebrowsing
