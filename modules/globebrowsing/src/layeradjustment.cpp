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

#include <modules/globebrowsing/src/layeradjustment.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <optional>

namespace {
    constexpr openspace::properties::Property::PropertyInfo ChromaKeyColorInfo = {
        "ChromaKeyColor",
        "Chroma Key Color",
        "This color is used as the chroma key for the layer that is adjusted.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ChromaKeyToleranceInfo = {
        "ChromaKeyTolerance",
        "Chroma Key Tolerance",
        "This value determines the tolerance that is used to determine whether a color "
        "is matching the selected Chroma key.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo TypeInfo = {
        "Type",
        "Type",
        "The type of layer adjustment that is applied to the underlying layer.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    struct [[codegen::Dictionary(LayerAdjustment)]] Parameters {
        enum class Type {
            None,
            ChromaKey,
            TransferFunction
        };
        // Specifies the type of the adjustment that is applied
        std::optional<Type> type;

        // Specifies the chroma key used when selecting 'ChromaKey' for the 'Type'
        std::optional<glm::vec3> chromaKeyColor [[codegen::color()]];

        // Specifies the tolerance to match the color to the chroma key when the
        // 'ChromaKey' type is selected for the 'Type'
        std::optional<float> chromaKeyTolerance;
    };
#include "layeradjustment_codegen.cpp"
} // namespace

namespace openspace::globebrowsing {

documentation::Documentation LayerAdjustment::Documentation() {
    return codegen::doc<Parameters>("globebrowsing_layeradjustment");
}

LayerAdjustment::LayerAdjustment()
    : properties::PropertyOwner({ "Adjustment" })
    , _chromaKeyColor(ChromaKeyColorInfo, glm::vec3(0.f), glm::vec3(0.f), glm::vec3(1.f))
    , _chromaKeyTolerance(ChromaKeyToleranceInfo, 0.f, 0.f, 1.f)
    , _typeOption(TypeInfo, properties::OptionProperty::DisplayType::Dropdown)
    , _typeId(static_cast<layers::Adjustment::ID>(_typeOption.value()))
{
    // Add options to option properties
    for (const layers::Adjustment& ai : layers::Adjustments) {
        _typeOption.addOption(static_cast<int>(ai.id), std::string(ai.identifier));
    }
    _typeOption.setValue(static_cast<int>(layers::Adjustment::ID::None));

    _typeOption.onChange([this]() {
        switch (type()) {
            case layers::Adjustment::ID::None:
                break;
            case layers::Adjustment::ID::ChromaKey:
                removeProperty(_chromaKeyColor);
                removeProperty(_chromaKeyTolerance);
                break;
            case layers::Adjustment::ID::TransferFunction:
                break;
        }
        _typeId = static_cast<layers::Adjustment::ID>(_typeOption.value());
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
    const Parameters p = codegen::bake<Parameters>(adjustmentDict);

    if (p.type.has_value()) {
        switch (*p.type) {
            case Parameters::Type::None:
                _typeOption = static_cast<int>(layers::Adjustment::ID::None);
                break;
            case Parameters::Type::ChromaKey:
                _typeOption = static_cast<int>(layers::Adjustment::ID::ChromaKey);
                break;
            case Parameters::Type::TransferFunction:
                _typeOption = static_cast<int>(layers::Adjustment::ID::TransferFunction);
                break;
        }
    }

    _chromaKeyColor = p.chromaKeyColor.value_or(_chromaKeyColor);
    _chromaKeyTolerance = p.chromaKeyTolerance.value_or(_chromaKeyTolerance);
}

layers::Adjustment::ID LayerAdjustment::type() const {
    return _typeId;
}

void LayerAdjustment::addVisibleProperties() {
    switch (type()) {
        case layers::Adjustment::ID::None:
            break;
        case layers::Adjustment::ID::ChromaKey:
            addProperty(_chromaKeyColor);
            addProperty(_chromaKeyTolerance);
            break;
        case layers::Adjustment::ID::TransferFunction:
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
