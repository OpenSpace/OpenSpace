/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#include <modules/digitaluniverse/rendering/colormapcomponent.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr openspace::properties::Property::PropertyInfo EnabledInfo = {
        "Enabled",
        "Color Map Enabled",
        "If this value is set to 'true', the provided color map is used (if one was "
        "provided in the configuration). If no color map was provided, changing this "
        "setting does not do anything",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo FileInfo = {
        "File",
        "Color Map File",
        "The path to the color map file to use for coloring the points",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ColorParameterInfo = {
        "Parameter",
        "Parameter",
        "This value determines which paramenter is used for coloring the points based "
        "on the color map. The property is set based on predefined options specified in "
        "the asset file. When changing the parameter, the value range to used for the"
        "mapping will also be changed",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ColorRangeInfo = {
        "ValueRange",
        "Value Range",
        "This value changes the range of values to be mapped with the current color map",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo SetRangeFromDataInfo = {
        "SetRangeFromData",
        "Set Data Range from Data",
        "Set the data range for the color mapping based on the available data for the "
        "curently selected data column",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo HideOutliersInfo = {
        "Hide",
        "Hide Outliers",
        "If true, points with values outside the provided range for the coloring will be "
        "rendered as transparent, i.e. not shown. Otherwise, the values will be clamped "
        "to use the color at the max VS min limit of the color map, respectively.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo UseMinOutlierColorInfo = {
        "UseMinColor",
        "Use Outside Min Color",
        "If true, use a separate color for items with values smaller than the minimum "
        "value of the specified value range",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo MinOutlierColorInfo = {
        "OutsideMinColor",
        "Outside Min Color",
        "The color to use for items with values smaller than the minimum value of the "
        "specified value range, if enabled",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    struct [[codegen::Dictionary(ColorMapComponent)]] Parameters {
        // [[codegen::verbatim(EnabledInfo.description)]]
        std::optional<bool> enabled;

        // [[codegen::verbatim(FileInfo.description)]]
        std::optional<std::string> file;

        struct ColorMapParameter {
            // The key for the datavar to use for color
            std::string key;

            // An optional value range to use for coloring when this option is selected.
            // If not included, the range will be set from the min and max value in the
            // dataset
            std::optional<glm::vec2> range;
        };
        std::optional<std::vector<ColorMapParameter>> parameterOptions;

        struct Outliers {
            // [[codegen::verbatim(HideOutliersInfo.description)]]
            std::optional<bool> hide;

            // [[codegen::verbatim(UseMinOutlierColorInfo.description)]]
            std::optional<bool> useMinColor;

            // [[codegen::verbatim(MinOutlierColorInfo.description)]]
            std::optional<glm::vec4> outsideMinColor [[codegen::color()]];
        };
        // A table with details about how to handle values outside of the
        // specified value range
        std::optional<Outliers> outliers;
    };
#include "colormapcomponent_codegen.cpp"
}  // namespace

namespace openspace {

documentation::Documentation ColorMapComponent::Documentation() {
    return codegen::doc<Parameters>("digitaluniverse_colormapcomponent");
}

ColorMapComponent::OutlierSettings::OutlierSettings()
    : properties::PropertyOwner({ "Outliers", "Outliers", "" })
    , hide(HideOutliersInfo, false)
    , useMinColor(UseMinOutlierColorInfo, false)
    , outsideMinColor(
        MinOutlierColorInfo,
        glm::vec4(0.5f, 0.5f, 0.5f, 1.f),
        glm::vec4(0.f),
        glm::vec4(1.f)
    )
{
    // @TODO (2023-10-02, emmbr) Are all these settings really needed? My thought is to
    // somehow also include missing vlaues here
    addProperty(hide);
    addProperty(useMinColor);
    outsideMinColor.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(outsideMinColor);
}

ColorMapComponent::ColorMapComponent()
    : properties::PropertyOwner({ "ColorMap", "Color Map", "" })
    , enabled(EnabledInfo, true)
    , dataColumn(ColorParameterInfo, properties::OptionProperty::DisplayType::Dropdown)
    , colorMapFile(FileInfo)
    , valueRange(ColorRangeInfo, glm::vec2(0.f))
    , setRangeFromData(SetRangeFromDataInfo)
{
    addProperty(enabled);
    addProperty(dataColumn);

    addProperty(valueRange);
    addProperty(setRangeFromData);

    colorMapFile.setReadOnly(true); // Currently this can't be changed
    addProperty(colorMapFile);

    addPropertySubOwner(outliers);
}

ColorMapComponent::ColorMapComponent(const ghoul::Dictionary& dictionary)
    : ColorMapComponent()
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    enabled = p.enabled.value_or(enabled);

    if (p.parameterOptions.has_value()) {
        std::vector<Parameters::ColorMapParameter> opts = *p.parameterOptions;

        _colorRangeData.reserve(opts.size());
        for (size_t i = 0; i < opts.size(); ++i) {
            dataColumn.addOption(static_cast<int>(i), opts[i].key);

            // TODO: set default value to be the data range
            _colorRangeData.push_back(opts[i].range.value_or(glm::vec2(0.f)));
        }

        // Following DU behavior here. The last colormap variable
        // entry is the one selected by default.
        dataColumn.setValue(static_cast<int>(_colorRangeData.size() - 1));
        valueRange = _colorRangeData.back();
    }

    dataColumn.onChange([this]() {
        valueRange = _colorRangeData[dataColumn.value()];
    });

    // TODO: read valueRange from asset if specified

    if (p.outliers.has_value()) {
        Parameters::Outliers o = *p.outliers;
        outliers.hide = o.hide.value_or(outliers.hide);
        outliers.useMinColor = o.useMinColor.value_or(outliers.useMinColor);
        outliers.outsideMinColor = o.outsideMinColor.value_or(outliers.outsideMinColor);
    }

    if (p.file.has_value()) {
        colorMapFile = absPath(*p.file).string();
    }
}

void ColorMapComponent::initialize(const speck::Dataset& dataset) {
    _colorMap = speck::color::loadFileWithCache(colorMapFile.value());

    // Initialize empty colormap ranges based on dataset
    for (const properties::OptionProperty::Option& option : dataColumn.options()) {
        int optionIndex = option.value;
        int colorParameterIndex = dataset.index(option.description);

        glm::vec2& range = _colorRangeData[optionIndex];
        if (glm::length(range) < glm::epsilon<float>()) {
            range = dataset.findValueRange(colorParameterIndex);
        }
    }

    // Set the value range again, to make sure that it's updated
    if (!_colorRangeData.empty()) {
        valueRange = _colorRangeData.back();
    }
}

glm::vec4 ColorMapComponent::colorFromColorMap(float valueToColorFrom) const {
    glm::vec2 currentColorRange = valueRange;
    float cmax = currentColorRange.y;
    float cmin = currentColorRange.x;

    float nColors = static_cast<float>(_colorMap.entries.size());

    bool isOutsideMin = valueToColorFrom < cmin;
    bool isOutsideMax = valueToColorFrom > cmax;

    if (outliers.hide && (isOutsideMin || isOutsideMax)) {
        return glm::vec4(0.f);
    }

    if (outliers.useMinColor && isOutsideMin) {
        return outliers.outsideMinColor;
    }

    // Find color value using Nearest neighbor

    float normalization = (cmax != cmin) ? (nColors) / (cmax - cmin) : 0;
    int colorIndex = static_cast<int>((valueToColorFrom - cmin) * normalization);

    // Clamp color index to valid range
    colorIndex = std::max(colorIndex, 0);
    colorIndex = std::min(colorIndex, static_cast<int>(nColors) - 1);

    return _colorMap.entries[colorIndex];
}

} // namespace openspace
