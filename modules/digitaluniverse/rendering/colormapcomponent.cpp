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
        "HideOutliers",
        "Hide Outliers",
        "If true, points with values outside the provided range for the coloring will be "
        "rendered as transparent, i.e. not shown. Otherwise, the values will be clamped "
        "to use the color at the max VS min limit of the color map, respectively.",
        openspace::properties::Property::Visibility::User
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

        // [[codegen::verbatim(HideOutliersInfo.description)]]
        std::optional<bool> hideOutliers;
    };
#include "colormapcomponent_codegen.cpp"
}  // namespace

namespace openspace {

documentation::Documentation ColorMapComponent::Documentation() {
    return codegen::doc<Parameters>("digitaluniverse_colormapcomponent");
}

ColorMapComponent::ColorMapComponent()
    : properties::PropertyOwner({ "ColorMap", "Color Map", "" })
    , enabled(EnabledInfo, true)
    , dataColumn(ColorParameterInfo, properties::OptionProperty::DisplayType::Dropdown)
    , colorMapFile(FileInfo)
    , valueRange(ColorRangeInfo, glm::vec2(0.f))
    , setRangeFromData(SetRangeFromDataInfo)
    , hideOutliers(HideOutliersInfo, false)
{
    addProperty(enabled);
    addProperty(dataColumn);

    addProperty(valueRange);
    addProperty(setRangeFromData);

    addProperty(hideOutliers);

    colorMapFile.setReadOnly(true); // Currently this can't be changed
    addProperty(colorMapFile);
}

ColorMapComponent::ColorMapComponent(const ghoul::Dictionary& dictionary)
    : ColorMapComponent()
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    enabled = p.enabled.value_or(enabled);

    if (p.parameterOptions.has_value()) {
        std::vector<Parameters::ColorMapParameter> opts = *p.parameterOptions;

        colorRangeData.reserve(opts.size());
        for (size_t i = 0; i < opts.size(); ++i) {
            dataColumn.addOption(static_cast<int>(i), opts[i].key);

            // TODO: set default value to be the data range
            colorRangeData.push_back(opts[i].range.value_or(glm::vec2(0.f)));
        }

        // Following DU behavior here. The last colormap variable
        // entry is the one selected by default.
        dataColumn.setValue(static_cast<int>(colorRangeData.size() - 1));
        valueRange = colorRangeData.back();
    }

    dataColumn.onChange([this]() {
        valueRange = colorRangeData[dataColumn.value()];
    });

    // TODO: read valueRange from asset if specified

    hideOutliers = p.hideOutliers.value_or(hideOutliers);

    if (p.file.has_value()) {
        colorMapFile = absPath(*p.file).string();
    }
}

//glm::vec4 ColorMapComponent::colorFromColorMap(float valueToColorFrom) const {
//    glm::vec2 currentColorRange = valueRange;
//    float cmax = currentColorRange.y;
//    float cmin = currentColorRange.x;
//
//    float nColors = static_cast<float>(_colorMap.entries.size());
//
//    if (hideOutliers) {
//        bool isOutsideRange = valueToColorFrom < cmin || valueToColorFrom > cmax;
//
//        if (isOutsideRange) {
//            return glm::vec4(0.f);
//        }
//    }
//
//    // Nearest neighbor
//
//    float normalization = (cmax != cmin) ? (nColors) / (cmax - cmin) : 0;
//    int colorIndex = static_cast<int>((valueToColorFrom - cmin) * normalization);
//
//    // Clamp color index to valid range
//    colorIndex = std::max(colorIndex, 0);
//    colorIndex = std::min(colorIndex, static_cast<int>(nColors) - 1);
//
//    return _colorMap.entries[colorIndex];
//}

} // namespace openspace
