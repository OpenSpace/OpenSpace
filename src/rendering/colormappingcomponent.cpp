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

#include <openspace/rendering/colormappingcomponent.h>

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
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo ColorRangeInfo = {
        "ValueRange",
        "Value Range",
        "This value changes the range of values to be mapped with the current color map",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo SetRangeFromDataInfo = {
        "SetRangeFromData",
        "Set Data Range from Data",
        "Set the data range for the color mapping based on the available data for the "
        "curently selected data column",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo HideOutsideInfo = {
        "HideValuesOutsideRange",
        "Hide Values Outside Range",
        "If true, points with values outside the provided range for the coloring will be "
        "rendered as transparent, i.e. not shown",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo UseNoDataColorInfo = {
        "ShowMissingData",
        "Show Missing Data",
        "If true, use a separate color (see NoDataColor) for items with values "
        "corresponding to missing data values",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo NoDataColorInfo = {
        "NoDataColor",
        "No Data Color",
        "The color to use for items with values corresponding to missing data values, "
        "if enabled. This color can also be read from the color map, but setting this "
        "value overrides any value in the color map. If a color value for the below "
        "range values is provided, the ShowMissingData property will automatically be "
        "set to true",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo UseAboveRangeColorInfo = {
        "UseAboveRangeColor",
        "Use Above Range Color",
        "If true, use a separate color (see AboveRangeColor) for items with values "
        "larger than the one in the provided data range. Otherwise, the values will "
        "be clamped to use the color at the upper limit of the color map. If a color is "
        "provided in the color map, this value will automatically be set to true",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo AboveRangeColorInfo = {
        "AboveRangeColor",
        "Above Range Color",
        "The color to use for items with values larger than the one in the provided "
        "data range, if enabled. This color can also be read from the color map, but "
        "setting this value overrides any value in the color map. If a color value for "
        "the above range values is provided, the UseAboveRangeColor property will "
        "automatically be set to true",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo UseBelowRangeColorInfo = {
        "UseBelowRangeColor",
        "Use Below Range Color",
        "If true, use a separate color (see BelowRangeColor) for items with values "
        "smaller than the one in the provided data range. Otherwise, the values will "
        "be clamped to use the color at the lower limit of the color map. If a color is "
        "provided in the color map, this value will automatically be set to true",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo BelowRangeColorInfo = {
        "BelowRangeColor",
        "Below Range Color",
        "The color to use for items with values smaller than the one in the provided "
        "data range, if enabled. This color can also be read from the color map, but "
        "setting this value overrides any value in the color map. If a color value for "
        "the below range values is provided, the UseBelowRangeColor property will "
        "automatically be set to true",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    struct [[codegen::Dictionary(ColorMappingComponent)]] Parameters {
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

        // [[codegen::verbatim(HideOutsideInfo.description)]]
        std::optional<bool> hideValuesOutsideRange;

        // [[codegen::verbatim(UseNoDataColorInfo.description)]]
        std::optional<bool> showMissingData;

        // [[codegen::verbatim(NoDataColorInfo.description)]]
        std::optional<glm::vec4> noDataColor [[codegen::color()]];

        // [[codegen::verbatim(UseAboveRangeColorInfo.description)]]
        std::optional<bool> useAboveRangeColor;

        // [[codegen::verbatim(AboveRangeColorInfo.description)]]
        std::optional<glm::vec4> aboveRangeColor [[codegen::color()]];

        // [[codegen::verbatim(UseBelowRangeColorInfo.description)]]
        std::optional<bool> useBelowRangeColor;

        // [[codegen::verbatim(BelowRangeColorInfo.description)]]
        std::optional<glm::vec4> belowRangeColor [[codegen::color()]];
    };
#include "colormappingcomponent_codegen.cpp"
}  // namespace

namespace openspace {

documentation::Documentation ColorMappingComponent::Documentation() {
    return codegen::doc<Parameters>("colormappingcomponent");
}

ColorMappingComponent::ColorMappingComponent()
    : properties::PropertyOwner({ "ColorMapping", "Color Mapping", "" })
    , enabled(EnabledInfo, true)
    , dataColumn(ColorParameterInfo, properties::OptionProperty::DisplayType::Dropdown)
    , colorMapFile(FileInfo)
    , valueRange(ColorRangeInfo, glm::vec2(0.f))
    , setRangeFromData(SetRangeFromDataInfo)
    , hideOutsideRange(HideOutsideInfo, false)
    , useNanColor(UseNoDataColorInfo, false)
    , nanColor(
        NoDataColorInfo,
        glm::vec4(0.5f, 0.5f, 0.5f, 1.f),
        glm::vec4(0.f),
        glm::vec4(1.f)
    )
    , useAboveRangeColor(UseAboveRangeColorInfo, false)
    , aboveRangeColor(AboveRangeColorInfo, glm::vec4(1.f), glm::vec4(0.f), glm::vec4(1.f))
    , useBelowRangeColor(UseBelowRangeColorInfo, false)
    , belowRangeColor(BelowRangeColorInfo, glm::vec4(1.f), glm::vec4(0.f), glm::vec4(1.f))
{
    addProperty(enabled);
    addProperty(dataColumn);

    addProperty(valueRange);
    addProperty(setRangeFromData);

    colorMapFile.setReadOnly(true); // Currently this can't be changed
    addProperty(colorMapFile);

    addProperty(hideOutsideRange);
    addProperty(useNanColor);
    nanColor.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(nanColor);

    addProperty(useAboveRangeColor);
    aboveRangeColor.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(aboveRangeColor);

    addProperty(useBelowRangeColor);
    belowRangeColor.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(belowRangeColor);
}

ColorMappingComponent::ColorMappingComponent(const ghoul::Dictionary& dictionary)
    : ColorMappingComponent()
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    enabled = p.enabled.value_or(enabled);

    if (p.parameterOptions.has_value()) {
        std::vector<Parameters::ColorMapParameter> opts = *p.parameterOptions;

        _colorRangeData.reserve(opts.size());
        for (size_t i = 0; i < opts.size(); ++i) {
            dataColumn.addOption(static_cast<int>(i), opts[i].key);
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

    // @TODO: read valueRange from asset if specified. How to avoid overriding it
    // in initialize?

    hideOutsideRange = p.hideValuesOutsideRange.value_or(hideOutsideRange);

    useNanColor = p.showMissingData.value_or(useNanColor);
    if (p.noDataColor.has_value()) {
        useNanColor = p.showMissingData.value_or(true);
        nanColor = *p.noDataColor;
        _nanColorInAsset = true;
    }

    useAboveRangeColor = p.useAboveRangeColor.value_or(useAboveRangeColor);
    if (p.aboveRangeColor.has_value()) {
        useAboveRangeColor = p.useAboveRangeColor.value_or(true);
        aboveRangeColor = *p.aboveRangeColor;
        _aboveRangeColorInAsset = true;
    }

    useBelowRangeColor = p.useBelowRangeColor.value_or(useBelowRangeColor);
    if (p.belowRangeColor.has_value()) {
        useBelowRangeColor = p.useBelowRangeColor.value_or(true);
        belowRangeColor = *p.belowRangeColor;
        _belowRangeColorInAsset = true;
    }

    if (p.file.has_value()) {
        colorMapFile = absPath(*p.file).string();
    }
}

ghoul::opengl::Texture* ColorMappingComponent::texture() const {
    return _texture.get();
}

void ColorMappingComponent::initialize(const dataloader::Dataset& dataset) {
    _colorMap = dataloader::color::loadFileWithCache(colorMapFile.value());

    // Initialize empty colormap ranges based on dataset
    for (const properties::OptionProperty::Option& option : dataColumn.options()) {
        int optionIndex = option.value;
        int colorParameterIndex = dataset.index(option.description);

        glm::vec2& range = _colorRangeData[optionIndex];
        if (glm::length(range) < glm::epsilon<float>()) {
            range = dataset.findValueRange(colorParameterIndex);
        }
    }

    // If no options were added, add each dataset parameter and its range as options
    if (dataColumn.options().empty() && !dataset.entries.empty()) {
        int i = 0;
        _colorRangeData.reserve(dataset.variables.size());
        for (const dataloader::Dataset::Variable& v : dataset.variables) {
            dataColumn.addOption(i, v.name);
            _colorRangeData.push_back(dataset.findValueRange(v.index));
            i++;
        }

        if (_colorRangeData.size() > 1) {
            dataColumn = static_cast<int>(_colorRangeData.size() - 1);
        }
    }
    else {
        // Otherwise, check if the selected columns exist
        for (int i = 0; i < dataColumn.options().size(); ++i) {
            std::string o = dataColumn.options()[i].description;
            const std::vector<dataloader::Dataset::Variable>& vars = dataset.variables;

            bool found = false;
            for (const dataloader::Dataset::Variable& v : dataset.variables) {
                if (v.name == o) {
                    found = true;
                    break;
                }
            }

            if (!found) {
                LWARNINGC("ColorMapping", fmt::format(
                    "Dataset does not contain specified parameter '{}'", o
                ));
            }
        }
    }

    if (_colorMap.nanColor.has_value() && !_nanColorInAsset) {
        nanColor = *_colorMap.nanColor;
        useNanColor = true; // @ TODO: Avoid overriding value set in asset? (also for useBelow and useAbove)
    }

    if (_colorMap.belowRangeColor.has_value() && !_belowRangeColorInAsset) {
        belowRangeColor = *_colorMap.belowRangeColor;
        useBelowRangeColor = true;
    }

    if (_colorMap.aboveRangeColor.has_value() && !_aboveRangeColorInAsset) {
        aboveRangeColor = *_colorMap.aboveRangeColor;
        useAboveRangeColor = true;
    }

    // Set the value range and selected option again, to make sure that it's updated
    if (!_colorRangeData.empty()) {
        valueRange = _colorRangeData.back();
    }
}

void ColorMappingComponent::initializeTexture() {
    if (_colorMap.entries.empty()) {
        return;
    }

    unsigned int width = static_cast<unsigned int>(_colorMap.entries.size());
    unsigned int height = 1;
    unsigned int size = width * height;
    std::vector<GLubyte> img;
    img.reserve(size * 4);

    for (const glm::vec4& c : _colorMap.entries) {
        img.push_back(static_cast<GLubyte>(255 * c.r));
        img.push_back(static_cast<GLubyte>(255 * c.g));
        img.push_back(static_cast<GLubyte>(255 * c.b));
        img.push_back(static_cast<GLubyte>(255 * c.a));
    }

    _texture = std::make_unique<ghoul::opengl::Texture>(
        glm::uvec3(width, height, 1),
        GL_TEXTURE_1D,
        ghoul::opengl::Texture::Format::RGBA

    );

    // TODO: update this for linear mapping?
    _texture->setFilter(ghoul::opengl::Texture::FilterMode::Nearest);
    _texture->setWrapping(ghoul::opengl::Texture::WrappingMode::ClampToEdge);
    _texture->setPixelData(
        reinterpret_cast<char*>(img.data()),
        ghoul::opengl::Texture::TakeOwnership::No
    );

    _texture->uploadTexture();
}

glm::vec4 ColorMappingComponent::colorFromColorMap(float valueToColorFrom) const {
    glm::vec2 currentColorRange = valueRange;
    float cmax = currentColorRange.y;
    float cmin = currentColorRange.x;

    float nColors = static_cast<float>(_colorMap.entries.size());

    bool isOutsideMin = valueToColorFrom < cmin;
    bool isOutsideMax = valueToColorFrom > cmax;

    if (hideOutsideRange && (isOutsideMin || isOutsideMax)) {
        return glm::vec4(0.f);
    }

    if (useNanColor && std::isnan(valueToColorFrom)) {
        return nanColor;
    }

    // Find color value using Nearest neighbor (same as texture)
    float normalization = (cmax != cmin) ? (nColors) / (cmax - cmin) : 0;
    int colorIndex = static_cast<int>((valueToColorFrom - cmin) * normalization);

    // Clamp color index to valid range
    colorIndex = std::max(colorIndex, 0);
    colorIndex = std::min(colorIndex, static_cast<int>(nColors) - 1);

    return _colorMap.entries[colorIndex];
}

} // namespace openspace