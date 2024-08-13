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

#include <openspace/rendering/colormappingcomponent.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr std::string_view _loggerCat = "ColorMapping";

    constexpr openspace::properties::Property::PropertyInfo EnabledInfo = {
        "Enabled",
        "Color Map Enabled",
        "If this value is set to 'true', the provided color map is used (if one was "
        "provided in the configuration). If no color map was provided, changing this "
        "setting does not do anything.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo FileInfo = {
        "File",
        "Color Map File",
        "The path to the color map file to use for coloring the points.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ParameterInfo = {
        "Parameter",
        "Parameter",
        "This value determines which paramenter is used for coloring the points based "
        "on the color map. The property is set based on predefined options specified in "
        "the asset file. When changing the parameter, the value range to used for the"
        "mapping will also be changed. Per default, it is set to the last parameter in "
        "the list of options.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo RangeInfo = {
        "ValueRange",
        "Value Range",
        "This value changes the range of values to be mapped with the current color map.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo SetRangeFromDataInfo = {
        "SetRangeFromData",
        "Set Data Range from Data",
        "Set the data range for the color mapping based on the available data for the "
        "curently selected data column.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo HideOutsideInfo = {
        "HideValuesOutsideRange",
        "Hide Values Outside Range",
        "If true, points with values outside the provided range for the coloring will be "
        "hidden, i.e. not rendered at all.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo UseNoDataColorInfo = {
        "ShowMissingData",
        "Show Missing Data",
        "If true, use a separate color (see NoDataColor) for items with values "
        "corresponding to missing data values.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo NoDataColorInfo = {
        "NoDataColor",
        "No Data Color",
        "The color to use for items with values corresponding to missing data values, "
        "if enabled. This color can also be read from the color map, but setting this "
        "value overrides any value in the color map. If a color value for the below "
        "range values is provided, the ShowMissingData property will automatically be "
        "set to true.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo UseAboveRangeColorInfo = {
        "UseAboveRangeColor",
        "Use Above Range Color",
        "If true, use a separate color (see AboveRangeColor) for items with values "
        "larger than the one in the provided data range. Otherwise, the values will "
        "be clamped to use the color at the upper limit of the color map. If a color is "
        "provided in the color map, this value will automatically be set to true.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo AboveRangeColorInfo = {
        "AboveRangeColor",
        "Above Range Color",
        "The color to use for items with values larger than the one in the provided "
        "data range, if enabled. This color can also be read from the color map, but "
        "setting this value overrides any value in the color map. If a color value for "
        "the above range values is provided, the UseAboveRangeColor property will "
        "automatically be set to true.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo UseBelowRangeColorInfo = {
        "UseBelowRangeColor",
        "Use Below Range Color",
        "If true, use a separate color (see BelowRangeColor) for items with values "
        "smaller than the one in the provided data range. Otherwise, the values will "
        "be clamped to use the color at the lower limit of the color map. If a color is "
        "provided in the color map, this value will automatically be set to true.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo BelowRangeColorInfo = {
        "BelowRangeColor",
        "Below Range Color",
        "The color to use for items with values smaller than the one in the provided "
        "data range, if enabled. This color can also be read from the color map, but "
        "setting this value overrides any value in the color map. If a color value for "
        "the below range values is provided, the UseBelowRangeColor property will "
        "automatically be set to true.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo InvertColorMapInfo = {
        "Invert",
        "Invert Color Map",
        "If true, the colors of the color map will be read in the inverse order.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    struct [[codegen::Dictionary(ColorMappingComponent)]] Parameters {
        // [[codegen::verbatim(EnabledInfo.description)]]
        std::optional<bool> enabled;

        // [[codegen::verbatim(FileInfo.description)]]
        std::optional<std::filesystem::path> file;

        struct ColorMapParameter {
            // The key for the data variable to use for color
            std::string key;

            // An optional value range to use for coloring when this option is selected.
            // If not included, the range will be set from the min and max value in the
            // dataset
            std::optional<glm::vec2> range;
        };
        // A list of options for color parameters to use for color mapping, that will
        // appear as options in the drop-down menu in the user interface. Per default,
        // the first option in the list is selected. Each option is a table in the form
        // { Key = \"theKey\", Range = {min, max} }, where the value range is optional.
        // If added, this range will automatically be set when the option is selected
        std::optional<std::vector<ColorMapParameter>> parameterOptions;

        // [[codegen::verbatim(ParameterInfo.description)]]
        std::optional<std::string> parameter;

        // [[codegen::verbatim(RangeInfo.description)]]
        std::optional<glm::vec2> valueRange;

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

        // [[codegen::verbatim(InvertColorMapInfo.description)]]
        std::optional<bool> invert;
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
    , invert(InvertColorMapInfo, false)
    , dataColumn(ParameterInfo, properties::OptionProperty::DisplayType::Dropdown)
    , colorMapFile(FileInfo)
    , valueRange(RangeInfo, glm::vec2(0.f))
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

    colorMapFile.onChange([this]() {
        const bool fileExists = std::filesystem::exists(colorMapFile.value());
        if (!fileExists) {
            LERROR(std::format("Could not find cmap file: {}", colorMapFile.value()));
        }
        _colorMapFileIsDirty = true;
    });
    addProperty(colorMapFile);

    invert.onChange([this]() {
        // Invert the entries of the colormap
        std::reverse(_colorMap.entries.begin(), _colorMap.entries.end());
        _colorMapTextureIsDirty = true;
    });
    addProperty(invert);

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
        for (size_t i = 0; i < opts.size(); i++) {
            dataColumn.addOption(static_cast<int>(i), opts[i].key);
            // Add the provided range or an empty range. We will fill it later on,
            // when the dataset is loaded, if it is empty
            _colorRangeData.push_back(opts[i].range.value_or(glm::vec2(0.f)));
        }
    }

    dataColumn.onChange([this]() {
        valueRange = _colorRangeData[dataColumn.value()];
    });

    _providedParameter = p.parameter;
    _providedRange = p.valueRange;

    hideOutsideRange = p.hideValuesOutsideRange.value_or(hideOutsideRange);

    useNanColor = p.showMissingData.value_or(useNanColor);
    if (p.noDataColor.has_value()) {
        useNanColor = p.showMissingData.value_or(true);
        nanColor = *p.noDataColor;
        _hasNanColorInAsset = true;
    }

    useAboveRangeColor = p.useAboveRangeColor.value_or(useAboveRangeColor);
    if (p.aboveRangeColor.has_value()) {
        useAboveRangeColor = p.useAboveRangeColor.value_or(true);
        aboveRangeColor = *p.aboveRangeColor;
        _hasAboveRangeColorInAsset = true;
    }

    useBelowRangeColor = p.useBelowRangeColor.value_or(useBelowRangeColor);
    if (p.belowRangeColor.has_value()) {
        useBelowRangeColor = p.useBelowRangeColor.value_or(true);
        belowRangeColor = *p.belowRangeColor;
        _hasBelowRangeColorInAsset = true;
    }

    if (p.file.has_value()) {
        colorMapFile = p.file->string();
    }

    invert = p.invert.value_or(invert);
}

ghoul::opengl::Texture* ColorMappingComponent::texture() const {
    return _texture.get();
}

void ColorMappingComponent::initialize(const dataloader::Dataset& dataset,
                                       bool useCaching)
{
    ZoneScoped;

    if (useCaching) {
        _colorMap = dataloader::color::loadFileWithCache(colorMapFile.value());
    }
    else {
        _colorMap = dataloader::color::loadFile(colorMapFile.value());
    }

    initializeParameterData(dataset);

    if (_colorMap.nanColor.has_value() && !_hasNanColorInAsset) {
        nanColor = *_colorMap.nanColor;
        // @ TODO: Avoid overriding value set in asset? (also for useBelow and useAbove)
        useNanColor = true;
    }

    if (_colorMap.belowRangeColor.has_value() && !_hasBelowRangeColorInAsset) {
        belowRangeColor = *_colorMap.belowRangeColor;
        useBelowRangeColor = true;
    }

    if (_colorMap.aboveRangeColor.has_value() && !_hasAboveRangeColorInAsset) {
        aboveRangeColor = *_colorMap.aboveRangeColor;
        useAboveRangeColor = true;
    }
}

void ColorMappingComponent::initializeTexture() {
    if (_colorMap.entries.empty()) {
        return;
    }

    const unsigned int width = static_cast<unsigned int>(_colorMap.entries.size());
    const unsigned int height = 1;
    const unsigned int size = width * height;
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

void ColorMappingComponent::update(const dataloader::Dataset& dataset, bool useCaching) {
    if (_colorMapFileIsDirty) {
        initialize(dataset, useCaching);
        _colorMapTextureIsDirty = true;
        _colorMapFileIsDirty = false;
    }

    if (_colorMapTextureIsDirty) {
        initializeTexture();
        _colorMapTextureIsDirty = false;
    }
}

glm::vec4 ColorMappingComponent::colorFromColorMap(float valueToColorFrom) const {
    const glm::vec2 currentColorRange = valueRange;
    const float cmax = currentColorRange.y;
    const float cmin = currentColorRange.x;

    const float nColors = static_cast<float>(_colorMap.entries.size());

    const bool isOutsideMin = valueToColorFrom < cmin;
    const bool isOutsideMax = valueToColorFrom > cmax;

    if (hideOutsideRange && (isOutsideMin || isOutsideMax)) {
        return glm::vec4(0.f);
    }

    if (useNanColor && std::isnan(valueToColorFrom)) {
        return nanColor;
    }

    // Find color value using Nearest neighbor (same as texture)
    const float normalization = (cmax != cmin) ? (nColors) / (cmax - cmin) : 0;
    int colorIndex = static_cast<int>((valueToColorFrom - cmin) * normalization);

    // Clamp color index to valid range
    colorIndex = std::max(colorIndex, 0);
    colorIndex = std::min(colorIndex, static_cast<int>(nColors) - 1);

    return _colorMap.entries[colorIndex];
}

void ColorMappingComponent::initializeParameterData(const dataloader::Dataset& dataset) {
    if (dataset.isEmpty()) {
        return;
    }

    // Initialize empty ranges based on values in the dataset
    for (const properties::OptionProperty::Option& option : dataColumn.options()) {
        const int optionIndex = option.value;
        const int colorParameterIndex = dataset.index(option.description);

        glm::vec2& range = _colorRangeData[optionIndex];
        if (glm::length(range) < glm::epsilon<float>()) {
            range = dataset.findValueRange(colorParameterIndex);
        }
    }

    // Index to keep track of the potentially provided default option for the parameter
    int indexOfProvidedOption = -1;

    // If no options were added, add each dataset parameter and its range as options
    if (dataColumn.options().empty() && !dataset.entries.empty()) {
        int i = 0;
        _colorRangeData.reserve(dataset.variables.size());
        for (const dataloader::Dataset::Variable& v : dataset.variables) {
            dataColumn.addOption(i, v.name);
            _colorRangeData.push_back(dataset.findValueRange(v.index));

            // While iterating over options, try to find the one provided, if any
            if (_providedParameter.has_value() && *_providedParameter == v.name) {
                indexOfProvidedOption = i;
            }

            i++;
        }
    }
    else {
        // Otherwise, check if the selected columns exist
        for (size_t i = 0; i < dataColumn.options().size(); i++) {
            std::string o = dataColumn.options()[i].description;

            bool found = false;
            for (const dataloader::Dataset::Variable& v : dataset.variables) {
                if (v.name == o) {
                    found = true;
                    break;
                }
            }

            if (!found) {
                LWARNING(std::format(
                    "Dataset does not contain specified parameter '{}'", o
                ));
            }
            // While iterating over options, try to find the one provided, if any
            else if (_providedParameter.has_value() && *_providedParameter == o) {
                indexOfProvidedOption = static_cast<int>(i);
            }
        }
    }

    if (_providedParameter.has_value()) {
        if (indexOfProvidedOption == -1) {
            LERROR(std::format(
                "Error when reading Parameter. Could not find provided parameter '{}' in "
                "list of parameter options", *_providedParameter
            ));
        }
        else {
            dataColumn = indexOfProvidedOption;
        }
    }

    // Set the value range to correspond to the selected data column, or the one set by
    // the user
    if (_providedRange.has_value()) {
        valueRange = *_providedRange;
    }
    else {
        valueRange = _colorRangeData[dataColumn.value()];
    }
}

} // namespace openspace
